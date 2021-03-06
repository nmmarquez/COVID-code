rm(list=ls())
library(MASS)
library(tidyverse)
library(leaflet)
library(lubridate)
library(tigris)
library(sf)
library(rmapshaper)
library(leafpop)
library(TMB)
library(ar.matrix)
options(tigris_use_cache=TRUE)

day_files <- list.files(
    "COVID-19/csse_covid_19_data/csse_covid_19_daily_reports",
    full.names = TRUE, pattern = "*.csv")

convert_all_dates <- function(Date){
    # dates are usually space seperated and one of these two formats
    funs <- c("mdy","ymd")
    
    # vector to store results
    dates <- as.POSIXct(rep(NA,length(Date)))
    
    # we try everything lubridate has. There will be some warnings
    # e.g. because mdy cannot translate everything. You can ignore this.
    for ( f in funs ){
        dates[is.na(dates)] <- do.call(f,list(Date[is.na(dates)]))  
    }
    round_date(dates, unit = "days")
}

# The JH repo only has admin2 data in the US after the 22nd so we just want
# those files which limits us to a two week window for now
DF <- day_files %>%
    str_split("/", simplify = TRUE) %>%
    .[,4] %>%
    str_replace(".csv", "") %>%
    mdy() %>%
    {which(. > mdy("03-21-2020"))} %>%
    {bind_rows(lapply(day_files[.], read_csv, col_types = "cccccddiiiic"))} %>%
    # all the dates are always formatted funk so lets fix this
    mutate(Last_Update = str_split(Last_Update, " ", simplify = TRUE)[,1]) %>%
    filter(Country_Region == "US") %>%
    mutate(Last_Update = convert_all_dates(Last_Update)) %>%
    arrange(Province_State, Admin2, Last_Update) %>%
    # apparently string length of GEOID is variable now
    mutate(FIPS = ifelse(str_length(FIPS) == 4, paste0("0", FIPS), FIPS))

# get county data from tigris
countyDF <- counties(class = "sf") %>%
    filter(as.numeric(STATEFP < 57) & STATEFP != "02" & STATEFP != "15") %>%
    as('Spatial') %>%
    ms_simplify() %>%
    st_as_sf() %>%
    st_transform("+proj=longlat +datum=WGS84")

# We want to limit ourselves to the locations that have over 100 cases
# and only use time periods of 50+ cases as well as 3 time periods of 50+
# cases
anlyzDF <- DF %>%
    group_by(Province_State, Admin2) %>%
    filter(any(Confirmed > 100) & Confirmed > 50 & (n() > 3)) %>%
    filter(!is.na(FIPS) & Admin2 != "Unassigned") %>%
    mutate(New_Cases = Confirmed - lag(Confirmed)) %>%
    mutate(Prev_Day_Cases = lag(Confirmed)) %>%
    mutate(New_Cases = ifelse(New_Cases < 0, NA, New_Cases)) %>%
    ungroup()

model_run <- function(DT, verbose = FALSE){
    model <- "rw_model"
    Data <- list(y=DT$Confirmed)
    Params <- list(phi=rep(0, length(Data$y)+14), log_sigma=c(0))
    dyn.load(dynlib(model))
    Obj <- MakeADFun(data=Data, parameters=Params, DLL=model, random="phi",
                     silent=!verbose)
    Obj$env$tracemgc <- verbose
    Obj$env$inner.control$trace <- verbose
    Opt <- nlminb(start=Obj$par, objective=Obj$fn, 
                                    gradient=Obj$gr,
                                    control=list(eval.max=1e4, iter.max=1e4))
    Report <- Obj$report()
    Report$convergence <- Opt$convergence
    sdrep <- sdreport(Obj, getJointPrecision = T)
    Report$par.vals <- sdrep$par.random
    ids <- which(row.names(sdrep$jointPrecision) == "phi")
    Report$prec <- sdrep$jointPrecision[ids,ids]
    dyn.unload(dynlib(model))
    Report
}

N <- 1000

for(j in unique(anlyzDF$Combined_Key)){
    subDF <- anlyzDF %>%
        filter(Combined_Key == j)
    
    subModel <- model_run(subDF)
    
    theta <- exp(t(ar.matrix::sim.AR(N, subModel$prec) + subModel$par.vals))
    chat <- matrix(NA, nrow = nrow(theta), ncol = N)
    
    for(i in 2:nrow(chat)){
        if((i-1) <= nrow(subDF) & !is.na(subDF$Prev_Day_Cases[i])){
            chat[i,] <- subDF$Prev_Day_Cases[i] +
                rpois(N, subDF$Prev_Day_Cases[i] * theta[i,])
        }
        else{
            chat[i,] <- chat[i-1,] +
                rpois(N, chat[i-1,] * exp(last(subModel$par.vals)))
        }
    }
    
    newDF <- tibble(Confirmed = NA, Last_Update = seq(
            min(subDF$Last_Update),
            (max(subDF$Last_Update)+days(14)), by = "day")) %>%
        mutate(Confirmed = apply(chat, 1, median, na.rm = TRUE)) %>%
        mutate(lwr = apply(chat, 1, quantile, probs = .025, na.rm = TRUE)) %>%
        mutate(upr = apply(chat, 1, quantile, probs = .975, na.rm = TRUE))
    
    theta_ <- exp(last(subModel$par.vals))
    
    annotations <- tibble(
        xpos = min(newDF$Last_Update),
        ypos = max(newDF$upr, na.rm=T),
        annotateText = paste0(
            "Location: ", last(subDF$Combined_Key),
            "<br>Estimated Double Time: ", round(log(2)/log(1+theta_), 2),
            " days<br>Confirmed: ", last(subDF$Confirmed),
            "<br>Deaths: ", last(subDF$Deaths),
            "<br>Recovered: ", last(subDF$Recovered)),
        hjustvar = c(0),
        vjustvar = c(1)) %>%
        mutate(annotateText = str_replace_all(annotateText, "<br>", "\n"))
    
    p1 <- ggplot(subDF, aes(x=Last_Update, y = Confirmed)) +
        geom_point() +
        theme_classic() +
        geom_line(data=newDF) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), data=newDF, alpha = .3) +
        geom_label(
            data=annotations,
            aes(x=xpos,y=ypos,label=annotateText,
                vjust=vjustvar, hjust=hjustvar)) +
        labs(x="Date", "Cases") +
        scale_y_log10() +
        theme(
            legend.text = element_text(size=13),
            legend.title = element_text(size=15),
            axis.text = element_text(size=13),
            axis.title = element_text(size=17),
            title =  element_text(size=20))
    
    ggsave(last(subDF$fp), p1)
}

anlyzDF <- DF %>%
    group_by(Province_State, Admin2) %>%
    filter(any(Confirmed > 100) & Confirmed > 50 & (n() > 3)) %>%
    filter(!is.na(FIPS) & Admin2 != "Unassigned") %>%
    mutate(New_Cases = Confirmed - lag(Confirmed)) %>%
    mutate(Prev_Day_Cases = lag(Confirmed)) %>%
    mutate(New_Cases = ifelse(New_Cases < 0, NA, New_Cases)) %>%
    mutate(model = list(glm.nb(New_Cases ~ 1 + offset(log(Prev_Day_Cases))))) %>%
    mutate(pct_inc = exp(summary(model[[1]])$coefficients[1,1])) %>%
    mutate(dt_st_err = summary(model[[1]])$coefficients[1,2]) %>%
    mutate(dt_hat = log(2)/log(1+pct_inc)) %>%
    mutate(theta = summary(model[[1]])$theta) %>%
    filter(Last_Update == max(Last_Update)) %>%
    mutate(Notes = paste0(
        "Location: ", Combined_Key,
        "<br>Estimated Double Time: ", round(dt_hat, 2),
        " days<br>Confirmed: ", Confirmed,
        "<br>Deaths: ", Deaths,
        "<br>Recovered: ", Recovered)) %>%
    filter(dt_hat<30) %>%
    rename(GEOID = FIPS) %>%
    mutate(
        alpha =case_when(
            Confirmed < 500 ~ .3,
            Confirmed >= 500 & Confirmed < 1500 ~ .6,
            TRUE ~ .9),
        fp = paste0("admin2-map/plots/", Combined_Key, ".png"))

sims <- 1000

### Make plots for popup
for(j in unique(anlyzDF$Combined_Key)){
    
    tmpDF <- DF %>%
        filter(Combined_Key == j) %>%
        select(Admin2, Province_State, Last_Update, Confirmed) %>%
        mutate(Prev_Day_Cases = lag(Confirmed)) %>%
        left_join(select(anlyzDF, -Last_Update, -Confirmed, Prev_Day_Cases)) %>%
        filter(Confirmed > 50) %>%
        mutate(type = "Observed")
    
    estDF <- tibble(
        Last_Update = seq(
            min(tmpDF$Last_Update),
            (max(tmpDF$Last_Update)+days(14)), by = "day")) %>%
        left_join(select(tmpDF, Last_Update, Prev_Day_Cases, Confirmed)) %>%
        mutate(Prev_Day_Cases = lag(Confirmed))
    
    lsim <- exp(rnorm(sims, log(last(tmpDF$pct_inc)), last(tmpDF$dt_st_err)))
    chat <- matrix(NA, nrow = nrow(estDF), ncol = sims)
    
    for(i in 2:nrow(estDF)){
        if(!is.na(estDF$Prev_Day_Cases[i])){
            chat[i,] <- estDF$Prev_Day_Cases[i] +
                rnegbin(lsim, estDF$Prev_Day_Cases[i] * lsim, last(tmpDF$theta))
        }
        else{
            chat[i,] <- chat[i-1,] +
                rnegbin(lsim, chat[i-1,] * lsim, last(tmpDF$theta))
        }
    }
    
    estDF$Confirmed <- apply(chat, 1, median)
    estDF$lwr <- apply(chat, 1, quantile, probs = .025, na.rm=T)
    estDF$upr <- apply(chat, 1, quantile, probs = .975, na.rm=T)
    
    annotations <- tibble(
        xpos = min(estDF$Last_Update),
        ypos = max(estDF$upr, na.rm=T),
        annotateText = last(tmpDF$Notes),
        hjustvar = c(0) ,
        vjustvar = c(1)) %>%
        mutate(annotateText = str_replace_all(annotateText, "<br>", "\n"))
    
    p1 <- ggplot(tmpDF, aes(x=Last_Update, y = Confirmed)) +
        geom_point() +
        theme_classic() +
        geom_line(data=estDF) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), data=estDF, alpha = .3) +
        geom_label(
            data=annotations,
            aes(x=xpos,y=ypos,label=annotateText,
                vjust=vjustvar, hjust=hjustvar)) +
        labs(x="Date", "Cases") +
        scale_y_log10() +
        theme(
            legend.text = element_text(size=13),
            legend.title = element_text(size=15),
            axis.text = element_text(size=13),
            axis.title = element_text(size=17),
            title =  element_text(size=20))
    
    ggsave(last(tmpDF$fp), p1)
    
}

bins <- c(0,3,5,7,10,15,30)
pal <- colorBin(
    palette = "Spectral", reverse=FALSE, 
    domain = c(0, max(anlyzDF$dt_hat)), bins = bins)

mapDF <- countyDF %>%
    left_join(anlyzDF, by = "GEOID") %>%
    filter(!is.na(dt_hat))

mapObj <- mapDF %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        group = "n",
        fillColor = ~pal(dt_hat),
        color = "#444444",
        #popup = ~popupImage(fp, width = 500, height = 500),
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.,
        fillOpacity = ~alpha,
        highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE),
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
    addLegend("bottomleft", pal = pal, values = ~dt_hat, 
              title = "Estimated<br>Doubling Time", opacity = 1) %>%
    addPopupImages(mapDF$fp, "n", width = 600, height = 500)

saveRDS(mapObj, "admin2-map/map.RDS")
