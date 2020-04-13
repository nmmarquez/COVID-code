rm(list=ls())
library(tidyverse)
library(leaflet)
library(lubridate)
library(tigris)
library(sf)
library(rmapshaper)
library(leafpop)
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
    arrange(Province_State, Admin2, Last_Update)


# Basic exponential curve fitting code
exp_func <- function(x, y){
    2^((1:length(y) + exp(x[1]))/exp(x[2]))
}

exp_days <- function(x, y){
    sqrt(sum((exp_func(x, y) - y)^2))
}

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
    mutate(dt_hat = exp(optim(c(0, 0), exp_days, y=Confirmed)$par[2])) %>%
    mutate(day_hat = exp(optim(c(0, 0), exp_days, y=Confirmed)$par[1])) %>%
    filter(Last_Update == max(Last_Update)) %>%
    mutate(Notes = paste0(
        "Location: ", Combined_Key,
        "<br>Estimated Double Time: ", round(dt_hat, 2),
        " days<br>Confirmed: ", Confirmed,
        "<br>Deaths: ", Deaths,
        "<br>Recovered: ", Recovered)) %>%
    filter(dt_hat<=30) %>%
    rename(GEOID =  FIPS) %>%
    mutate(
        alpha =case_when(
            Confirmed < 500 ~ .3,
            Confirmed >= 500 & Confirmed < 1500 ~ .6,
            TRUE ~ .9),
        fp = paste0("admin2-map/plots/", Combined_Key, ".png"))

### Make plots for popup
for(j in unique(anlyzDF$Combined_Key)){
    tmpDF <- DF %>%
        filter(Combined_Key == j) %>%
        select(Admin2, Province_State, Last_Update, Confirmed) %>%
        left_join(select(anlyzDF, -Last_Update, -Confirmed)) %>%
        filter(Confirmed > 50) %>%
        mutate(type = "Observed")

    estDF <- tibble(
        Last_Update = seq(
            min(tmpDF$Last_Update),
            (max(tmpDF$Last_Update)+days(14)), by = "day")) %>%
        mutate(date_estimate = 1:n() + round(tmpDF$day_hat[1])) %>%
        mutate(Confirmed = exp_func(
            log(c(tmpDF$day_hat[1], tmpDF$dt_hat[1])), date_estimate))

    annotations <- tibble(
        xpos = min(estDF$Last_Update),
        ypos = max(estDF$Confirmed),
        annotateText = tmpDF$Notes[1],
        hjustvar = c(0) ,
        vjustvar = c(1)) %>%
        mutate(annotateText = str_replace_all(annotateText, "<br>", "\n"))

    p1 <- ggplot(tmpDF, aes(x=Last_Update, y = Confirmed)) +
        geom_point() +
        theme_classic() +
        geom_line(data=estDF) +
        geom_label(
            data=annotations,
            aes(x=xpos,y=ypos,label=annotateText,
                vjust=vjustvar, hjust=hjustvar)) +
        labs(x="Date", "Cases") +
        theme(
            legend.text = element_text(size=13),
            legend.title = element_text(size=15),
            axis.text = element_text(size=13),
            axis.title = element_text(size=17),
            title =  element_text(size=20))

    ggsave(tmpDF$fp[1], p1)

}

bins <- c(0,3,5,7,10,15,31)
pal <- colorBin(
    palette = "Spectral", reverse=FALSE, 
    domain = c(0, max(anlyzDF$dt_hat)), bins = bins)

mapDF <- countyDF %>%
    left_join(anlyzDF, by = "GEOID") %>%
    filter(!is.na(dt_hat))

mapDF %>%
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
    addPopupImages(mapDF$fp, "n", width = 400, height = 400)
