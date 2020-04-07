rm(list=ls())
library(tidyverse)
library(leaflet)
library(lubridate)
library(tigris)
library(sf)
library(rmapshaper)
options(tigris_use_cache=TRUE)

day_files <- list.files(
    "COVID-19/csse_covid_19_data/csse_covid_19_daily_reports",
    full.names = TRUE, pattern = "*.csv")

# The JH repo only has admin2 data in the US after the 22nd so we just want
# those files which limits us to a two week window for now
DF <- day_files %>%
    str_split("/", simplify = TRUE) %>%
    .[,4] %>%
    str_replace(".csv", "") %>%
    mdy() %>%
    {which(. > mdy("03-22-2020"))} %>%
    {bind_rows(lapply(day_files[.], read_csv))} %>%
    filter(Country_Region == "US") %>%
    mutate(Last_Update = floor_date(Last_Update, unit = "day")) %>%
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
    filter(Last_Update == max(Last_Update)) %>%
    mutate(Notes = paste0(
        "Location: ", Combined_Key,
        "<br>Estimated Double Time: ", round(dt_hat, 2),
        " days<br>Confirmed: ", Confirmed,
        "<br>Deaths: ", Deaths,
        "<br>Recovered: ", Recovered)) %>%
    filter(dt_hat<=20) %>%
    rename(GEOID =  FIPS)

pal <- colorNumeric(palette = "Spectral", reverse=FALSE, 
                    domain = c(0, max(anlyzDF$dt_hat)))

countyDF %>%
    left_join(anlyzDF, by = "GEOID") %>%
    filter(!is.na(dt_hat)) %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        fillColor = ~pal(dt_hat),
        color = "#444444",
        popup = ~Notes,
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE),
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
    addLegend("bottomleft", pal = pal, values = ~dt_hat, 
              title = "Estimated<br>Doubling Time", opacity = 1)
