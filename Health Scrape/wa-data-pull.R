rm(list=ls())
library(tidyverse)
library(jsonlite)
library(lubridate)

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


zctacounty <-  zcta::zcta_county_rel_10 %>%
    select(zcta = zcta5, geoid, poppt, zpoppct) %>%
    mutate(geoid = sprintf("%05d", geoid)) %>%
    group_by(zcta) %>%
    filter(zpoppct == max(zpoppct)) %>%
    ungroup() %>%
    right_join(zipzcta::zipzcta) %>%
    filter(state == "WA") %>%
    select(zip, geoid)

facilityDF <- read_json(
    "Health Scrape/facility.json", simplifyVector = T) %>%
    sapply(unlist) %>%
    t %>%
    as_tibble() %>%
    left_join(zctacounty, by = "zip") %>%
    group_by(geoid) %>%
    summarize(`Facility Count` = n()) %>%
    rename(FIPS = geoid)

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
    filter(Province_State == "Washington") %>%
    left_join(facilityDF, by = "FIPS")

write_csv(DF, "./Health Scrape/covid_county_facility.csv")