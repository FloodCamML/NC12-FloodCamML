## CoPe COMET Physical Data
## FloodCamML Group
## Emory Wellman
## 5/20/21

# Packages to load
library(dplyr)
library(dbplyr)
library(lubridate)
library(RPostgres)
library(DBI)
library(pool)
library(foreach)
library(dbx)
library(readr)
library(httr)
library(later)
library(rvest)
library(xml2)
library(noaaoceans)


## Pull tide data: Use noaaoceans ############################

# Filter to stations in NC with water level sensor
nc_station <- station_df %>% 
  filter(station_state == 'NC' & water_level == '1')

## Hi/lo tide data from Oregon Inlet gauge

OregonInlet_Tides <- noaaoceans::query_coops_data(
  station_id = '8652587',
  start_date = format(Sys.Date(),"%Y%m%d"),
  end_date = format(Sys.Date()+1,"%Y%m%d"),
  data_product = 'predictions',
  interval = 'hilo',
  datum = 'MLLW'
)

## Try (and fail) to write a function which uses this query_coops_function to pull the hi/lo tide data for the day.

date <- Sys.Date() 
date <- gsub("-", "", date) #Getting date in the format needed by query_coops_data

#date <- as.character(date)
#date <- as.integer(date)
#newdate <- as.Date(date,format="%m/%d/%y")

## Trying to feed in the formatted System Date rather than a string of numbers as above.
## Not working - package really just wants to see yyyymmdd format, won't accept something representing it.

get_tides <- function(location) {
  if(location == "Oregon Inlet") {
    noaaoceans::query_coops_data(
      station_id = '8652587',
      start_date = format(Sys.Date(),"%Y%m%d"),
      end_date = format(Sys.Date(),"%Y%m%d"),
      data_product = 'predictions',
      units="english",
      time_zone = "lst_ldt",
      interval = 'hilo',
      datum = 'MLLW'
    )
  }
}


## Pull tide data: NOAA COOPS ###############

## https://api.tidesandcurrents.noaa.gov/api/prod/
## Based on the info for the API, it seemed possible to specify predictions, today, and hilo to get tide data.
## I can get the water_levels with hilo to work, but not  what we want.

get_tides <- function(location) {
  if(location == "Oregon Inlet Gauge"){
    request <-
      httr::GET(
        url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter/",
        query = list(
          "station" = "8652587",
          "product" = "water_level",
          "units" = "english",
          "datum"="NAVD",
          "time_zone" = "lst",
          "format" = "json",
          "date"="today",
          "interval"="hilo",
          "application" = "UNC_Institute_for_the_Environment, https://github.com/acgold"
        )
      )
    
    wl <-
      tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$data)
    colnames(wl)[1:2] <- c("date", "level_ft_navd88")
    
    if(nrow(wl) == 0){
      stop("No data available")
    }
    
    wl <- wl %>%
      transmute(
        location = "Oregon Inlet Gauge",
        date = lubridate::ymd_hm(date),
        level = as.numeric(level_ft_navd88)
      )
    
    return(wl)
  }
}


get_tides("Oregon Inlet Gauge")


## Water Level Data Functions for Hatteras and Oregon Inlet Gauges ###############

# Define functions to get local water levels from each location
get_thirdparty_wl <- function(location) {
  if(location == "Oregon Inlet Gauge"){
    request <-
      httr::GET(
        url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter/",
        query = list(
          "station" = "8652587",
          "date" = "latest",
          "product" = "water_level",
          "units" = "english",
          "datum" = "NAVD",
          "time_zone" = "lst",
          "format" = "json",
          "application" = "UNC_Institute_for_the_Environment, https://github.com/acgold"
        )
      )
    
    wl <-
      tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$data)
    colnames(wl)[1:2] <- c("date", "level_ft_navd88")
    
    if(nrow(wl) == 0){
      stop("No data available")
    }
    
    wl <- wl %>%
      transmute(
        location = "Oregon Inlet Gauge",
        date = lubridate::ymd_hm(date),
        level = as.numeric(level_ft_navd88)
      )
    
    return(wl)
  }
  
  if(location == "Hatteras Gauge"){
    request <-
      httr::GET(
        url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter/",
        query = list(
          "station" = "8654467",
          "date" = "latest",
          "product" = "water_level",
          "units" = "english",
          "datum" = "NAVD",
          "time_zone" = "lst",
          "format" = "json",
          "application" = "UNC_Institute_for_the_Environment, https://github.com/acgold"
        )
      )
    
    wl <-
      tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$data)
    colnames(wl)[1:2] <- c("date", "level_ft_navd88")
    
    if(nrow(wl) == 0){
      stop("No data available")
    }
    
    wl <- wl %>%
      transmute(
        location = "Hatteras Gauge",
        date = lubridate::ymd_hm(date),
        level = as.numeric(level_ft_navd88)
      )
    
    return(wl)
  }
}

# Test

get_thirdparty_wl(location="Hatteras Gauge")
get_thirdparty_wl(location="Oregon Inlet Gauge")

## Adam's old code.

#------------------------ Functions to retrieve atm pressure -------------------
# Source env variables if working on desktop
# source("C:/Users/Adam Gold/Desktop/postgres_keys.R")
# source("C:/Users/Adam Gold/Desktop/noaa_api_token.R")
# source("postgres_keys.R")

# Connect to database
#con <- dbPool(
# drv = RPostgres::Postgres(),
#dbname = Sys.getenv("POSTGRESQL_DATABASE"),
#host = Sys.getenv("POSTGRESQL_HOSTNAME"),
#port = Sys.getenv("POSTGRESQL_PORT"),
#password = Sys.getenv("POSTGRESQL_PASSWORD"),
#user = Sys.getenv("POSTGRESQL_USER")
#)

# download the sensor locations so we can use it easier
#sensor_locations <- con %>%
#  tbl("sensor_locations") %>%
# collect() 

# These are just connections for reading/writing
#raw_data <- con %>%
#  tbl("sensor_data")

#processed_data <- con %>% 
#  tbl("sensor_data_processed")




#-------------------- Process the data ---------------------------
monitor_function <- function(debug = T) {
  
  new_data <- raw_data %>% 
    filter(processed == F) %>% 
    collect()
  
  if(nrow(new_data) == 0){
    if (debug == T) {
      cat("- No new raw data!", "\n")
    }
  }
  
  if(nrow(new_data) > 0){
    
    # Create a column for the atm pressure, fill it with NAs, remove any duplicated rows
    pre_interpolated_data <- new_data %>%
      dplyr::mutate("pressure_mb" = NA_real_) %>%
      group_by(place) %>% 
      arrange(date) %>%
      distinct() %>% 
      ungroup()
    
    # Get all of the place names of the new data so we can iterate through
    place_names <- unique(new_data$place)
    
    # process atm data and new data for each place   
    interpolated_data <- foreach(i = 1:length(place_names), .combine = "rbind") %do% {
      
      selected_place_name <- place_names[i]
      
      # select new data for each place
      pre_interpolated_data_filtered <- pre_interpolated_data %>% 
        filter(place == selected_place_name)
      
      # extract the date range and duration
      new_data_date_range <- c(min(pre_interpolated_data_filtered$date, na.rm=T)-minutes(30), max(pre_interpolated_data_filtered$date, na.rm=T)+minutes(30))
      new_data_date_duration <- time_length(diff(new_data_date_range), unit = "days")
      
      if(new_data_date_duration >= 30){
        chunks <- ceiling(new_data_date_duration / 30)
        span <- duration(new_data_date_duration / chunks, units = "days")
        
        atm_tibble <- foreach(j = 1:chunks, .combine = "rbind") %do% {
          range_min <- new_data_date_range[1] + (span * (j - 1))
          range_max <- new_data_date_range[1] + (span * j)
          
          get_atm_pressure(place = selected_place_name,
                           begin_date = range_min,
                           end_date = range_max) 
        }
        
        atm_tibble <- atm_tibble %>% 
          distinct(date, .keep_all=T)
      }
      
      if(new_data_date_duration < 30){
        # get atm pressure from NOAA
        atm_tibble <- get_atm_pressure(
          place = selected_place_name,
          begin_date = new_data_date_range[1],
          end_date = new_data_date_range[2]
        ) %>% 
          distinct()
      }
      
      interpolated_data_filtered <- pre_interpolated_data_filtered %>% 
        filter(date > min(atm_tibble$date, na.rm=T) & date < max(atm_tibble$date, na.rm=T)) %>% 
        mutate(pressure_mb = approxfun(atm_tibble$date, atm_tibble$pressure_mb)(date)) 
      
      if(debug == T) {
        cat("- New raw data detected for:", selected_place_name, "\n")
        cat("-",pre_interpolated_data_filtered %>% nrow(),"new rows", "\n")
        cat("- Date duration is",round(new_data_date_duration,digits = 2),"days", "\n")
        cat("-",(pre_interpolated_data_filtered %>% nrow())-(interpolated_data_filtered %>% nrow()),"new observations filtered out b/c not within atm pressure range","\n")
      }
      
      processing_data <- interpolated_data_filtered %>%
        left_join(sensor_locations, by = c("place", "sensor_ID")) %>% 
        transmute(
          place = place,
          sensor_ID = sensor_ID,
          date = date,
          road_water_level = NA,
          road_elevation = road_elevation,
          sensor_water_level = NA,
          sensor_elevation = sensor_elevation,
          atm_pressure = pressure_mb,
          sensor_pressure = ifelse(pressure < 500, pressure/10 * 68.9476, pressure),
          voltage = voltage,
          notes = notes.x
        ) %>%
        mutate(
          sensor_water_level = ((((sensor_pressure - atm_pressure) * 100
          ) / (1020 * 9.81)) * 3.28084) + sensor_elevation,
          road_water_level = sensor_water_level - road_elevation,
          qa_qc_flag = F,
          tag = "new_data"
        ) 
      
      final_data <- processing_data %>% 
        rbind(processed_data %>% 
                filter(date >= !!new_data_date_range[1] & date <= !!new_data_date_range[2]) %>% 
                collect() %>% 
                mutate(tag = "processed_data")) %>% 
        mutate(diff_lag = sensor_water_level - lag(sensor_water_level),
               time_lag = time_length(date - lag(date), unit = "minute"),
               diff_per_time_lag = diff_lag/time_lag,
               qa_qc_flag = ifelse((diff_per_time_lag >= abs(.1)) ,T,F)) %>% 
        filter(tag == "new_data") %>% 
        dplyr::select(-c(tag,diff_lag, time_lag, diff_per_time_lag))
      
      final_data
    }
    
    dbx::dbxUpsert(
      conn = con,
      table = "sensor_data_processed",
      records = interpolated_data,
      where_cols = c("place", "sensor_ID", "date"),
      skip_existing = T
    )
    
    dbx::dbxUpdate(conn = con,
                   table="sensor_data",
                   records = new_data %>% mutate(processed = T),
                   where_cols = c("place", "sensor_ID", "date")
    )
    
    if (debug == T) {
      cat("- Wrote to database!", "\n")
    }
  }
}

# Run infinite loop that updates atm data and processes it
run = T

while(run ==T){
  start_time <- Sys.time()
  print(start_time)
  monitor_function(debug = T)
  
  delay <- difftime(Sys.time(),start_time, units = "secs")
  
  Sys.sleep((60*6) - delay)
}


#---------------- Wunderground web scraping ----------------------
hatteras_weather <- xml2::read_html("https://www.wunderground.com/weather/us/nc/hatteras")

## EHW: Error - no applicable method for 'xml_find_first' applied to an object of class "xml_missing"

timestamp <- hatteras_weather %>% 
  html_node(".timestamp") %>%
  html_node(".timestamp :nth-child(2)") %>%
  html_node(".wu-unit-pressure .wu-value-to") %>%
  html_text() %>%
  stringr::str_split(.,pattern = " on ") %>% 
  unlist()

extractor<-function(file,htmlnode){
  
  if(inherits(x, 'xml_missing')){
    a<-c('non-exist')
    
  }else{
    
    a<-read_html(file)%>%
      html_node(htmlnode)%>%
      html_text()%>%
      clean_up()
  }
  
  return(a)
}

extractor(timestamp,htmlnode)

timestamp[1] %>% lubridate::hms()

wu_pressure <- hatteras_weather %>% 
  html_node(".timestamp") %>%
  html_node(".timestamp :nth-child(2)") %>%
  html_node(".wu-unit-pressure .wu-value-to") %>%
  html_text() %>%
  as.numeric() 

round(wu_pressure * 33.8639)

tictoc::toc()

#------------ Carolina Beach openweathermap --------------------
# owm_city_codes <- tibble::tibble("location" = c("Carolina Beach, North Carolina"),
# "owm_code" = c(4459261))

# owm_request <- httr::GET(url="https://api.openweathermap.org/data/2.5/weather",
#                          query=list(
#                            id = 4459261,
#                            appid= APP_ID))
# 
# owm_latest_atm_pressure <- tibble::tibble(location = "Carolina Beach, North Carolina",
#                                           date = lubridate::floor_date(Sys.time(), "1 minutes"),
#                                           pressure_mb = jsonlite::fromJSON(rawToChar(owm_request$content))$main$pressure,
#                                           notes = "owm.org")
# if(atm_in_db$date[atm_in_db$location == owm_latest_atm_pressure$location] < owm_latest_atm_pressure$date){
#   
#   DBI::dbAppendTable(conn = con,
#                      name = "atm_pressure",
#                      value = owm_latest_atm_pressure)
# }

#------------ EHW: Buxton Openweather Map --------------------
owm_city_codes <- tibble::tibble("location" = c("Buxton, North Carolina"),
                                 "owm_code" = c(4458391))

APP_ID <- 31de1ff3785bbeba413b5e6045aebb73

## EHW: Openweather APPID not being accepted. 
## Code runs up to a point if I omit it, then fails.

owm_request <- httr::GET(url="https://api.openweathermap.org/data/2.5/weather",
                         query=list(
                           id = 4458391,
                           appid=31de1ff3785bbeba413b5e6045aebb73))

owm_latest_atm_pressure <- tibble::tibble(location = "Buxton, North Carolina",
                                          date = lubridate::floor_date(Sys.time(), "1 minutes"),
                                          pressure_mb = jsonlite::fromJSON(rawToChar(owm_request$content))$main$pressure,
                                          notes = "owm.org")

## EHW
## Error: atm_in_db not found
## Code wont run past here  

if(atm_in_db$date[atm_in_db$location == owm_latest_atm_pressure$location] < owm_latest_atm_pressure$date){
  
  DBI::dbAppendTable(conn = con,
                     name = "atm_pressure",
                     value = owm_latest_atm_pressure)
}


#------------------ nws atm ---------------------------------
latest_atm <- foreach(i = 1:nrow(nws_stationIDs), .combine = "bind_rows") %do% {
  nws_request <- httr::GET(url = paste0("https://api.weather.gov/stations/",nws_stationIDs$stationID[i],"/observations/latest?require_qc=true"))
  nws_request_parsed <- jsonlite::fromJSON(rawToChar(nws_request$content))
  
  latest_atm_pressure <- tibble::tibble("location" = nws_stationIDs$location[i],
                                        "nws_code" = nws_request_parsed$properties$station,
                                        "date" = lubridate::ymd_hms(nws_request_parsed$properties$timestamp),
                                        "pressure_pa" = nws_request_parsed$properties$barometricPressure$value,
                                        "pressure_mb" = pressure_pa/100)
  latest_atm_pressure
}

atm_in_db <- atm_pressure %>%
  group_by(location) %>%
  filter(date == max(date, na.rm = T)) %>% 
  collect()

foreach(i = 1:nrow(latest_atm), .combine = "bind_rows") %do% {
  if(atm_in_db$date[atm_in_db$location == latest_atm$location[i]] < latest_atm$date[i]){
    
    DBI::dbAppendTable(conn = con,
                       name = "atm_pressure",
                       value = latest_atm[i,])
  }
}