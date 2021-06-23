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
#library(dbx)
library(readr)
library(httr)
library(later)
library(rvest)
library(xml2)
library(noaaoceans)


## Pull tide data: Use noaaoceans ############################

tides_today <- function(location) {

  # get today's date
  date <- Sys.Date()
  date <- gsub("-", "", date)

  ## get hi/lo tide data
  if(location == "Oregon Inlet Marina") {
    station_id <- '8652587'
  }
  if(location == "USCG Hatteras") {
    station_id <- '8654467'
  }

  tides <- noaaoceans::query_coops_data(
    station_id,
    date,
    date,
    'predictions',
    units = "english",  # feet
    time_zone = "lst_ldt",
    interval = 'hilo',
    datum = 'MLLW')  # alternatively, 'MHW'

  #wind <- noaaoceans::query_coops_data(
  #  station_id,
  #  begin_date=date,  # if we want the last hour, need to specify, otherwise it will give us the full day
  #  end_date=date,
  #  data_product='wind',
  #  units = "english",  # feet
  #  time_zone = "lst_ldt")

  return(tides)
}
