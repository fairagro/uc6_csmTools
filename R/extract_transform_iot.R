#' Pull sensor data from FROST server (Sensorhub) and transform into ICASA format
#' 
#' @export
#'
#' @param x xxx
#' 
#' @return XXX
#' 
#' @importFrom httr
#' 

# Load packages tmp -------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)


# Obtain token ------------------------------------------------------------

base_url <- "http://tuzehez-sensors.srv.mwn.de:3000/"
#client_id <- "your-client-id"
#client_secret <- "your-client-secret"
username <- "temp"
password <- "temp"


response <- POST(
  url = base_url,
  body = list(
    grant_type = "password",
    #client_id = client_id,
    #client_secret = client_secret,
    username = username,
    password = password
  ),
  encode = "json"
)

token <- content(response)$access_token  # parse token
print(token)

# Get observations and metadata -------------------------------------------

# placeholder, server down for few days...
tmp <- GET('http://tuzehez-sensors.srv.mwn.de:6002/FROST-Server/v1.0/Observations(12177)', 
           accept_json(), 
           add_headers('Authorization' = 'Bearer 31232187asdsadh23187'))
# TODO: apparently max 100 obs per stream, need apply function to pull entire months/years from single device

# Get observations and metadata -------------------------------------------

ds_obs_raw <- as.data.frame(
  fromJSON("inst/extdata/IOT-dataStreams/weather_station_air_temperature.json")  # placeholder until server is up
)

#
ds_obs_raw$value.phenomenonTime <- ymd_hms(iot100_temp$value.phenomenonTime)  # chr to datetime

# transform to daily data
ds_obs_fmt <- ds_obs_raw %>%
  mutate(date = as.Date(value.phenomenonTime)) %>%
  group_by(date) %>%
  summarise(TMEAN = mean(value.result, na.rm = TRUE), .groups = "drop")



