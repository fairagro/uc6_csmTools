# Load packages tmp -------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)


# Obtain token (4min validity) --------------------------------------------

get_frost_token <- function(url, client_id, client_secret, username, password) {
  
  response <- POST(
    url = url,
    body = list(
      grant_type = "password",
      client_id = client_id,
      client_secret = client_secret,
      username = username,
      password = password
    ),
    encode = "form",
    add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
    verbose()
  )
  
  token <- content(response)$access_token
  
  return(token)
}

token <- get_frost_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)


# NOT RUN: simulate and push data onto FROST server --------------------------

## Create virtual device, sensor, and datastream, with corresponding obs property (air temperature) ------

### Device (Thing)
# body_thing <- toJSON(
#   list(
#     name = "HEF_wsta_virtual",
#     description = "HEF building virtual weather station",
#     Locations = list(
#       list(
#         name = "Freising, Germany",
#         encodingType = "application/vnd.geo+json",
#         description = "Freising, HEF building",
#         location = list(
#           coordinates = c(11.72400, 48.40173),
#           type = "Point"
#         )
#       )
#     )
#   ), auto_unbox = TRUE
# )
# 
# token <- get_frost_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)
# response <- POST(
#   url = paste0(user_url, "Things"),
#   body = body_thing,
#   encode = "json",
#   add_headers(
#     `Content-Type` = "application/json",
#     `Authorization` = paste("Bearer", token)
#   ),
#   verbose()
# )
# 
# # Observed Property
# body_obsPpt <- toJSON(
#   list(
#     name = "air_temperature",
#     description = "https://en.wikipedia.org/wiki/Atmospheric_temperature",
#     definition = "Celsius"
#   ), auto_unbox = TRUE
# )
# 
# token <- get_frost_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)
# response <- POST(
#   url = paste0(user_url, "ObservedProperties"),
#   body = body_obsPpt,
#   encode = "json",
#   add_headers(
#     `Content-Type` = "application/json",
#     `Authorization` = paste("Bearer", token)
#   ),
#   verbose()
# )
# #print(content(response, "text"))
# 
# # Sensor
# body_sensor <- toJSON(
#   list(
#     name = "UC6VT01",
#     encodingType = "application/pdf",
#     metadata = "http://wiki.seeedstudio.com/Grove-Temperature_and_Humidity_Sensor_Pro/",
#     description = "http://wiki.seeedstudio.com/Grove-Temperature_and_Humidity_Sensor_Pro/"
#     ), auto_unbox = TRUE
# )
# 
# token <- get_frost_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)
# response <- POST(
#   url = paste0(user_url, "Sensors"),
#   body = body_sensor,
#   encode = "json",
#   add_headers(
#     `Content-Type` = "application/json",
#     `Authorization` = paste("Bearer", token)
#   ),
#   verbose()
# )
# print(content(response, "text"))  # error
# 
# # Datastream
# body_datastream <- toJSON(
#   list(
#     name = "air_temperature_hef",
#     unitOfMeasurement = list(
#       name = "Celsius",
#       symbol = "C",
#       definition = "https://en.wikipedia.org/wiki/Celsius"
#     ),
#     Thing = list(
#       `@iot.id` = 7
#     ),
#     description = "A datastream for air temperature measured by the HEF virtual weather station",
#     Sensor = list(
#       `@iot.id` = 1
#     ),
#     ObservedProperty = list(
#       `@iot.id` = 3
#     ),
#     observationType = "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement"  # clarify
#   ), auto_unbox = TRUE
# )
# 
# token <- get_frost_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)
# response <- POST(
#   url = paste0(user_url, "Datastreams"),
#   body = body_datastream,
#   encode = "json",
#   add_headers(
#     `Content-Type` = "application/json",
#     `Authorization` = paste("Bearer", token)
#   ),
#   verbose()
# )
# #print(content(response, "text"))  # error
# 
# ## Simulate two months of hourly temperature data ------
# 
# start_date <- as.POSIXct("2024-05-01 00:05:00 CEST")
# end_date <- as.POSIXct("2024-06-30 23:05:00 CEST")
# 
# timestamps <- seq(from = start_date, to = end_date, by = "hour")  # hourly time stamps sequence
# 
# set.seed(123)  
# mean_temp <- 15  # mean temperature in degrees Celsius
# temp_variation <- 10  # temperature variation
# sim_temp_ts <- mean_temp + temp_variation * sin(2 * pi * (hour(timestamps) / 24)) + rnorm(length(timestamps), mean = 0, sd = 2)
# 
# sim_temp_ts_df <- data.frame(time = format(ymd_hms(timestamps, tz = "Europe/Berlin"), "%Y-%m-%dT%H:%M:%S%z"),
#                              air_temperature = sim_temp_ts
#                              ) %>%
#   mutate(time = str_replace(time, "(\\+\\d{2})(\\d{2})$", "\\1:\\2")) %>%
#   as.data.frame()
# 
# 
# ## Post the dummy data onto sensor hub ------
# 
# post_observation <- function(x){
#   
#   body_obs <- toJSON(
#     list(
#       phenomenonTime = x[1],
#       result = x[2],
#       Datastream = list(
#         `@iot.id` = 1
#       )
#     ), auto_unbox = TRUE
#   )
#   
#   response <- POST(
#     url = paste0(user_url, "Observations"),
#     body = body_obs,
#     encode = "json",
#     add_headers(
#       `Content-Type` = "application/json",
#       `Authorization` = paste("Bearer", token)
#     ),
#     verbose()
#   )
#   
#   print(content(response, "text"))
# }
# 
# token <- get_frost_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)
# apply(sim_temp_ts_df, 1, post_observation)  # post all observations
# TODO: silence messages


# Get observations and metadata ------------------------------------------- TODO

# placeholder, server down for few days...
tmp <- GET('http://localhost:8080/FROST-Server/v1.0/Things(1)', 
           #accept_json(), 
           add_headers('Authorization' = paste("Bearer", token))
           )
# TODO: apparently max 100 obs per stream, need apply function to pull entire months/years from single device

# Get observations and metadata -------------------------------------------

# ARGS: ds_id OR (?) sensor_id AND attribute / res = c("daily","weekly","monthly") / agg_fun = c("max", "min", "mean")


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



