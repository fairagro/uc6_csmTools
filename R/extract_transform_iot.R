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

# Get sensor metadata
token <- get_frost_token(url = base_url, client_id = client_id,
                         client_secret = client_secret, username = username, password = password)


get_iot_metadata <- function(url, token,
                             object = c("device","sensor","observed_property","datastream","observations"),
                             object_id) {
  
  url <- paste0(url, switch(object,
                            "device" = paste0("Things(", object_id, ")"),
                            "sensor" = paste0("Sensors(", object_id, ")"),
                            "observed_property" = paste0("ObservedProperties(", object_id, ")"),
                            "datastream" = paste0("Datastream(", object_id, ")")
  ))
  
  raw <- GET(url, add_headers(`Authorization` = paste("Bearer", token)))
  
  json <- fromJSON(content(raw, as = "text"))
  return(as.data.frame(json))
}

tmp <- get_iot_metadata(url = user_url,
                        token = token,
                        object = "device",
                        object_id = 7)


airtemp_sensor_raw <- GET(paste0(url, "Sensors(1)"), 
                      add_headers(`Authorization` = paste("Bearer", token)))
airtemp_sensor_json <- fromJSON(
  content(airtemp_sensor_raw, as = "text")
)

# TODO: pull back device/location/sensor/property metadata from datastream
get_iot_data <- function(url, datastream_id, token) {
  
  # function to replace fill null columns
  replace_null_with_na <- function(x) {
    if (is.null(x)) {
      return(NA)
    } else if (is.list(x)) {
      return(lapply(x, replace_null_with_na))
    } else {
      return(x)
    }
  }
  
  # Get data
  all_obs <- list()
  i <- 1
  
  repeat {
    url_obs <- paste0(url, "Datastreams(", datastream_id, ")/Observations(", i, ")")

    raw <- GET(url_obs, add_headers(`Authorization` = paste("Bearer", token)))
    
    if (status_code(raw) != 200) {
      break
    }
    
    all_obs[[i]] <- fromJSON(content(raw, as = "text"))
    all_obs[[i]] <- replace_null_with_na(all_obs[[i]])  # necessary for dataframe conversion
    i <- i + 1
  }
  
  data <- do.call(rbind, lapply(all_obs, as.data.frame))
  
  
  # Get metadata
  url_ds <- paste0(user_url, "Datastreams(", datastream_id, ")")
  metadata_datastream <- fromJSON(
    content(
      GET(url_ds, add_headers(`Authorization` = paste("Bearer", token))),
      as = "text")
  )
  
  metadata_property <- fromJSON(
    content(
      GET(metadata_datastream$`ObservedProperty@iot.navigationLink`,
          add_headers(`Authorization` = paste("Bearer", token))),
      as = "text")
  )
  
  metadata_device <- fromJSON(
    content(
      GET(metadata_datastream$`Thing@iot.navigationLink`,
          add_headers(`Authorization` = paste("Bearer", token))),
      as = "text")
  )
  
  metadata_sensor <- fromJSON(
    content(
      GET(metadata_datastream$`Sensor@iot.navigationLink`,
          add_headers(`Authorization` = paste("Bearer", token))),
      as = "text")
  )
  
  # Compile metadata
  metadata <- list(
    device = metadata_device,
    sensor = metadata_sensor,
    datasstream = metadata_datastream,
    property = metadata_property
  )
  
  out <- list(data = data, metadata = metadata)
  
  return(out)
}

token <- get_frost_token(url = base_url, client_id = client_id,
                         client_secret = client_secret, username = username, password = password)
obs_airtemp <- get_iot_data(url = user_url, datastream_id = 1, token = token)


# Transform to daily data
obs_airtemp_fmt <- 
  as.data.frame(obs_airtemp$data) %>%
  mutate(result = as.numeric(result),
         date = as.Date(ymd_hms(phenomenonTime))) %>%
  group_by(date) %>%
  summarise(TMIN = min(result, na.rm = TRUE),
            TMAX = max(result, na.rm = TRUE),
            TMEAN = mean(result, na.rm = TRUE), .groups = "drop")

# TODO: make metadata header for 
