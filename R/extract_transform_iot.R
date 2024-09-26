# Load packages tmp -------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(purrr)
library(units)
library(DSSAT)
library(usethis)

#use_build_ignore("./R/extract_transform_iot.R")


# Obtain keycloak token (4min validity) --------------------------------------------

get_kc_token <- function(url, client_id, client_secret, username, password) {
  
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


#### Get obseverations and metadata -----------

# Get sensor metadata
get_ogc_obs <- function(url_user, datastream_id, token) {
  
  # Build URL: currently select phenomenonTime and resul
  # See: https://developers.sensorup.com/docs/#observations_get for additional attributes
  url <- paste0(url_user, "Datastreams(", datastream_id, ")/Observations?$select=phenomenonTime,result")
  observations <- data.frame(phenomenonTime = character(), result = character(), stringsAsFactors = FALSE)
  
  # Get data (time series)
  while (!is.null(url)) {
    response <- GET(url, add_headers(`Authorization` = paste("Bearer", token)))
    data <- fromJSON(
      content(response, as = "text", encoding = "UTF-8")
    )
    observations <- rbind(observations, data$value)
    url <- data$`@iot.nextLink`
  }
  
  
  # Get metadata
  get_metadata <- function(url, token) {
    response <- GET(url, add_headers(`Authorization` = paste("Bearer", token)))
    fromJSON(content(response, as = "text"))
  }
  
  # Base URL for the Datastream
  url_datastream <- paste0(url_user, "Datastreams(", datastream_id, ")")
  datastream <- get_metadata(url_datastream, token)  # measured location (xy), measurement unit (name+symbol)
  property <- get_metadata(datastream$`ObservedProperty@iot.navigationLink`, token)  # property name (user-defined; here AGROVOC + snake_case)
  device <- get_metadata(datastream$`Thing@iot.navigationLink`, token)  # name + description device
  sensor <- get_metadata(datastream$`Sensor@iot.navigationLink`, token)  # name + metadata sensor (measurement heights?)
  
  # Compile metadata
  metadata <- list(
    device = device,
    sensor = sensor,
    datastream = datastream,
    property = property
  )
  
  out <- list(data = observations, metadata = metadata)
  
  return(out)
}

token <- get_kc_token(url = base_url, client_id = client_id, client_secret = client_secret, username = username, password = password)
obs_airtemp <- get_ogc_obs(url_user = user_url, datastream_id = 1, token = token)

# Compile DSSAT header
# TODO: handle original station name/DSSAT station name
# TODO: add commentary header: data pulled from TUM Sensorhub with csmTools on ####.##.###; Platform-registered device name: ####; sensor name: ####.
# TODO: format DSSAT output weather file (POST AND ADD MORE VARIABLES: SRAD, RAIN, WIND, RHUM)
# input_dssat[[1]]$WEAT_METADATA
format_ogc_icasa <- function(data, metadata){
}


data <- obs_airtemp$data
metadata <- obs_airtemp$metadata
var <- metadata$property$name  # standard: https://agrovoc.fao.org/browse/agrovoc/en/page/c_6496
units <- make_units(metadata$datastream$unitOfMeasurement$name)  # standard: https://en.wikipedia.org/wiki/List_of_metric_units

# Spatial metadata
lon <- metadata$datastream$observedArea$coordinates[[1]]
lat <- metadata$datastream$observedArea$coordinates[[2]]
obj_type <- metadata$datastream$observedArea$type

# Format data to ICASA
data <- data %>%
  mutate(!!sym(var) := as.numeric(result),
         DATE = as.Date(ymd_hms(phenomenonTime))) %>%
  select(DATE, !!sym(var))

### TODO: GET UNITS AND CONVERT IF NEEDED

# TODO: define ofc(agrovoc)-icasa data map as internal data
agg_funs <- list(
  TMAX = ~ if (cur_column() == "air_temperature") { max(.x, na.rm = TRUE) } else { NA} ,
  TMIN = ~ if (cur_column() == "air_temperature") { min(.x, na.rm = TRUE) } else { NA },
  RAIN = ~ if (cur_column() == "rainfall") { sum(.x, na.rm = TRUE) } else { NA },
  DEWP = ~ if (cur_column() == "dew_point_temperature") { mean(.x, na.rm = TRUE) } else { NA },
  WIND = ~ if (cur_column() == "wind_speed") { sum(.x, na.rm = TRUE) } else { NA },
  RHUM = ~ if (cur_column() == "relative_humidity") { mean(.x, na.rm = TRUE) } else { NA }
)

data_daily <- data %>%
  group_by(DATE) %>%
  summarise(across(all_of(var), agg_funs, .names = "{.fn}"))

# Format mmetadata
tav <- data_daily %>%
  summarise(TAV = mean((TMAX + TMIN)/2, na.rm = TRUE)) %>%
  pull(TAV)
amp <- data_daily %>%
  mutate(mo = month(DATE)) %>%
  group_by(mo) %>%
  summarise(mo_TAV = mean((TMAX + TMIN)/2, na.rm = TRUE)) %>%
  summarise(AMP = (max(mo_TAV)-min(mo_TAV))/2) %>%
  pull(AMP)

metadata <- data.frame(
  WSTA = "DEFS",  # TODO: retrieve location name and produce name
  LAT = lat,
  LONG = lon,
  ELEV = NA,
  TAV = tav,
  AMP = amp,
  REFHT = 2,  # document in device/sensor metadata? Would need text mining workflow
  WNDHT = NA  # document in device/sensor metadata?
  )

comments <- "TODO"

# TODO: add_property_mapping <- function(name, unit, icasa = list(), agg_funs = list())



###---- Compile file--------------------
iot_airtemp_fmt <- build_wth(ls = iot_airtemp, keep_unmapped = FALSE) # TODO: Test with SITE and DESCRIPTION in template
write_wth2(iot_airtemp_fmt, "test_iot.WTH")  # TODO: correct LAT/LONG
