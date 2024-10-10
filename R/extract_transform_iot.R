# Load packages tmp -------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(units)
library(DSSAT)
library(usethis)
library(jsonlite)

#use_build_ignore("./R/extract_transform_iot.R")

####
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

#### Post OGC data
post_ogc_iot <- function(object = c("Things","Sensors","ObservedProperties","Datastreams","Observations"), body, url, token){
  
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  url <- paste0(url, object)
  response <- POST(url, body = body_json, encode = "json",
                   add_headers(
                     `Content-Type` = "application/json",
                     `Authorization` = paste("Bearer", token)
                   ),
                   verbose())
}

#### Find datastreams
locate_datastreams <- function(url, token = NULL, var = c("air_temperature","solar_radiation","rainfall"), lat, lon, from, to, ...){
  
  # Identify all devices from the target server
  locate_devices <- function(...) {
    
    url_locs <- paste0(url, "Things?$expand=Locations")
    response <- GET(url_locs, add_headers(`Authorization` = paste("Bearer", token)))
    devices <- fromJSON(
      content(response, as = "text", encoding = "UTF-8")
    )
    
    locations <- devices$value$Locations
    
    devices <- do.call(
      rbind,
      lapply(locations, function(df){
        df %>%
          rowwise() %>%
          mutate(x = location$coordinates[[1]][1],
                 y = location$coordinates[[1]][2],
                 url = paste0(url, "Things(", `@iot.id`, ")")) %>%
          rename(location_name = name, location_description = description) %>%
          #filter(x == lon & y == lat) %>%
          select(`@iot.id`, url, location_name, location_description, x, y) %>%
          as.data.frame()
      })
    )
    
    # if (nrow(devices) == 0) {
    #   return("No device was found at the specified coordinates.")
    # } else {
    #   return(devices)
    # }
  }
  devices <- locate_devices(url, token)
  #devices <- rbind(devices, devices)  #tmp
  devices_nms <- paste0("device_", devices$`@iot.id`)
  
  # Retrieve all datastreams for the selected devices
  url_dev_ds <- paste0(devices$url, "?$expand=Datastreams")
  response <- lapply(url_dev_ds, function(url) GET(url, add_headers(`Authorization` = paste("Bearer", token))))
  url_ds <- lapply(response, function(x) {
    fromJSON(
      content(x, as = "text", encoding = "UTF-8")
    )$Datastreams %>%
      pull(`@iot.selfLink`)
  })
  
  # Get datastreams metadata incl. observed properties
  url_ds_prop <- lapply(url_ds, function(url) paste0(url, "?$expand=ObservedProperty"))
  response <- lapply(url_ds_prop, function(urls) {
    lapply(urls, function(url) {
      GET(url, add_headers(`Authorization` = paste("Bearer", token)))
    })
  })
  ds_ls <- lapply(response, function(dev) {
    lapply(dev, function(x){
      fromJSON(
        content(x, as = "text", encoding = "UTF-8")
      )
    })
  })
  names(ds_ls) <- devices_nms
  
  ds_out <- list()
  for (i in seq_along(ds_ls)) {
    ds_out[[i]] <- 
      do.call(
        rbind,
        lapply(ds_ls[[i]], function(ds) {
          tibble(ds_id = ds$`@iot.id`,
                 ds_link = ds$`@iot.selfLink`,
                 ds_name = ds$name,
                 ds_description = ds$description,
                 obs_type = ds$observationType,
                 obs_property_id = ds$ObservedProperty$`@iot.id`,
                 obs_property = ds$ObservedProperty$name,
                 ds_unit = ds$unitOfMeasurement$name,
                 ds_lon = as.numeric(ds$observedArea$coordinates[2]),  #CHECK IF VALID
                 ds_lat = as.numeric(ds$observedArea$coordinates[1]),  #CHECK IF VALID
                 timeframe = ds$phenomenonTime) %>%
            separate(timeframe, into = c("start_date","end_date"), sep = "/") %>%
            mutate(across(start_date:end_date, ~ as.Date(.x)))
        })
      )
  }
  datastreams <- do.call(rbind, ds_out)
  
  # Find focal datastream(s)
  datastreams <- datastreams %>% filter(ds_lon == lon & ds_lat == lat)
  
  if (nrow(datastreams) == 0) {
    return("No data was measured at the specified location")
  } else {
    datastreams <- datastreams %>% filter(obs_property %in% var)
    if (nrow(datastreams) == 0) {
      return("No data for the focal property could be retrieved at the specified location.")
    } else {
      datastreams <- datastreams %>%
        mutate(is_contained = as.Date(from) >= start_date & as.Date(to) <= end_date) %>%
        filter(is_contained)
      if (nrow(datastreams) == 0) {
        return("Measured data does not encompass the requested timeframe.")
      } else {
        return(datastreams[-ncol(datastreams)])
      }
    }
  }
}

####
extract_iot <- function(url, token = NULL, var = c("air_temperature","solar_radiation","rainfall"), lon, lat, from, to) {
  
  if(is.null(token)) {
    token <- get_kc_token(url = url, client_id, client_secret, username, password)  # check token
  }
  
  # Find datastream
  ds_metadata <- locate_datastreams(url, token, var, lon, lat, from, to)
  
  # Extract data
  urls_obs <- paste0(ds_metadata$ds_link, "?$expand=Observations($select=phenomenonTime,result),ObservedProperty($select=name)")  # API call
  response <- lapply(urls_obs, function(url) GET(url, add_headers(`Authorization` = paste("Bearer", token))))  # get time series
  raw <- lapply(response, function(ds){
    fromJSON(
      content(ds, as = "text", encoding = "UTF-8")  #need obs property
    )$Observations
  })
  
  # Append metadata and format raw data
  data <- raw
  for (i in seq_along(data)){
    attr(data[[i]], "metadata") <- ds_metadata[i,]  #TODO: enrich metadata w/ device/sensor name and description
    data[[i]] <- data[[i]] %>%
      mutate(measurement_date = ymd_hms(phenomenonTime)) %>%
      mutate(!!attr(data[[i]], "metadata")$obs_property := as.numeric(result)) %>%
      select(-c(phenomenonTime, result))
  }
  return(data)
}

####
etl_iot_wrapper <- function(url, token, var = c("air_temperature","solar_radiation","rainfall"), lon, lat, from, to,
                            input_model, output_model, map = load_map(path), keep_unmapped = FALSE, col_exempt = NULL) {
  
  raw <- extract_iot(url, token, var, lon, lat, from, to)
  
  mapped_data <- map_data(df = raw[[1]], input_model, output_model, map, keep_unmapped = FALSE, col_exempt = NULL)
  
  return(mapped_data)
}



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
