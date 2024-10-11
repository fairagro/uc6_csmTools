# Load packages tmp -------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(units)
library(DSSAT)
#library(usethis)
library(jsonlite)
library(tidygeocoder)
library(elevatr)

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

#### Post OGC data to the hosting server
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

#### Delete OGC objects on the hosting server
delete_ogc_iot <- function(object = c("Things","Sensors","ObservedProperties","Datastreams","Observations"), object_id, url, token){
  
  if (!grepl("^http[s]?://", url)) {
    stop("Invalid URL: Must start with http:// or https://")
  }
  
  url <- paste0(url, object, "(", object_id, ")")
  response <- DELETE(url,
                     add_headers(
                       `Content-Type` = "application/json",
                       `Authorization` = paste("Bearer", token)
                       ))
}

#### Modify OGC objects on the hosting server
patch_ogc_iot <- function(object = c("Things","Sensors","ObservedProperties","Datastreams","Observations"), object_id, url, token, body){
  
  if (!grepl("^http[s]?://", url)) {
    stop("Invalid URL: Must start with http:// or https://")
  }
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  url <- paste0(url, object, "(", object_id, ")")
  response <- PATCH(url, body = body_json, encode = "json",
                    add_headers(
                      `Content-Type` = "application/json",
                      `Authorization` = paste("Bearer", token)
                    ))
}


#### Find datastreams
# TODO: update function to handle multiple devices in single location
locate_datastreams <- function(url, token = NULL, var = c("air_temperature","solar_radiation","rainfall"), lon, lat, from, to, ...){
  
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
          tibble(Datastream_id = ds$`@iot.id`,
                 Datastream_link = ds$`@iot.selfLink`,
                 Datastream_name = ds$name,
                 Datastream_description = ds$description,
                 observationType = ds$observationType,
                 ObservedProperty_id = ds$ObservedProperty$`@iot.id`,
                 ObservedProperty_name = ds$ObservedProperty$name,
                 unitOfMeasurement_name = ds$unitOfMeasurement$name,
                 unitOfMeasurement_symbol = ds$unitOfMeasurement$symbol,
                 longitude = as.numeric(ds$observedArea$coordinates[1]),
                 latitude = as.numeric(ds$observedArea$coordinates[2]),
                 phenomenonTime = ds$phenomenonTime) %>%
            separate(phenomenonTime, into = c("start_date","end_date"), sep = "/") %>%
            mutate(across(start_date:end_date, ~ as.Date(.x)))
        })
      )
  }
  datastreams <- do.call(rbind, ds_out)

  # Find focal datastream(s)
  out <- datastreams %>% filter(longitude == lon & latitude == lat)

  if (nrow(out) == 0) {
    return("No data was measured at the specified location")
  } else {
    out <- out %>% filter(ObservedProperty_name %in% var)
    if (nrow(out) == 0) {
      return("No data for the focal property could be retrieved at the specified location.")
    } else {
      out <- out %>%
        mutate(is_contained = as.Date(from) >= start_date & as.Date(to) <= end_date) %>%
        filter(is_contained)
      if (nrow(out) == 0) {
        return("Measured data does not encompass the requested timeframe.")
      } else {
        return(out[-ncol(out)])
      }
    }
  }
}

#### Function to handle Sensorhub pagination
get_all_obs <- function(url, token) {
  
  url_ds <- sub("\\?.*", "", url)  # datastream url
  
  all_obs <- data.frame(phenomenonTime = character(0), result = numeric(0))
  i = 0
  repeat {
    response <- GET(url, add_headers(`Authorization` = paste("Bearer", token)))
    content <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    all_obs <- rbind(all_obs, content$Observations)
    if (is.null(content$`Observations@iot.nextLink`)) break
    i <- i+100
    url <- paste0(url_ds, "?$expand=Observations($select=phenomenonTime,result;$skip=",i,"),ObservedProperty($select=name)")
  }
  return(all_obs)
}

####
extract_iot <- function(url, token = NULL, var = c("air_temperature","solar_radiation","rainfall"), lon, lat, from, to) {
  
  if(is.null(token)) {
    token <- get_kc_token(url = url, client_id, client_secret, username, password)  # check token
  }
  
  # Find datastream
  ds_metadata <- locate_datastreams(url, token, var, lon, lat, from, to)
  
  urls_obs <- paste0(ds_metadata$Datastream_link, "?$expand=Observations($select=phenomenonTime,result;$skip=0),ObservedProperty($select=name)")
  data <- lapply(urls_obs, function(url) get_all_obs(url, token))
  names(data) <- var
  
  # Append metadata and format raw data
  for (i in seq_along(data)){
    attr(data[[i]], "metadata") <- ds_metadata[i,]  #TODO: enrich metadata w/ device/sensor name and description
    data[[i]] <- data[[i]] %>%
      mutate(measurement_date = ymd_hms(phenomenonTime)) %>%
      mutate(!!attr(data[[i]], "metadata")$ObservedProperty_name := as.numeric(result)) %>%
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

