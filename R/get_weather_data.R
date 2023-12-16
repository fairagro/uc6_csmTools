#' Select DWD weather stations clost to the given coordinates
#' 
#' Select DWD stations within a given radius around a set of coordinates. Workaround for nearbyStations not functioning
#' for solar raidation data
#' 
#' @param lat a numeric depicting the latitude (y component) of the target location [degrees N/S, range 47:55]
#' @param lon a numeric depicting the longitude (x component) of the target location [degrees N/S, range 47:55]
#' @param res a character setting the maximum data recording starting date and minimum ending date
#' @param per ###
#' 
#' @return a data frame containing information on weather stations that best  meet the specified location, date range,
#' variables, and temporal resolution parameters
#' 
#' @importFrom rdwd nearbyStations
#' @importFrom dplyr filter
#' 
#' @export
#'

nearbyStations_solar <- function(lat, lon, res, per){##inherit
  
  datalist <- selectDWD(res = c("hourly","daily"), per = per, var = "solar")
  
  # Get weather stations for air temperature (most widely measured variable)
  wst <- nearbyStations(lat = lat,
                        lon = lon,
                        radius = 50,
                        var = "air_temperature",
                        res = unique(params_ipt$air_temperature[["res"]]), ##!!
                        per = per,
                        mindate = date,
                        quiet = TRUE)
  
  # Extract the IDs of all stations corresponding to the criteria
  wst_ids <- as.numeric(sapply(datalist, function(x) {
    
    x <- strsplit(x, "_")[[1]]
    result <- x[length(x) - 1]
  }))
  
  # Retrieve in the station list Keep only the closest station
  solar_wst <- wst[which(wst$Stations_id %in% wst_ids), ]
  solar_wst$var <- "solar"
  solar_wst_id <- solar_wst[which.min(wst$dist), ]$Stations_id
  
  return(solar_wst)
}

#' Select closest DWD weather stations
#' 
#' Download DWD past weather data for crop modeling relevant variables: air temperature, precipitation, solar radiation,
#' dew point temperature, relative humidity, and wind speed. The function picks the best quality data based on the provided
#' list of weather stations separately for each target variable. Additional arguments for picking data types, like
#' temporal resolution, are specified within the weather station data frame.
#' 
#' @param lat a numeric depicting the latitude (y component) of the target location [degrees N/S, range 47:55]
#' @param lon a numeric depicting the longitude (x component) of the target location [degrees N/S, range 47:55]
#' @param min_date a character setting the maximum data recording starting date and minimum ending date
#' @param params ###
#' 
#' @return a data frame containing information on weather stations that best  meet the specified location, date range,
#' variables, and temporal resolution parameters
#' 
#' @importFrom rdwd nearbyStations
#' @importFrom dplyr filter
#' 
#' @export
#'

# Get station data for each year based on the set quality parameters
find_stations <- function(lat, lon, min_date, params){
  
  # Set period
  per <- if(min_date <= Sys.Date() %m-% months(18)) { "historical" } else { "recent" }
  
  # Find closest station for each weather variable
  if("solar_radiation" %in% names(params)){
    
    params_ipt <- params[!names(params)=="solar_radiation"]
    
  } else {
    
    params_ipt <- params
  }
  
  wst_full <- lapply(params_dup, function(df){
    
    # 
    wst <- nearbyStations(lat = lat,
                          lon = longitude,
                          radius = unique(df[["max_radius"]]),
                          var = unique(df[["var"]]),
                          res = unique(df[["res"]]),
                          per = per,
                          mindate = min_date,
                          quiet = TRUE)
    
    return(wst)
  })
  names(wst_full) <- names(params_dup)
  
  #
  if("solar_radiation" %in% names(params)){
    
    
    solar_df <- params[names(params)=="solar_radiation"]
    wst_solar <- nearbyStations_solar(res = unique(solar_df[["res"]]), per = per)
    wst_solar$var <- "solar"
    wst_full <- append(wst_full, list(wst_solar))
  }
  names(wst_full)[length(wst_full)] <- "solar_radiation"
  
  # Keep the closest station that meets the minimum date requirement
  wst_sub <- lapply(wst_full, function(df){
    suppressWarnings(df %>%
                       filter(von_datum <= min_date & bis_datum >= min_date) %>%
                       filter(dist == min(dist, na.rm = TRUE))
    )
  })
  
  return(do.call(rbind, wst_sub))
}


#' Download DWD past weather in a specific year for a given set of variables
#' 
#' Download DWD past weather data for crop modeling relevant variables: air temperature, precipitation, solar radiation,
#' dew point temperature, relative humidity, and wind speed. The function picks the best quality data based on the provided
#' list of weather stations separately for each target variable. Additional arguments for picking data types, like
#' temporal resolution, are specified within the weather station data frame.
#' 
#' @param vars a character vector specifying the variable(s) to download
#' @param year an integer indicating the target year
#' @param stations a data frame listing target weather stations, a generated by the functions nearbyStations",
#' "nearbyStations_solar", and "find_stations"
#' 
#' @return a list of named data frames, each containing weather data for one of the target variable(s)
#' 
#' @importFrom rdwd selectDWD dataDWD
#' 
#' @export
#'

download_dwd <- function(
    vars = c("air_temperature","precipitation","solar_radiation","dewpoint","relative_humidity","wind_speed"),
    year,
    stations){
  
  from_date <- paste0(year, "-01-01")
  to_date <- paste0(year, "-12-31")
  
  dwd_data <- lapply(vars, function(x){
    
    station <- stations[which(stations$var_ipt == x &
                                stations$von_datum <= from_date & stations$von_datum <= to_date),]
    
    if(nrow(station)>1){ 
      
      station <- station[which.min(station$dist),]
    }
    
    if(nrow(station)==0){
      
      data <- data.frame()
      
    } else {
      
      url <- selectDWD(id = station$Stations_id, res = station$res, per = station$per, var = station$var)
      data <- dataDWD(url, read = TRUE, quiet = TRUE) #dir = locdir(dir = tmpdir), 
    }
    
    attr(data, "var") <- x
    attr(data, "res") <- station$res
    attr(data, "wst_id") <- paste0("DWD", station$Stations_id)
    attr(data, "wst_name") <- station$Stationsname
    attr(data, "wst_xy") <- c(station$geoBreite, station$geoBreite)
    
    date <- names(data[sapply(data, is_date)])[1] ##TOCHECK
    data <- data %>%
      filter(!!date >= as_date(from_date) &  !!date <= as_date(to_date))
    
    return(data)
  })
  
  return(dwd_data)
}

#' Format DWD weather data into a specified data standard
#' 
#' Transform DWD weather data into a specified data standard (as of 14.12.23: ICASA or DSSAT)
#' 
#' @param data a character vector specifying the variable(s) to download
#' @param lookup a map for a target standards (TODO: replace by standard name, fetch the map internally)
#' 
#' @return ###
#' 
#' @importFrom leroy-bml/csmTools R/etl_utils
#' 
#' @export
#'

format_wth <- function(data, lookup) { ##!! replaces fmt_dssat
  
  # Find date column
  data <- lapply(data, function(x){
    if(is.POSIXct(x)|is.POSIXlt(x)|is.POSIXt(x)){ 
      as_date(x)
    } else {
      x
    }
  })
  data <- as.data.frame(data)
  
  # Drop date column for mapping numeric variables
  date <- data[sapply(data, is_date)]
  data <- data[!names(data) %in% names(date)]
  
  # Create empty dataframe with model-standard naming
  lookup_sub <- lookup[lookup$repo_var %in% names(data),]
  out_fmt <- as.data.frame(matrix(ncol = nrow(lookup_sub), nrow = nrow(data)))
  names(out_fmt) <- lookup_sub$std_var
  
  #
  out_fmt[ncol(out_fmt)+1] <- date

  for (i in 1:nrow(lookup_sub)) {
    
    old_name <- lookup_sub$repo_var[i]
    new_name <- lookup_sub$std_var[i]
    
    if (old_name %in% names(data)) {
      out_fmt[[new_name]] <- data[[old_name]]
    } else {
      out_fmt[[new_name]] <- NA
    }
  }
  
  # Aggregate and convert to model-standard units
  agg_data <- list()
  
  for (i in seq_len(nrow(lookup_sub))) {
    
    column <- lookup_sub$std_var[i]
    agg_fun <- lookup_sub$agg_fun[i]
    conv_fct <- lookup_sub$conv_fct[i]
    
    if (is.na(agg_fun)) {
      agg_var <- cbind(unique(date), var = NA)
      colnames(agg_var)[2] <- column
    } else {
      agg_var <- aggregate(out_fmt[column],
                           by = list(out_fmt[, ncol(out_fmt)]),
                           FUN = function(x) safe_aggregate(x, match.fun(agg_fun)))
      agg_var[2] <- agg_var[2] * conv_fct
    }
    
    agg_data[[i]] <- agg_var
  }
  
  out <- Reduce(function(x,y) merge(x,y), agg_data)
  
  # Map date variable
  names(out)[1] <- na.omit(lookup$std_var[lookup$repo_var == names(date)])[1]
  
  return(out)
}