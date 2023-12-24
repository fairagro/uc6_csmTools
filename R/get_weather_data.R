#' Select DWD weather stations measuring solar radition closest to the given coordinates
#' 
#' Select DWD stations within a given radius around a set of coordinates. Workaround for rdwd::nearbyStations not
#' functioning for solar raidation data
#' 
#' @param lat a numeric depicting the latitude (y component) of the target location [degrees N/S, range 47:55]
#' @param lon a numeric depicting the longitude (x component) of the target location [degrees N/S, range 47:55]
#' @param res a character setting the maximum data recording starting date and minimum ending date
#' @param per ###
#' 
#' @return a data frame containing information on weather stations that best  meet the specified location, date range,
#' variables, and temporal resolution parameters
#' 
#' @importFrom rdwd nearbyStations selectDWD dataDWD metaInfo
#' @importFrom dplyr filter arrange distinct left_join mutate
#' @importFrom spsUtil quiet
#' @importFrom OSMscale earthDist
#' 
#' @export
#'

nearbyStations_solar <- function(lat, lon, res, max_radius = 50){  ##inherit
  
  data_ls <- selectDWD(var = "solar", res = c("daily","hourly"), expand = TRUE)
  
  # Extract the IDs of all stations corresponding to the criteria
  wst_ids <- as.numeric(sapply(data_ls, function(x) {
    x <- strsplit(x, "_")[[1]]
    result <- x[length(x) - 1]
  }))
  
  wst_res <- unlist(
    lapply(data_ls, function(x) {
    strsplit(x, "/")[[1]][8]
  }
  ), use.names = FALSE)
  
  data_urls <- data.frame(Stations_id = as.integer(wst_ids), res = wst_res, url = data_ls)
  
  # Get station data directly from the metadata as nearbyStations does not return solar radiation data consistently
  # Cause: "per" argument set to "" for multiple records, should either "historical" or "recent" for fun to work normally
  stations <- do.call(
    rbind, 
    lapply(wst_ids, function(df) quiet(metaInfo(df)))
  )
  stations <- stations[stations$var == "solar" & stations$res %in% res, ]
  
  # Calculate distance from target location
  target <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(stations)))
  names(target) <- names(stations)
  target$geoBreite <- lat
  target$geoLaenge <- lon
  
  stations <- rbind(target, stations)
  stations$dist <- earthDist("geoBreite", "geoLaenge", i = 1, data = stations)
  
  stations <- stations %>%
    # Format dates so filter can be applied
    mutate(von_datum = as_date(von_datum), bis_datum = as_date(bis_datum),
           # Fil gaps in period (as of 21.12.23, missing data in solar for some reason)
           per = ifelse(per == "", "historical", per)) %>%
    #relocate(var_ipt, .before = everything()) %>%
    left_join(data_urls, by = c("Stations_id", "res")) %>%
    filter(dist <= max_radius) %>%
    arrange(dist) %>%
    distinct()

  return(stations)
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
  
  
  wst <- lapply(params_ipt, function(x){
    
    nearbyStations(lat = lat,
                   lon = lon,
                   radius = x[["max_radius"]][[1]],
                   var = x[["vars"]][[1]],
                   res = x[["res"]][[1]],
                   per = per,
                   mindate = min_date,
                   quiet = TRUE)
  }
  )

  #
  if("solar_radiation" %in% names(params)){
    
    params_solar <- params$solar_radiation
    
    wst_solar <- nearbyStations_solar(lat = lat,
                                      lon = lon,
                                      res = params_solar[["res"]][[1]],
                                      max_radius = params_solar[["max_radius"]][[1]])
    
    wst_solar$var <- "solar"
    wst_full <- append(wst, list(wst_solar))
    
  }
  names(wst_full)[length(wst_full)] <- "solar_radiation"
  
  # Keep the closest station that meets the minimum date requirement
  wst_sub <- lapply(wst_full, function(df){
    suppressWarnings(df %>%
                       filter(von_datum <= min_date & bis_datum >= min_date)
                       #filter(dist == min(dist, na.rm = TRUE))
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
#' @importFrom dplyr filter arrange
#' @importFrom lubridate as_date year
#' 
#' @export
#'

download_dwd <- function(
    vars = c("air_temperature","precipitation","solar_radiation","dewpoint","relative_humidity","wind_speed"),
    year,
    stations,
    dir = tempdir()){
  
  from_date <- paste0(year, "-01-01")
  to_date <- paste0(year, "-12-31")
  
  dwd_data <- lapply(vars, function(x){

    # Sort the stations by distance from the field, from lower to higher
    station <- stations %>%
      filter(var_ipt == x) %>%
      arrange(dist) %>%
      filter(as_date(von_datum) <= as_date(from_date) & as_date(bis_datum) >= as_date(to_date))
    
    if (nrow(station) == 0){
      data <- data.frame()
      
    } else {

      # Download data starting from the closest station
      for (i in 1:nrow(station)){
        
        url <- selectDWD(id = station[i, ]$Stations_id,
                         res = station[i, ]$res,
                         per = station[i, ]$per,
                         var = station[i, ]$var)
        data <- dataDWD(url, read = TRUE, dir = dir, quiet = TRUE)
        
        # Append station metadata for later reuse 
        attr(data, "var") <- x
        attr(data, "res") <- station[i, ]$res
        attr(data, "wst_id") <- paste0("DWD", station[i, ]$Stations_id)
        attr(data, "wst_name") <- station[i, ]$Stationsname
        attr(data, "wst_xy") <- c(station[i, ]$geoBreite, station[i, ]$geoBreite)
        attr(data, "dist") <- station[i, ]$dist
        
        # Break the loop if data is available for the target year
        # This check is necessary as some stations have significant time gaps than are not reported in stations metadata
        if (year %in% year(as_date(data$MESS_DATUM))){
          break
        }
      }
    }
    
    # Print progress information as the process can take a while
    if (nrow(data) == 0) {
      
      message(paste0(year, ": ",  x, " - data not available"))
      return(data)
      
    } else {
      
      data <- data %>%
        filter(as_date(MESS_DATUM) >= as_date(from_date) &  as_date(MESS_DATUM) <= as_date(to_date))
      
      message(paste0(year, ": ",  x, " - data download complete"))
      return(data)
    }

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
#' @importFrom lubridate is.POSIXct is.POSIXlt is.POSIXt as_date
#' 
#' @export
#'

format_wth <- function(data, lookup) {
  
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

#' Impute missing values in daily weather time series data
#' 
#' So far only performs linear interpolation and extrapolation. Other methods will be added in the future.
#' (e.g., imputation with remote sensing NASA power data, sequential imputation, random forests, etc.)
#' 
#' @param df a data frame with weather time series data in which NAs are to be replaced
#' @param na.rm logical. If the result of the (spline) interpolation still results in leading and/or trailing NAs,
#' should these be removed (using na.trim)?
#' @param rule an integer (of length 1 or 2) specifying how leading and trailing NAs are handled. 
#' If rule is 1 then NAs are returned for such points and if it is 2, the value at the closest data extreme is used.
#' 
#' @return a data frame with NAs replaced according to the input arguments
#' 
#' @importFrom zoo na.approx
#' 
#' @export
#'

impute_weather <- function(df, na.rm = TRUE, rule = 2) {
  
  na_cols <- colSums(is.na(df)) > 0
  df_na <- df[, na_cols, drop = FALSE]
  
  df_na[] <- lapply(df_na, function(x) {
    if (is.numeric(x)) {
      x <- na.approx(x, na.rm = na.rm, rule = rule)
    }
    return(x)
  })
  
  df <- cbind(df[, !na_cols], df_na)
  return(df)
  
  # TODO: other methods (NASA-power imputation, sequential imputation, random forest...)
}

#' Wrapping function for downloading, formatting and imputing daily weather time series
#' 
#' Source repositories are currently limited to DWD Open Data Server, and imputation method to linear interpolation
#' 
#' @param lat a numeric; latitude (y coordinate) of the target location [degrees E/W, range 6:15]
#' @param lon a numeric; longitude (x coordinate) of the target location [degrees N/S, range 47:55]
#' @param years a numeric vector specifying the target years
#' @param src a character vector specifying the source repository (currently only "dwd" is supported).
#' @param map_to a character vector specifying the target data standard (currently only "icasa" and "dssat" are supported)
#' @param vars a character vector specifying the target weather variables.
#' @param res a list of character vectors specifying the temporal resolution of the target weather variables. Length and
#' indices should match the variable vector. A the function returns daily weather, only "hourly" and "daily" are supported.
#' @param max_radius a numeric vector specifying the maximum distance around the location within which weather stations
#' will be selected. Length and indices should match the variable and res vectors.
#' 
#' @return a list of dataframes containing daily weather for the requested location, years and variables
#' Each data frame contains one full year of data.
#' 
#' @importFrom dplyr distinct mutate select
#' @importFrom tibble rownames_to_column
#' 
#' @export
#'

get_weather <- function(
    years, lon, lat,
    src = c("dwd","nasa_power"),  # may implement other sources in the future
    map_to = c("icasa","dssat"),  # add option for not mapping to any standard
    vars = c("air_temperature", "precipitation", "solar_radiation", "dewpoint", "relative_humidity", "wind_speed"),
    res = list("hourly", c("daily", "hourly"), c("daily", "hourly"), "hourly", "hourly", "hourly") ,
    # max radius in km; defined separately for each variable as quality requirements differ
    max_radius = c(50, 10, 50, 20, 20, 20)){
  
  
  # Define parameters -------------------------------------------------------
  
  start_dates <- sapply(years, function(x) paste0(x, "-01-01"))
  
  vars_ipt <- lapply(vars, function(x) {
    switch(x,
           precipitation = c("precipitation","more_precip"),
           solar_radiation = "solar",
           wind_speed = "wind",
           dewpoint = "dew_point",
           relative_humidity = "moisture",
           x)
  }
  )
  
  params <- data.frame(vars = I(vars_ipt), res = I(res), max_radius = max_radius)
  params <- split(params, seq(nrow(params)))
  for (i in 1:length(params)){
    names(params)[i] <- unique(vars[i])
  }
  
  
  # Download the data -------------------------------------------------------
  
  if (src == "dwd"){
    
    # Get stations metadata
    
    stations <- lapply(start_dates, function(x) find_stations(lat = lat, lon = lon, min_date = x, params = params))
    names(stations) <- paste0("Y", substr(start_dates, 1, 4))
    
    stations <- do.call(rbind, stations) %>%
      distinct() %>%
      rownames_to_column("var_ipt") %>%
      # Remove the data frame name prefix appended during row binding
      mutate(var_ipt = gsub("^[^.]*\\.", "", var_ipt)) %>%
      # Remove the station suffix
      mutate(var_ipt = gsub("\\..*", "", var_ipt))
    
    
    # Download the data
    
    # Here one caveat is that the rdwd::dataDWD function downloads the entire historical data for the specified station(s)
    # Target year is only filtered afterwards in the custom function
    # this makes the runtime needlessly slow for multiple years/variables
    dwd_raw <- lapply(years, function(x){
      data <- download_dwd(vars = names(params), year = x, stations)
      names(data) <- names(params)
      return(data)
    }
    )
    names(dwd_raw) <- paste0("Y", years)
  }
  
  # Format and map the data -------------------------------------------------
  
  # TODO: link metadata attributes from df to common columns prior to merge (currently they get lost)
  
  # Drop variables with no data (empty data frames)
  dwd_ipt <- lapply(dwd_raw, function(df) Filter(function(x) nrow(x) > 0, df))##
  
  # Select adequate lookup
  lookup <- switch(src,
                   nasa = switch(map_to,
                                 icasa = nasa_icasa,
                                 dssat = nasa_dssat
                   ),
                   dwd = switch(map_to,
                                icasa = dwd_icasa,
                                dssat = dwd_dssat
                   )
  )
  
  vars <- c("MESS_DATUM", lookup[["repo_var"]][!is.na(lookup[["repo_var"]])])
  
  # Apply mapping
  dwd_ipt <- lapply(dwd_ipt, function(ls) {
    lapply(ls, function(df){
      common_vars <- intersect(vars, names(df))
      df[, common_vars, drop = FALSE]
    })
  })
  
  dwd_trans <- lapply(dwd_ipt, function(ls){
    lapply(ls, function(df) data <- format_wth(df, lookup = lookup))
  })
  
  dwd_out <- lapply(dwd_trans, function(ls){
    Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), ls)
  })
  
  
  # Impute missing data -----------------------------------------------------
  
  dwd_out <- lapply(dwd_out, function(df){
    if (anyNA(df)) {
      
      col_order <- colnames(df)
      
      df <- impute_weather(df, na.rm = TRUE, rule = 2)
      df <- df %>% select(all_of(col_order))
    }
    return(df)
  })
  
  return(dwd_out)
}
