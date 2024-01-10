#' Reset index variables to start at 1
#' 
#' Includes a special case for ignoring 0s (ID in treatment matrix with no linked record in a management section)
#' 
#' @export
#' 
#' @param df a data frame with rows identified by an integer index variable
#' @param id_col a length 1 character containing the name of the index column in df
#' 
#' @return a data frame; the input data with a reset index variable
#'

reset_id <- function(df, id_col) {
  
  id <- df[[id_col]]
  suppressWarnings(
    min_id <- min(id[id != 0])
  )
  
  id[id > 0] <- id[id > 0] - min_id + 1
  df[[id_col]] <- id
  
  return(df)
}

#' Split a multiyear crop experiment dataset by year
#' 
#' Handles all artefacts created by spitting the original data (e.g., empty data frames, columns with only NAs)
#' 
#' @export
#' 
#' @param ls a list of data frames containing ICASA-structured crop experiment data
#' 
#' @return a list containing n lists of crop experiment data tables, where n is the number of years in the experiment
#'

split_by_year <- function(ls) {

  # Drop metadata common to all years
  ls_ipt <- ls[!names(ls) %in% c("GENERAL", "FIELDS", "SOIL_Header", "SOIL_Layers")]  #!!
  
  # Split data by year
  ls_yr <- lapply(ls_ipt, function(df) split(df, f = df[["Year"]]))
  ls_yr <- revert_list_str(ls_yr)
  names(ls_yr) <- paste0("Y", names(ls_yr))
  
  # Remove empty data frames, e.g., management categories irrelevant for the focal year
  ls_yr <- lapply(ls_yr, function(ls) ls[lengths(ls) > 0])
  
  # Remove columns with only NAs in each data frame
  ls_yr <- lapply(ls_yr, function(ls){
    ls <- lapply(ls, function(df) {
      df[, colSums(is.na(df)) != nrow(df)]
    })
  })
  
  # Special case: file A with only TRTNO & Year, TimeSeries with only TRTNO, Year, DATE
  ls_yr <- lapply(ls_yr, function(ls){
    if (all(colnames(ls[["OBSERVED_Summary"]]) %in% c("TRTNO","Year","DATE"))) {
      ls[["OBSERVED_Summary"]] <- NULL
    }
    if (all(colnames(ls[["OBSERVED_TimeSeries"]]) %in% c("TRTNO","Year","DATE"))) {
      ls[["OBSERVED_TimeSeries"]] <- NULL
    }
    return(ls)
  })
  
  # Reset management IDs
  filex_sec_ids <- c("N", "C", "L", "A", "C", "P", "T", "I", "F", "R", "C", "E", "H")
  filex_trt_ids <- c("CU", "FL", "SA", "IC", "MP", "MI", "MF", "MR", "MC", "MT", "ME", "MH", "SM")
  
  ls_yr <- lapply(ls_yr, function(ls) {
    
    ls_mngt <- ls[names(ls) %in% filex_sections]
    ls_rest <- ls[!names(ls) %in% filex_sections]
    
    # Reset management IDs in treatment matrix #!! CHECK!!! FERTILIZER
    for (i in colnames(ls_mngt[["TREATMENTS"]])) {
      if (i %in% filex_trt_ids) {
        ls_mngt[["TREATMENTS"]] <- reset_id(ls_mngt[["TREATMENTS"]], i)
      }
    }
    
    # Reset management IDs in management data franes
    ls_mngt <- lapply(ls_mngt, function(df) {
      for (i in colnames(df)) {
        if (i %in% filex_sec_ids) {
          df <- reset_id(df, i)
        }
      }
      return(df)
    })
    
    return(c(ls_rest, ls_mngt))
  })
  
  return(ls_yr)
}

#' Prepare data tables for export as various DSSAT input file types
#' 
#' @export
#' 
#' @param df a data frame of the data to be formatted
#' @param template a model data frame for the focal file type, as generated in 'dssat_file_templates.R'
#' 
#' @return a data frame; a formatted DSSAT input table
#' 
#' @importFrom dplyr "%>%" bind_rows select slice arrange
#' @importFrom tidyr all_of
#' @importFrom purrr map map_lgl
#' @importFrom lubridate as.POSIXct
#' @importFrom DSSAT as_DSSAT_tbl
#'

format_table <- function(df, template) {
  
  has_lists <- map_lgl(template, is.list)
  is_list <- has_lists[has_lists]
  
  # Collapse columns of composite sections
  if (length(is_list) > 0) {
    df <- collapse_cols(df, intersect(names(is_list), names(df)))
  }
  
  col_names <- intersect(names(df), names(template))
  
  # Apply template format to the input table
  for (col_name in col_names) {
    
    if(class(template[[col_name]][[1]])[1] == "POSIXct") {
      if (is.list(template[[col_name]])) {
        df[[col_name]][[1]] <- as.POSIXct(as.Date(df[[col_name]][[1]], format = "%Y-%m-%d"))
      } else {
        # TODO: ensure that input dates are always in the format YYYY-MM-DD
        df[[col_name]] <- as.POSIXct(as.Date(df[[col_name]], format = "%Y-%m-%d"))
      }
    }
    
    df[[col_name]] <- as(df[[col_name]], class(template[[col_name]])[1])
  }
  
  # Populate the template with the input data
  out <- template %>%
    bind_rows(df) %>%
    select(all_of(colnames(template))) %>%
    slice(-1) %>%
    arrange(row.names(template))
  
  # Format NULL lists for composite tables (e.g., IRRIGATION)
  has_lists <- map_lgl(out, is.list)
  out[has_lists] <- map(out[has_lists], ~ ifelse(is.null(.x[[1]]), rep(NA, length(out[[1]])), .x))
  
  
  return(as_DSSAT_tbl(out))
}


#' Nest selected columns into single cells
#' 
#' This function is necessary to format composite management tables, such as INITIAL_CONDITIONS and IRRIGATION
#' 
#' @export
#' 
#' @param df a data frame to collapse
#' @param cols a character vector of column names to collapse
#' 
#' @return a data frame where the specified rows have been collapsed into single rows based on all unique combinations of the remaining columns
#' 
#' @importFrom dplyr "%>%" group_by summarise across
#' @importFrom tidyr all_of
#'

collapse_cols <- function(df, cols) {
  
  grp_cols <- setdiff(colnames(df), cols)
  
  df %>%
    group_by_at(grp_cols) %>%
    summarise(across(all_of(cols), ~ list(.x)), .groups = "drop")
}


#' Format crop management data as DSSAT input tables
#' 
#' @export
#' 
#' @param ls a list of data frames containing crop management data, to be formatted into DSSAT input tables
#' @param title a length 1 character vector specifying the title of the dataset
#' @param site_code a length 1 character vector provide the DSSAT standard code for the experimental site
#' 
#' @return a data frame with codes mapped to the format specified in the supplied map
#' 
#' @importFrom dplyr "%>%" mutate across where
#'

# Assemble File X
build_filex <- function(ls, title = NULL, site_code = NA_character_) {
  
  # Apply the template format to each section
  year <- unique(ls[["PLANTING_DETAILS"]]$Year)
  ls <- ls[!grepl("WEATHER|SOIL|OBSERVED", names(ls))]
  sec_nms <- sort(names(ls))
  filex <- mapply(format_table, ls[sec_nms], FILEX_template[sec_nms], SIMPLIFY = FALSE)
  
  # Set default values for missing parameters in treatments matrix
  filex[["TREATMENTS"]] <- filex[["TREATMENTS"]] %>%
    mutate(R = ifelse(is.na(R), 1, R),  # default to 1
           O = ifelse(is.na(O), 0, O),  # default to 0
           C = ifelse(is.na(C), 0, C),  # default to 0
           SM = ifelse(is.na(SM), 1, SM),  # default to 1
    ) %>%  
    mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x)))  # exception is simulation controls
  
  # Check requirements for writing file X
  required_sections <- c("TREATMENTS","CULTIVARS","FIELDS","PLANTING_DETAILS")
  
  for (i in names(FILEX_template)) {
    if (length(filex[[i]]) == 0) {
      if(i %in% required_sections) {
        filex[[i]] <- FILEX_template[[i]]
        warning(paste0("Required section missing from input data: ", i))
      } else if (i == "GENERAL") {
        filex[[i]] <- FILEX_template[[i]]
        warning("No Metadata provided with input data. ###")
      } else if (i == "SIMULATION_CONTROLS") {
        filex[[i]] <- FILEX_template[[i]]
        warning("SIMULATION_CONTROLS section not provided with input data. Controls set to default values.")
      } else {
        filex[[i]] <- NULL
      }
    }
  }
  
  # Set section in the right order
  filex <- filex[match(names(FILEX_template), names(filex))]
  filex <- filex[lengths(filex) > 0]

  # Set atributes for file export
  attr(filex, "experiment") <- title
  extension <- paste0(unique(ls[["CULTIVARS"]]$CR), "X")
  attr(filex, "file_name") <- paste0(site_code, substr(year, nchar(year) - 1, nchar(year)), "01.", extension)
  attr(filex, "comments") <- paste0("File generated on ", Sys.Date(), " with csmTools")
  
  return(filex)
}


#' Format observed summary data as a DSSAT input table
#' 
#' @export
#' 
#' @param ls a list of data frames containing crop experiment data ### (File A)
#' @param title a length 1 character vector specifying the title of the dataset
#' @param site_code a length 1 character vector provide the DSSAT standard code for the experimental site
#' 
#' @return a data frame with codes mapped to the format specified in the supplied map
#' 
#' @importFrom dplyr "%>%" mutate select across where arrange
#' @importFrom DSSAT as_DSSAT_tbl
#'

build_filea <- function(ls, title = NULL, site_code = NA_character_) {
  
  if ("OBSERVED_Summary" %in% names(ls)) {
    
    year <- unique(ls[["OBSERVED_Summary"]]$Year)
    
    filea <- ls[["OBSERVED_Summary"]] %>%
      select(-Year) %>%
      mutate(across(where(is.Date), ~ format(as.Date(.x), "%y%j"))) %>%
      arrange(TRTNO) %>%
      as_DSSAT_tbl()

    # Set atributes for file export
    attr(filea, "v_fmt") <- v_fmt_filea[names(v_fmt_filea) %in% names(filea)]
    attr(filea, "experiment") <- title
    extension <- paste0(unique(ls[["CULTIVARS"]]$CR), "A")
    attr(filea, "file_name") <- paste0(site_code, substr(year, nchar(year) - 1, nchar(year)), "01.", extension)
    attr(filea, "comments") <- paste0("File generated on ", Sys.Date(), " with csmTools")

  } else {
    
    filea <- NULL
  }
  
  return(filea)
}


#' Format observed time series data as a DSSAT input table
#' 
#' @export
#' 
#' @param ls a list of data frames containing crop experiment data ### (File T)
#' @param title a length 1 character vector specifying the title of the dataset
#' @param site_code a length 1 character vector provide the DSSAT standard code for the experimental site
#' 
#' @return a data frame with codes mapped to the format specified in the supplied map
#' 
#' @importFrom dplyr "%>%" mutate select across where arrange
#' @importFrom DSSAT as_DSSAT_tbl
#'

build_filet <- function(ls, title = NULL, site_code = NA_character_) {
  
  if ("OBSERVED_TimeSeries" %in% names(ls)) {
    
    year <- unique(ls[["OBSERVED_TimeSeries"]]$Year)
    
    filet <- ls[["OBSERVED_TimeSeries"]] %>%
      select(-Year) %>%
      mutate(DATE = format(as.Date(DATE), "%y%j")) %>%
      arrange(TRTNO, DATE) %>%
      as_DSSAT_tbl()
    
    # Set atributes for file export
    attr(filet, "v_fmt") <- v_fmt_filet[names(v_fmt_filet) %in% names(filet)]  # print formats
    attr(filet, "experiment") <- title  # file title (1st row)
    extension <- paste0(unique(ls[["CULTIVARS"]]$CR), "T")  # DSSAT file extension
    attr(filet, "file_name") <- paste0(site_code, substr(year, nchar(year) - 1, nchar(year)), "01.", extension)
    attr(filet, "comments") <- paste0("File generated on ", Sys.Date(), " with csmTools")
    
  } else {
    
    filet <- NULL
  }
  
  return(filet)
}


#' Format observed time series data as a DSSAT input table
#' 
#' @export
#' 
#' @param ls a list of data frames containing soil profile data (layers + surface as separate data frames)
#' 
#' @return a data frame containing soil profile data formatted as a DSSAT input
#' 
#' @importFrom dplyr "%>%" slice bind_rows bind_cols
#'

build_sol <- function(ls) {
  
  data <- ls[["SOIL_Layers"]]
  header <- ls[["SOIL_Header"]]
  
  header_dups <- header %>%
    slice(1) %>%
    replicate(nrow(data), ., simplify = FALSE) %>%
    bind_rows()
  
  data <- bind_cols(header_dups, data)
  
  # Apply the template format for daily weather data
  sol <- format_table(data, SOIL_template)
  
  return(sol)
}


#' Format observed time series data as a DSSAT input table
#' 
#' Currently contains a workaround to correct date formatting issues with the write_wth function that leads to
#' failed simulation
#' 
#' @export
#' 
#' @param ls a list of data frames containing weather data (daily + header as separate data frames) and station
#' metadata as comments
#' 
#' @return a data frame containing soil profile data formatted as a DSSAT input
#' 
#' @importFrom dplyr "%>%" mutate
#' @importFrom DSSAT as_DSSAT_tbl
#'

build_wth <- function(ls) {
  
  data <- ls[["WEATHER_Daily"]]
  header <- ls[["WEATHER_Header"]]
  comments <- attr(ls[["WEATHER_Daily"]], "comments")
  
  # Apply the template format for daily weather data
  wth <- format_table(data, WEATHER_template) %>%
    mutate(DATE = format(as.Date(DATE), "%y%j"))
  
  # Corrected print formats (temp workaround for date formatting issue with write_wth)
  wth_fmt <- c(DATE = "%5s",  # instead of %7s
               SRAD = "%6.1f", TMAX = "%6.1f",TMIN = "%6.1f",
               RAIN = "%6.1f", DEWP = "%6.1f", WIND = "%6.0f",
               PAR = "%6.1f", EVAP = "%6.1f", RHUM = "%6.1f"
  )
  #wth <- as.data.frame(mapply(function(x, y) sprintf(y, x), wth, wth_fmt))
  
  # Make file names
  insi <- header$INSI
  year <- ls[["WEATHER_Header"]]$Year
  
  # Set metadata as attributes
  attr(wth, "v_fmt") <- wth_fmt  # corrected print formats
  attr(wth, "GENERAL") <- as_DSSAT_tbl(header[!names(header) == "Year"])
  attr(wth, "location") <- ls[["GENERAL"]]$SITE
  attr(wth, "comments") <- comments
  attr(wth, "file_name") <- paste0(insi, substr(year, nchar(year) - 1, nchar(year)), "01.WTH")
  
  return(wth)
}


#' Write multiple types of standard DSSAT files before
#' 
#' Only handles input tables that have been previously mapped to the DSSAT standard format
#' 
#' @export
#' 
#' @param ls a list DSSAT-formated crop experiment data 
#' @param path a character vector storing the path to the directory where the files will be written
#' 
#' @return NULL
#' 
#' @importFrom DSSAT write_filex write_filea write_filet write_sol write_wth
#'

# temp workaround for date formatting issue with DSSAT::write_wth (open an issue on GitHub)
write_wth2 <- function(wth, file_name) {
  write_wth(wth = wth, file_name = file_name, force_std_fmt = FALSE)
}

write_dssat <- function(ls, path = getwd()) {
  
  # Write functions (from DSSAT package)
  write_funcs <- list(
    FILEX = write_filex,
    FILEA = write_filea,
    FILET = write_filet,
    SOL = write_sol,
    WTH = write_wth2
  )
  
  walk(names(write_funcs), ~{
    if (!is.null(ls[[.]])) {
      write_funcs[[.]](ls[[.]], file_name = paste0(path, "/", attr(ls[[.]], "file_name")))
    } else if (. %in% c("filex", "wth", "sol")) {
      warning(paste0("Required ", ., " file is missing."))
    }
  })
}
