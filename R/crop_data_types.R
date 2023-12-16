#' Identify the type of experimental data
#' 
#' Determines whether a data table contains management data (i.e., fixed experimental parameters), observed data
#' (i.e., parameters measured during the experiment) or other attributes (i.e., neither management nor observed data)
#' 
#' @param db a list of data frames
#' @param years_col character string; name of the "year" column
#' @param plots_col character string; name of the plot identifier column
#' @param plots_len integer; length of the plot identifier (i0.e., number of plots)
#' @param max_events integer; maximum number of management event of the same type (e.g., irrigation) within one year; default is 8
#' 
#' @return the input database tagged as "management", "observed" or "other" data category (as attr(., "category"))
#' 
#' @importFrom leroy-bml/csmTools R/get_pkeys
#' @importFrom dplyr distinct
#' 
#' @export
#'

tag_data_type <- function(db, years_col, plots_col, plots_len, max_events = 8) {
  
  # Tag tables independent of plots as "other" as both management and observed data are tied to the plots
  df_desc <- lapply(db, function(df){
    
    if(!plots_col %in% names(df) | !years_col %in% names(df) ){
      
      attr(df, "category") <- "other"
      return(df)
    }
  })
  
  df_desc[sapply(df_desc, is.null)] <- NULL
  
  
  db <- db[!names(db) %in% names(df_desc)]
  
  db_nokeys <- lapply(db, function(df){
    
    plots <- if(plots_col %in% colnames(df)){ plots_col } else { NULL }
    
    mngt_id <- get_pkeys(df, alternates = FALSE) # primary keys
    
    mngt_cols <- setdiff(names(df), c(mngt_id, plots))
    df <- df[colnames(df) %in% mngt_cols]
    df <- distinct(df)
    
  })
  
  # Calculate number of rows (i.e., unique data) per year
  rows_yr <- lapply(db_nokeys, function(df){

    df_grouped <- split(df, df[[years_col]]) 
    result <- sapply(df_grouped, nrow)
    result <- mean(result)
    
    return(result)
  })
  
  df_tags <- lapply(rows_yr, function(x){
    if(x <= max_events){ 
      "management" # few data points per plot within year (up to max_event)
    } else if(x > plots_len) {
      "observed-timeseries" # more than one data point per plot within year
    } else {
      "observed-summary" # ca. one data point per plot within year (can be less due to possible duplicate values)
    }
  } )
  
  db_tagged <- mapply(function(df, x){
    attr(df, "category") <- x
    return(df)
  }, db, df_tags, SIMPLIFY = FALSE)
  
  db_out <- append(db_tagged, df_desc)
  return(db_out)
  
}

#' Identify if a management type is an experimental treatment
#' 
#' Determines for each year whether a management type has different fixed value among plots within year (experimental treatment)
#' or is uniform among plots within year.
#' 
#' @param df a data frame containing management data
#' @param years_col character string; name of the "year" column
#' @param plots_col character string; name of the plot identifier column
#'  
#' @return a data frame containing the year column and a logical "is_trt"
#' 
#' @importFrom leroy-bml/csmTools R/get_pkeys
#' @importFrom dplyr group_by_at arrange mutate across c_across rowwise select n_distinct summarise
#' @importFrom tidyr matches all_of spread
#' 
#' @export
#'

is_treatment <- function(df, years_col, plots_col){
  
  if(is.null(plots_col)){
    df$plots_id = 1
    plots_col <- names(df)[ncol(df)]
  }
  
  mngt_id <- get_pkeys(df, alternates = FALSE)
  date_col = colnames(df[apply(df, 2, is_date)])[1] ### CHECK WHY DOES NOT WORK WITH SAPPLY
  # [1] is in case they are two dates, e.g., sowing and emergence (not great solution...)
  
  management_cols <- setdiff(names(df), c(mngt_id, years_col, plots_col))
  
  out <- df %>%
    # Count event within years (ordered by date to attribute the same rank to identical events)
    group_by_at(c(years_col, plots_col)) %>%
    arrange(!!sym(date_col)) %>%
    mutate(events_count = row_number()) %>%
    # Collapse all management features into one variable for wide format conversion
    mutate(across(matches(paste(management_cols, collapse = "|")), as.character)) %>%
    rowwise() %>%
    mutate(mngt_full = paste(c_across(all_of(management_cols)), collapse = "|")) %>%
    # Convert to wide format, i.e., year x plot matrix with management as values
    dplyr::select(all_of(c(years_col, plots_col)), events_count, mngt_full) %>%
    spread(plots_col, mngt_full) %>%
    # Count the number of unique management events among plots
    rowwise() %>%
    mutate(unique_count = n_distinct(c_across(3:ncol(.)))) %>%
    # If there are more than one distinct management sequence among plots (within year), return TRUE
    group_by_at(years_col) %>%
    summarise(is_trt = any(ifelse(unique_count > 1, TRUE, FALSE))) %>%
    as.data.frame()
  
  return(out)
}