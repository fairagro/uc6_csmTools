#' Find years column in a design data frame based on temporal coverage metadata
#' 
#' @param df a data frame
#' @param start a character or date depicting the first day of first year of the experiment
#' @param end a character or date depicting the last day of last year of the experiment
#' 
#' @return the name of the year column
#' 
#' @export
#'

get_years_col <- function(df, start, end){
  
  is_year <- apply(df, 2, function(x)
    all(x >= year(start)) & all(x <= year(end))
  )
  
  return(names(df[is_year]))
}

#' Find treatments column in a design data frame based on the years and plots column
#' 
#' @param df a data frame
#' @param years_col the name of the years column
#' @param plots_col the name of the plots column
#' 
#' @return a list containing the name of the treatment column and the number of replicates
#' 
#' @export
#'

get_treatments_col <- function(df, years_col, plots_col){
  
  pkey <- get_pkeys(df, alternates = FALSE)
  
  # Unidenified columns
  cols <- setdiff(names(design), c(pkey, years_col, plots_col))
  # Drop plots and keep unique rows
  df_noplots <- design %>%
    dplyr::select(-all_of(PLOTS_nm)) %>%
    distinct()
  
  # Find column with fixed number of rows per group (i.e., number of replicates)
  trt_col <- NULL
  reps_n <- NULL
  for (i in cols) {
    
    reps <- design_noplots %>%
      group_by(.[[i]]) %>% 
      summarise(n = n()/ YEARS_n) %>%
      pull(n)
    
    if(length(unique(reps))==1){
      trt_col <- c(trt_col, i)
      reps_n <- unique(reps)
    }
    
  }
  
  return(list(trt_col, reps_n))
}

#' Find a table based on its primary key
#' 
#' @param db a list of linked data frames
#' @param pkey a character, the name of the focal primary key
#' 
#' @return a data frame with its name in the input list as an attribute
#' 
#' @export
#'

get_tbl <- function(db, pkey){
  
  pkeys <- lapply(db, function(df) get_pkeys(df, alternates = FALSE))
  tbl <- db[[which(pkeys == pkey)]]
  attr(tbl, "tbl_name") <- names(db[which(pkeys == pkey)])
  
  return(tbl)
} 