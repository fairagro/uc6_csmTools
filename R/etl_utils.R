#' Determine whether a vector is date data
#' 
#' @param x a vector
#' 
#' @return a logical value indicating whether x is a date or not
#' 
#' @importFrom lubridate parse_date_time
#' 
#' @export
#'

is_date <- function(x) {
  
  # Convert all dates in DSSAT format %Y%j into usable formats
  if (is.numeric(x) & all(nchar(as.character(x)) == 5)) {
    
    x <- format(as.Date(as.character(x), format = "%y%j"), "%Y-%m-%d")
  }
  
  # If some values do not deparse (warning message), return NA (prevent parsing of 5 digits+ numericals)
  dates <- tryCatch(
    {
      parse_date_time(x, orders = c("Ymd", "mdY", "dmy", "ymd", "mdy"))
    },
    warning = function(w){
      return(NA)
    }
  )
  
  return(!all(is.na(dates)))
}

#' Skip NAs in aggregation functions regardless of whether na.rm is an argument
#' 
#' @param x a function name
#' 
#' @return a function name with na.rm argument as appropriate
#' 
#' @export
#'

safe_aggregate <- function(x, FUN, ...) {
  
  if("na.rm" %in% names(formals(FUN))) {
    return(FUN(x, na.rm = TRUE, ...))
  } else {
    return(FUN(x, ...))
  }
}

#' Subset rows from two data frames
#' 
#' Subsets rows from two data frames by returning te rows that are unique to each one
#' 
#' @param df1 the first data frame
#' @param df2 the second data frame
#' 
#' @return a data frame containing the rows that are unique to each data frame
#' 
#' @export
#'

substr_rows <- function(df1, df2) {
  
  diff1 <- anti_join(df1, df2)
  diff2 <- anti_join(df2, df1)
  
  diff <- bind_rows(diff1, diff2)
  
  return(diff)
}

#' Revert hierarchy of nested lists
#' 
#' @return a list of lists
#' 
#' @export
#'

revert_list_str <- function(ls) {
  x <- lapply(ls, `[`, names(ls[[1]]))
  apply(do.call(rbind, x), 2, as.list) 
}

#' Append data frame attributes to the first non-join columns so that they are not lost at joining
#' 
#' @return a list containing the two data frames with the attributes appended to the first non-join columns
#' 
#' @export
#'

attr_to_column <- function(df1, df2, attr_name){
  
  col1 <- setdiff(names(df1), names(df2))[1]
  attr(df1[[col1]], attr_name) <- attr(df1, attr_name)
  attr(df1, attr_name) <- NULL
  
  col2 <- setdiff(names(df2), names(df1))[1]
  attr(df2[[col2]], attr_name) <- attr(df2, attr_name)
  attr(df2, attr_name) <- NULL
  
  return(list(df1, df2))
}
