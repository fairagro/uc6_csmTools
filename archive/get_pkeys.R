#' Identify the primary key(s) in a table
#' 
#' 
#' @param df a data frame
#' @param col_name character string; name of the "year" column
#' @param type character string; specifies the data type for primary keys: "int" for integer, "all" for any type
#' 
#' @return a logical stating whether the input variable is a unique identifier
#' 
#' @export
#'

is_unique_id <- function(df, col_name, type = c("int","all")) {
  switch(
    type,
    "int" = {nrow(df) == length(unique(df[[col_name]])) & is.integer(df[[col_name]]) },
    "all" = {nrow(df) == length(unique(df[[col_name]])) }
  )
}


#' Identify the primary key(s) in a table
#' 
#' 
#' @param df a data frame
#' @param alternates logical; whether to include alternate primary keys in case when multiple column uniquely identfy the rows
#' 
#' @return a character vectir containing the primary key(s) column name(s)
#' 
#' @export
#'

get_pkeys <- function(df, alternates = FALSE){
  
  is_unique <- sapply(names(df), function(col_name) { 
    is_unique_id(df, col_name, type = "all")
  })
  
  is_pk <- is_unique[is_unique == TRUE]
  
  if(alternates == FALSE && length(is_pk) > 1){ 
    pk <- names(is_pk)[1]
  } else {
    pk <- names(is_pk)
  }
  
  return(pk)
}
