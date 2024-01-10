#' Retrieve the parent table(s) of a table
#' 
#' @param tbl a data frame
#' @param tbl_list a list of data frames
#' 
#' @return the names of the parent tables of the focal table
#' 
#' @importFrom leroy-bml/csmTools R/get_pkeys
#' 
#' @export
#'

get_parent <- function(tbl, tbl_list) {
  
  cols <- colnames(tbl)
  
  pkeys <- lapply(tbl_list, function(df) get_pkeys(df, alternates = FALSE))
  
  parent_nms <- c()
  
  for (df_name in names(tbl_list)) {
    
    if (identical(tbl_list[[df_name]], tbl)) next
    
    common_cols <- intersect(cols, pkeys[[df_name]])
    
    if (length(common_cols) > 0) {
      parent_nms <- c(parent_nms, df_name)
    }
  }
  
  return(parent_nms)
}

#' Determine whether two data frames are linked by a common column
#' 
#' @param df1 the first data frame
#' @param df2 the second data frame
#' @param subset ###
#' 
#' @return a logical stating whether the two data frames are dependent or not
#' 
#' @export
#'

has_link <- function(df1, df2, subset = NULL) {
  
  common_cols <- intersect(names(df1), names(df2))
  
  if(!is.null(subset)){
    common_cols <- common_cols[common_cols %in% subset]
  }
  
  return(length(common_cols) > 0)
}
