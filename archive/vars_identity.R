# FUN - identify whether variables are parent keys (id unique across database), child key (id present in multiple table
# and used for joining them) or an attribute (any non-id variable)
ident_vars <- function(db, df_name, col_name, type = "int") {
  
  df <- db[[df_name]]
  
  is_uniqueid <- is_unique_id(df, col_name, type = type)
  is_present_elsewhere <- as.logical(lapply(db[!names(db) %in% df_name], function(df) col_name %in% names(df)))
  is_unique_db <- if(any(is_present_elsewhere) == TRUE) { FALSE } else { TRUE }
  
  tmp <- if (is_uniqueid == TRUE & is_unique_db == TRUE) {
    "parent"
  } else if (is_uniqueid == TRUE & is_unique_db == FALSE) {
    "child"
  } else if (is_uniqueid == FALSE) {
    "attr"
  }
  
  return(tmp)
}
# FUN - produce a summary of variable types across all tables of the database
get_var_ident <- function(db, type = c("all","int")) {
  results <- data.frame(var = character(), var_type = logical())
  for (i in names(db)) {
    for (j in colnames(db[[i]])) {
      result <- ident_vars(db, i, j, type = type)
      new_row <- data.frame(table = i, var = j, var_type = result)
      # Append the new row to the results dataframe
      results <- rbind(results, new_row)
    }
  }
  
  dups <- unique(results$var[duplicated(results$var) | duplicated(results$var, fromLast = TRUE)])
  
  out <- results %>%
    group_by(var) %>%
    mutate(var_type = ifelse(any(var %in% dups) & any(var_type == "child"), "child", var_type)) %>%
    ungroup()
  
  return(out)
}



#' Skip NAs in aggregation functions regardless of whether na.rm is an argument
#' 
#' @param x a function name
#' 
#' @return a function name with na.rm argument as appropriate
#' 
#' @export
#'

# FUN - bind or drop fields in tables where it is missing (always assuming absence = data applies to all rows) # BEGINING
bind_keys <- function(df, parent, colname) {
  
  if (!colname %in% colnames(df)) {
    fields <- unique(parent[colname])
    df <- crossing(df, fields)
  }
  
  return(df)
}