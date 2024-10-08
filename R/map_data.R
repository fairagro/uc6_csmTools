#' Convert a mapping code list stored in a data frame as a string into a lookup table
#' 
#' The string format is analogous to the code_mapping field in standard ARDN SC2 json files around which the standard
#' mapping files for the package will be built. See: https://agmip.github.io/ARDN/Annotation_SC2.html
#' 
#' @export
#' 
#' @param vec a code mapping list [format: list('source1: target1', 'source2: target2')] stored as a string
#' 
#' @return a lookup dataframe with 2 columns: source (original data) and target (standard data)
#' 
#' @importFrom rlang eval_tidy parse_expr
#'

make_code_lookup <- function(vec){
  
  if (vec == "1") {
    lkp <- data.frame(source = NA_character_, tagret = NA_character_)
  } else {
    
    ls <- eval_tidy(parse_expr(vec))
    ls <- strsplit(as.character(ls), ": ")
    ls <- lapply(ls, function(x) list(source = x[1], target = x[2]))
    
    ls_lkp <- lapply(ls, function(lst) {
      if (grepl("c\\(", lst$source)) {
        df <- data.frame(source = strsplit(gsub("[c()]", "", lst$source), ", ")[[1]],
                         target = lst$target)
        return(df)
      } else {
        df <- as.data.frame(do.call(cbind, lst))
        return(df)
      }
      return(ls)
    })
    
    lkp <- as.data.frame(do.call(rbind, ls_lkp))
    
    lkp$target <- ifelse(lkp$target == 1, lkp$source, lkp$target)
  }

  return(lkp)
}


#' Map categorical data to standard codes
#' 
#' Currently handles lookup tables as maps rather than json SC2 files, which will be implemented in the future.
#' 
#' @export
#' 
#' @param df a data frame of the data to be mapped
#' @param map a lookup table of the categories to be madde to standard codes, as generated by 'made_code_lookup'
#' 
#' @return a data frame with codes mapped to the format specified in the supplied map
#' 
#' @importFrom dplyr recode_factor
#' @importFrom rlang "!!!"
#'

map_codes <- function(df, map){
  
  map <- map[map$std_header %in% colnames(df),]
  
  for (i in 1:nrow(map)){

    if (map$std_unit[i] == "code") {

      header <- map$std_header[i]
      
      mappings <- map$code_mappings[i]
      lookup <- make_code_lookup(mappings)
      var <- df[[header]]
      
      if (all(is.na(lookup))) {
        next
      } else {
        df[[header]] <- recode_factor(var, !!!setNames(as.list(lookup$target), lookup$source), .default = NA_character_)
      }
    }
  }
  return(df)
}


#' Convert numeric variables based on units provided as strings
#' 
#' ####str unit formats
#' 
#' @export
#' 
#' @param x a numeric to be converted
#' @param u1 the input unit, in which x is expressed
#' @param u2 the target unit, in which the output is expressed
#' 
#' @return a converted numeric
#' 
#' @importFrom magrittr "%>%"
#' @importFrom units set_units
#'

convert_unit <- function(x, u1, u2){
  set_units(x, u1, mode = "standard") %>% set_units(u2, mode = "standard") %>% as.numeric()
}


#' Map a data table into a standard format
#' 
#' Currently only handles exact matches (header names, units, and codes). Future versions will handle more complex
#' data mapping (e.g., conditional, concatenation, etc.)
#' 
#' @export
#' 
#' @param df a data frame of the data to be mapped
#' @param table_name the name of the focal crop experiment data section in the map
#' @param map a lookup table detailing input and target headers, units, and codes
#' @param keep_unmapped a logical indicating whether to keep unmapped variables in the output
#' @param col_exempt a vector of column names to be kept into the output if keep_unmapped = FALSE
#' 
#' @return a data frame with headers, units, and codes mapped to the format specified in the map
#' 
#' @importFrom dplyr distinct
#'

map_data <- function(df, tbl_name, map, keep_unmapped = TRUE, col_exempt = NULL){
  
  df0 <- df  # Store original data
  map <- map[map$table_name == tbl_name,]  # Restrict map to the focal data section
  mapped_cols <- c()  # Empty vector to store mapped column names
  
  
  # Map headers
  for (i in seq_along(colnames(df))) {
    for (j in 1:nrow(map)){
      
      if (colnames(df)[i] == map$column_header[j]){
        if (is.na(map$column_header[j]) | map$std_header[j] == "") {
          next
        }
        mapped_cols <- c(mapped_cols, colnames(df)[i])
        colnames(df)[i] <- map$std_header[j]
      }
    }
  }
  
  # Store unmapped variables
  unmapped_cols <- setdiff(colnames(df0), mapped_cols)

  # Map codes
  df <- map_codes(df, map)
  
  # Convert units
  for (i in seq_along(colnames(df))) {
    for (j in 1:nrow(map)){
      
      if (colnames(df)[i] == map$std_header[j]){
        if (is.na(map$std_header[j]) |
            map$std_unit[j] %in% c("","date","code","text","yyyy")) {
          next
        }
        df[[i]] <- convert_unit(df[[i]], map$column_unit[j], map$std_unit[j])
      }
    }
  }
  
  # Drop columns not in standard if required
  if (keep_unmapped) {
    df <- distinct(df[, !colnames(df) %in% col_exempt])
  } else {
    df <- distinct(df[, !colnames(df) %in% setdiff(unmapped_cols, col_exempt)])
  }
  
  return(df)
}


