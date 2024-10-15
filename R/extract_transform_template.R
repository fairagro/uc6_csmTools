#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

# Extract data
drop_artefacts <- function(df) {
  colnames(df) <- ifelse(is.na(colnames(df)), "unnamed", colnames(df))  # handles NA columns
  colnames(df) <- make.names(colnames(df), unique = TRUE, allow_ = TRUE)
  df <- df[ , !grepl("^unnamed", colnames(df))]
  #df <- df %>% select_if(function(col) !all(is.na(col)))
  df <- df %>% filter(rowSums(is.na(.)) != ncol(.))
  return(df)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'
#'
get_template_codes <- function(wb){
  
  codes <- wb_to_df(wb, sheet = "DropDown", startRow = 2)
  codes <- codes %>% select(-ends_with("_sort"))  # drop sorted column (not aligned with codes; only for data entry)
  
  codes_prefixes <- unique(sub("_.*", "", names(codes)))
  
  # split into separate dataframe for each code list
  codes_ls <- lapply(codes_prefixes, function(prefix) {
    cols <- grep(paste0("^", prefix, "(_|$)"), names(codes), value = TRUE)
    df <- codes %>% select(all_of(cols))
    
    if (ncol(df) >= 2) {
      names(df)[1:2] <- c("desc", "code")
    } else if (ncol(df) == 1) {
      names(df)[1] <- "desc"
    }
    
    df <- df %>% filter(!is.na(.[[1]]))
    
    return(df)
  })
  names(codes_ls) <- codes_prefixes
  
  codes_ls <- codes_ls[!names(codes_ls) %in% "country"]
  #codes_ls$country <- codes_df$country %>% mutate(code = countrycode(desc, origin = "country.name", destination = "genc2c"))
  return(codes_ls)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

extract_template <- function(path){
  
  wb <- suppressWarnings(wb_load(path))
  nms <- wb_get_sheet_names(wb)
  nms_data <- nms[grepl("^[A-Z_]+$", nms)]
  
  # Get all data sections as data frames  
  input <- lapply(nms_data, function(sheet) {
    df <- wb_to_df(wb, sheet = sheet, startRow = 4)
    drop_artefacts(df)  # Drop empty rows and artefacts columns
  })
  names(input) <- nms_data
  
  #
  codes <- get_template_codes(wb)
  
  # Delete empty dataframes
  # TODO: quality check if mandatory data is input
  out <- input[!sapply(input, function(df) all(dim(df) == c(0, 0)) || all(is.na(df)))]
  attr(out, "codes") <- codes
  
  # TEMP adjust problematic header
  out[["FERTILIZERS"]] <- out[["FERTILIZERS"]] %>% rename(`fert_#_tot` = fert_._tot)  ### tmp
  
  # Get all codes for categorical variables
  return(out)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

desc_to_codes <- function(df, codes) {
  
  # TODO: homogenize with existing recoding function
  
  # Iterate through each column of the data df
  for (col in names(df)) {
    # Check if the column is categorical (character or factor)
    if (is.character(df[[col]]) || is.factor(df[[col]])) {
      # Iterate through each dataframe in the code list
      for (code_df in codes) {
        # Check if the column contains any of the variable names in the code dataframe
        if (any(df[[col]] %in% code_df$desc)) {
          # Replace the descriptive names with the corresponding codes
          
          if (length(df[[col]]) == 0) {
            message("Column ", col, " is empty.")
          } else {
            df[[col]] <- vapply(df[[col]], function(x) {
              match_index <- match(x, code_df$desc)
              if (!is.na(match_index)) {
                as.character(code_df$code[match_index])
              } else {
                as.character(x)
              }
            }, FUN.VALUE = character(1))
          }
        }
      }
    }
  }
  return(df)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

icasa_long_to_short <- function(df, keep_unmapped = TRUE){

  map <- load_map( # TODO: make internal data object
    "C:/Users/bmlle/Documents/0_DATA/TUM/HEF/FAIRagro/2-UseCases/UC6_IntegratedModeling/Workflows/csmTools/data/icasa_mappings.csv"
  ) %>%
    select(icasa_header_long, icasa_header_short) %>%
    distinct()
  
  rename_vector <- setNames(as.character(map$icasa_header_short), map$icasa_header_long)
  
  rename_vector <- rename_vector[names(rename_vector) %in% names(df)]
  df <- df %>% rename_with(~ rename_vector[.x], .cols = names(rename_vector))

  if (keep_unmapped == FALSE) df <- df[colnames(df) %in% rename_vector]
  
  return(df)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

format_treatment_matrix <- function(ls){
  
  if("TREATMENTS" %in% names(ls)){
    df <- ls[["TREATMENTS"]]
  } else {
    error("Treatment matrix not found.")
  }
  
  df <- df %>%
    mutate(across(where(is.character), ~ sub("\\|.*", "", .)),
           across(everything(), ~ trimws(., which = "left")),
           simulation_control_level = ifelse(is.na(simulation_control_level), 1, simulation_control_level),
           across(genotype_level_number:mulch_level, ~ifelse(is.na(.x), 0, .x)),
           across(c(treatment_number, genotype_level_number:mulch_level), ~as.numeric(.)))
  
  ls[["TREATMENTS"]] <- df
  
  return(ls)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

format_observed_data <- function(ls){
  
  merge_obs <- function(section_nm) {
    
    pattern <- paste0(section_nm, "|OBS_DATA_LINKS")
    section <- ls[grepl(pattern, names(ls))]
    
    section <- Reduce(function(x, y) left_join(x, y, by = c("EXPER_ID", "TREAT_ID")), c(section[1], section[-1]))
    if (identical(colnames(section), colnames(ls$OBS_DATA_LINKS))) { 
      return(NULL) 
    } else { 
      return(section) 
    }
  }
    
    ls$SUMMARY <- merge_obs("SUMMARY")
    ls$TIME_SERIES <- merge_obs("TIME_SER")
    
    ls <- ls[!grepl("GROWTH|DEV|OBS", names(ls))]
    
    return(ls)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

template_to_icasa <- function(ls) {
  
  ls <- lapply(ls, function(df){ desc_to_codes(df, codes = attributes(ls)$codes) })  # map template description to icasa codes
  ls <- format_treatment_matrix(ls)  # format treatment matrix
  ls <- lapply(ls, function(df){ icasa_long_to_short(df, keep_unmapped = FALSE)})  # map headers to long format
  ls <- format_observed_data(ls)  # merged observations to measurements
  
  return(ls)
}

#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

# Transform to DSSAT
template_icasa_to_dssat <- function(ls){
  
  ls <- template_icasa
  
  # Map data from ICASA to DSSAT
  # TODO: integrate into metadata and section mapping routine (no wrapper)
  ls <- lapply(ls, function(df){ 
    map_data(df, input_model = "icasa", output_model = "dssat", map = load_map(map_path), keep_unmapped = FALSE, col_exempt = NULL)
  })
  map_sections <- function(vector, map, direction = c("from_icasa", "to_icasa"), output_model = "DSSAT") {
    
    # TEMP map, to integrate in main map
    map <- data.frame(
      section_in = c("EXP_METADATA", "PLOT_DETAILS", "PERSONS", "INSTITUTIONS", 
                     "TILLAGE", "TILLAGE_EVENTS","GENOTYPES", "FIELDS", "INITIAL_CONDITIONS", "INITIAL_COND_LAYERS",
                     "PLANTINGS", "FERTILIZERS", "FERTILIZER_APPLICS","HARVESTS", "HARVEST_EVENTS", "TREATMENTS",
                     "OBS_DATA_LINKS", "SUMMARY_GROWTH", "SUMMARY_DEV", "TIME_SER", "TIME_SER_DEV"),
      section_out = c("GENERAL", "GENERAL", "GENERAL", "GENERAL", 
                      "TILLAGE", "TILLAGE", "CULTIVARS", "FIELDS", "INITIAL_CONDITIONS", "INITIAL_CONDITIONS", 
                      "PLANTING_DETAILS", "FERTILIZERS", "FERTILIZERS", "HARVEST", "HARVEST", "TREATMENTS",
                      "OBS_DATA_LINKS", "SUMMARY", "SUMMARY", "TIME_SERIES", "TIME_SERIES")
    )
    
    # if (direction == "to_icasa"){ # !!run only after mapping headers!
    #   map <- map %>% rename(header_in = icasa_header_short, header_out = header, unit_in = unit, unit_out = icasa_unit)  
    # } else if (direction == "from_icasa"){
    #   map <- map %>% rename(header_in = header, header_out = icasa_header_short, unit_in = icasa_unit, unit_out = unit)
    # }
    
    # Create a named vector for easy lookup
    rename_map <- setNames(map$section_out, map$section_in)
    
    # Rename elements of the input vector using the lookup table
    renamed_vector <- sapply(vector, function(x) {
      if (x %in% names(rename_map)) {
        rename_map[x]
      } else {
        x
      }
    })
    
    names(renamed_vector) <- NULL
    return(renamed_vector)
  }
  names(ls) <- map_sections(vector = names(ls))
  
  combine_dual_tier <- function(ls) {
    
    unique_names <- unique(names(ls))  # find unique names
    
    out <- list()
    for (name in unique_names) {
      
      section_dfs <- ls[names(ls) == name]  # filter dataframes with the same name
      common_cols <- Reduce(intersect, lapply(section_dfs, colnames))
      
      joined_dfs <- Reduce(function(x, y) merge(x, y, by = common_cols, all = TRUE), section_dfs)  # merged sections
      out[[name]] <- joined_dfs
    }
    return(out)
  }
  ls <- combine_dual_tier(ls)

  ls <- fmt_metadata(ls, model = "dssat", section = "management")
  return(ls)
}


#' ###
#' 
#' 
#' @param df ###
#' 
#' @return a data frame ###
#' 
#' @importFrom ###
#'

# NB: fails when applied to the mapped dataset with unmapped variables kept, due to duplicate name in multiple table (ELEV.x, ELEV.y)
split_experiments <- function(ls, split_var = "EID") {
  
  # ls <- input_mapped
  # split_var <- "EID"
  
  
  join_field_env <- function(subset_name, id_col) {
    subset <- ls[grepl(subset_name, names(ls))]
    subset_nms <- names(subset)
    subset_cols <- lapply(subset, colnames)
    
    split_vars <- sapply(subset, function(df) {
      colnames(df)[sapply(df, function(col) all(col %in% unique(ls$FIELDS[[id_col]])))]
    })
    
    subset_exp <- lapply(subset_nms, function(nm) {
      df <- subset[[nm]]
      join_col <- split_vars[[nm]]
      df <- df %>% left_join(ls$FIELDS, by = setNames(id_col, join_col))
      return(df)
    })
    names(subset_exp) <- subset_nms
    
    list(subset_exp = subset_exp, subset_cols = subset_cols)
  }
  
  if (any(grepl("FIELDS", names(ls)))) {
    if (any(grepl("SOIL", names(ls)))) {
      soil_result <- join_field_env("SOIL", "SOIL_SUBSET")
    }
    if (any(grepl("WEAT", names(ls)))) {
      wth_result <- join_field_env("WEAT", "WTH_SUBSET")
    }
    ls <- c(ls[!grepl("SOIL|WEAT", names(ls))], soil_result$subset_exp, wth_result$subset_exp)
  }
  
  experiments <- lapply(ls, function(df) {
    split(df, f = df[[split_var]])
  })
  
  if (any(grepl("SOIL|WEAT", names(ls)))) {
    if (!is.null(soil_result$subset_cols)) {
      soil_exp_sub <- lapply(names(soil_result$subset_exp), function(nm) {
        exp_list <- experiments[[nm]]
        for (i in seq_along(exp_list)) {
          exp_list[[i]] <- exp_list[[i]] %>% select(all_of(soil_result$subset_cols[[nm]]))
        }
        return(exp_list)
      })
      names(soil_exp_sub) <- names(soil_result$subset_exp)
    }
    
    if (!is.null(wth_result$subset_cols)) {
      wth_exp_sub <- lapply(names(wth_result$subset_exp), function(nm) {
        exp_list <- experiments[[nm]]
        for (i in seq_along(exp_list)) {
          exp_list[[i]] <- exp_list[[i]] %>% select(all_of(wth_result$subset_cols[[nm]]))
        }
        return(exp_list)
      })
      names(wth_exp_sub) <- names(wth_result$subset_exp)
    }
    experiments <- c(experiments[!grepl("WEAT|SOIL", names(ls))], soil_exp_sub, wth_exp_sub)
  }
  
  experiments <- revert_list_str(experiments)
  return(experiments)
}
