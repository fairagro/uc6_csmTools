# BIN After 2) DATA tables in original script
#

# ---- 3) DESIGN tables ----------------------------------------------------

# Merge design tables with their attributes
DESIGN_all_tbls <- merge_tbls(DESIGN_all_tbls, ATTR_tbls, type = "child-parent", drop_keys = TRUE)



# Assemble experiments table ----------------------------------------------


# Merge design tables into a single dataframe (keep join keys)
DESIGN <- merge_tbls(db_list[DESIGN_tbl_nm], DESIGN_all_tbls, type = "child-parent", drop_keys = FALSE)[[1]]

# Format provenance and identification metadata
EXP_NAME = metadata$Identification$title

PEOPLE <- metadata$Provenance %>% mutate(address = paste0(organisation, ", ", street, ", ", postal_code, " ", city))
PERSONS = paste(PEOPLE$persons, collapse = "; ")
ADDRESS = paste(PEOPLE$address, collapse = "; ")

DOI = metadata$Identification$doi
## TODO: add publication info based on sepcified repo in function call (repo, date, version) 

# Compile EXPERIMENTS table: contains all metadata attributes globally applicable within year
EXP_tbl <- merge_tbls(db_list_ipt[DESIGN_tbl_nm],
                      DESIGN_all_tbls[!names(DESIGN_all_tbls) %in% attr(TREATMENTS_tbl, "tbl_name")],
                      type = "child-parent", drop_keys = TRUE)[[1]]

DESIGN_pkeys <- unlist(lapply(DESIGN_all_tbls, function(df) get_pkeys(df, alternates = FALSE)), use.names = FALSE)

EXPERIMENTS <- EXP_tbl %>%
  dplyr::select(-any_of(c(PLOTS_all_ids, DESIGN_pkeys, REPS_nm))) %>%
  distinct() %>%
  mutate(EXP_NAME = EXP_NAME,
         PERSONS = PERSONS,
         ADDRESS = ADDRESS,
         COUNTRY = "Germany",  # TODO: deduce from spatial coverage
         DOI = DOI) %>%
  relocate(c(EXP_NAME, PERSONS, ADDRESS, DOI), .before = everything())

# Check if spatial data is in the table using bounding box data
EXP_cols <- names(EXPERIMENTS)

for (col_name in EXP_cols) { ##!! is not fool proof
  
  if (all(EXPERIMENTS[[col_name]] >= metadata$Spatial_coverage[1,1] & 
          EXPERIMENTS[[col_name]] <= metadata$Spatial_coverage[2,1])) {
    names(EXPERIMENTS)[names(EXPERIMENTS) == col_name] <- "FL_LON"
  }
  
  if (all(EXPERIMENTS[[col_name]] >= metadata$Spatial_coverage[1,2] & 
          EXPERIMENTS[[col_name]] <= metadata$Spatial_coverage[2,2])) {
    names(EXPERIMENTS)[names(EXPERIMENTS) == col_name] <- "FL_LAT"
  }
}

# If not in table, replace by the mean of the bounding box x and y boundaries
# Whether this is accurate depends on how the bouding box is defined, and only if there is one field
if(!"FL_LON" %in% names(EXPERIMENTS)){
  EXPERIMENTS[["FL_LON"]] = mean(metadata$Spatial_coverage[,1])
}

if(!"FL_LAT" %in% names(EXPERIMENTS)){
  EXPERIMENTS[["FL_LAT"]] = mean(metadata$Spatial_coverage[,2])
}



# FILTER YEAR/CROP FUNCTION (ALSO ADJUST IDs)
select_year <- function(db, trt_matrix, year_col, year){
  
  db <- BNR_MANAGEMENT_out
  years_col <- "Versuchsjahr"
  year = 1995
  trt_matrix <- TREATMENTS
  
  ids <- c()
  db_id <- list()
  for (i in names(db)){
    
    df <- db[[i]]
    df_id <- df %>% filter(get(years_col) == year)
    
    id <- df_id[1]
    id_nm <- names(id)
    min_id <- min(id[id > 0])
    
    # Update IDs
    db_id[[i]] <- df_id %>% mutate(!!id_nm := ifelse(get(id_nm) > 1, get(id_nm) - (min_id-1), get(id_nm)))
    ids[i] <- id_nm
  }
  
  trt_matrix <- trt_matrix %>%
    filter(get(years_col) == year) %>%
    mutate(across(as.character(ids), ~ifelse(.x > 1, .x - (min(.x)-1), .x)))
}



# FORMER SECTION OUTPUT vMAPPER -------------------------------------------

# # File output -------------------------------------------------------
# 
# # Split crop experiment data by year
# if (multiyear){
#   
#   # Drop metadata common to all years
#   BNR_mapped_ipt <- BNR_mapped[!names(BNR_mapped) %in% c("GENERAL", "FIELDS")]  #!!
#   
#   # Split data by year
#   BNR_mapped_yr <- lapply(BNR_mapped_ipt, function(df) split(df, f = df[["Year"]]))
#   BNR_mapped_yr <- revert_list_str(BNR_mapped_yr)
#   names(BNR_mapped_yr) <- paste0("Y", names(BNR_mapped_yr))
#   
#   # Remove empty data frames, e.g., management categories irrelevant for the focal year
#   BNR_mapped_yr <- lapply(BNR_mapped_yr, function(ls) ls[lengths(ls) > 0])
#   
#   # Remove columns with only NAs in each data frame
#   BNR_mapped_yr <- lapply(BNR_mapped_yr, function(ls){
#     ls <- lapply(ls, function(df) {
#       df[, colSums(is.na(df)) != nrow(df)]
#     })
#   })
#   
#   # Reset management IDs
#   mngt_df_nms <- names(BNR_mapped[grepl("MANAGEMENT", names(BNR_mapped))])
#   mngt_ls <- BNR_mapped[names(BNR_mapped) %in% mngt_df_nms]
#   mngt_ids <- unique(unlist(lapply(mngt_ls, function(df) colnames(df)[1]), use.names = FALSE))
#   
#   reset_id <- function(df, id_col) {
#     
#     id <- df[[id_col]]
#     suppressWarnings(
#       min_id <- min(id[id != 0])
#     )
#     
#     id[id > 0] <- id[id > 0] - min_id + 1
#     df[[id_col]] <- id
#     
#     return(df)
#   } #!!
#   
#   BNR_mapped_yr <- lapply(BNR_mapped_yr, function(ls) {
#     
#     # Reset management IDs in treatment matrix
#     for (i in colnames(ls[["TREATMENTS"]])) {
#       if (i %in% mngt_ids) {
#         ls[["TREATMENTS"]] <- reset_id(ls[["TREATMENTS"]], i)
#       }
#     }
#     
#     # Reset management IDs in management data franes
#     ls_mngt <- ls[names(ls) %in% mngt_df_nms]
#     ls_rest <- ls[!names(ls) %in% mngt_df_nms]
#     
#     ls_mngt <- lapply(ls_mngt, function(df) {
#       for (i in colnames(df)) {
#         if (i %in% mngt_ids) {
#           df <- reset_id(df, i)
#         }
#       }
#       return(df)
#     })
#     
#     return(c(ls_rest, ls_mngt))
#   })
#   
# }
# 
# # Append soil to each year (single profile for now)
# BNR_mapped_yr <- lapply(BNR_mapped_yr, function(x)
#   append(x, list(GENERAL = GENERAL, FIELDS = FIELDS, SOIL_Header = SOIL_Header, SOIL_Layers = SOIL_Layers))
# )
# 
# # Output to vMapper and AGMIP translator ----------------------------
# 
# 
# # Create a folder for each year (i.e., sublist) and export the dataframes as csv files
# dir.create(paste0("./inst/extdata/", db_name, "/1_out"))  # TODO: replace by datapath
# 
# for (i in names(BNR_mapped_yr)) {
#   dir.create(paste0("./inst/extdata/", db_name, "/1_out/", i))
#   
#   for (j in names(BNR_mapped_yr[[i]])) {
#     write.csv(BNR_mapped_yr[[i]][[j]], file = paste0("./inst/extdata/", db_name, "/1_out/", i, "/", j, ".csv"),
#               row.names = FALSE,
#               fileEncoding = "latin1")
#   }
# }

# Group all folders in a zip file
##zipr("data.zip", files = list.dirs(paste0("./data/", db_name), recursive = FALSE), recurse = TRUE)

# CHOOSE YEAR FOR SIMU: wheat, full weather, 1995 No OM application on WHT years!
# VMAPPER EXPORT AND CONVERT TO DSSAT


# PROBLEMS vMAPPER:
## IRRIGATION 0/1 INCREASES NR OF TRT, BUT TRTNO NOT UPDATED
## CULTIVARS AND CROP TOGETHER IN SAME TABLE??
## vMapper does not handle replicates: treatments replicated as are measured data
## German charcters are not handled
## Initial conditions not included?
## Try with package DSSAT

# Load vMapper
#browseURL("https://data.agmip.org/ardn/tools/vmapper")
#' Load all csv files in the "Raw Data Files" field
#' Set header raw to 1 and leave data start to 2 for each table
#' Confirm units/codes on all variables appearing in red (they are properly mapped already)
#' Exception Year: only map to EX_YEAR in EXPERIMENTS data sheet, untick on all other tables
#' Set relationships
#' ####
#' 
#browseURL("https://data.agmip.org/ardn/tools/data_factory")
