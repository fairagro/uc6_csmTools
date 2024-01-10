#' Example of UC6 ETL pipeline with LTE data from the Seehausen experiment, sourced from the BonaRes repository
#' 
#' Some parts of the pipeline are handled by generic functions, others are specific to the Seehausen LTE as a
#' temporary workarounds (most notably data mapping steps).
#' Additional generic worfklows will be developed at a later stage with adapted standards to handle complex 
#' mappings.
#' 
#' @importFrom stringr str_extract
#' @importFrom readxl read_excel
#' @importFrom dplyr "%>%" select mutate relocate group_by arrange bind_rows filter if_all dense_rank desc ungroup
#' @importFrom dplyr cur_group_id distinct where across left_join
#' @importFrom tidyr all_of everything separate starts_with unnest
#' @importFrom tibble as_tibble
#' @importFrom lubridate as_date year
#' @importFrom DSSAT read_sol write_sol write_filex write_filea write_wth write_dssbatch run_dssat read_output
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline labs guides guide_legend theme_bw theme
#' @importFrom ggplot2 scale_colour_manual scale_size_manual scale_linewidth_manual
#' @importFrom ggplot2 element_text element_blank
#' 

# Load libraries ----------------------------------------------------------

# Only run if libraries are not locally installed or to update them (NB: all at once!)
#source("./inst/install_libraries.R")

# Load libraries, internal data (e.g., vocabularies and maps), and internal functions
source("./inst/load_libraries.R")


# Load data ---------------------------------------------------------------


db_files <- list.files(path = "./inst/extdata/lte_seehausen/0_raw", pattern = "\\.csv$")
db_paths <- sapply(db_files, function(x){ paste0("./inst/extdata/lte_seehausen/0_raw/", x) })
db_list <- lapply(db_paths, function(x) { file <- read.csv(x, fileEncoding = "iso-8859-1") })
# TODO: check best encoding, iso-8859-1 vs. latin1

# Simplify names
names(db_list) <- str_extract(names(db_list), "_V[0-9]+_[0-9]+_(.*)") # cut names after version number
names(db_list) <- gsub("_V[0-9]+_[0-9]+_", "", names(db_list)) # drop version number
names(db_list) <- sub("\\..*$", "", names(db_list)) # drop file extension

#
db_name <- unique(gsub("\\..*$", "", db_files))



# Get metadata ------------------------------------------------------------


# metadata <- read_metadata(
#   path = "https://maps.bonares.de/finder/resources/dataform/xml/8dbfffb7-80eb-4d36-9f83-5d080248ff8c",
#   repo = "bnr",
#   sheet = NULL)
# 
# as of 21.12.2023, BonaRes repo servers appears to be down
# 'Error in open.connection(x, "rb") : Received HTTP/0.9 when not allowed'
# '503 Service Unavailable' in browser
# temp workaround code:
#
# In addition: the metadata available in the xml file is somewhat irrelevant (e.g., huge bounding box,
# spatial data reported appears to be more related to contact person location that the field experiment site)
# The metdata provided at https://tools.bonares.de/ltfe/lte-details/344/ is much more helpful but currently
# not directly exploitable in R (no download link for the excel metadata file).
# Manual download is currently used. The read_metadata function will be updated once the data records are restored
# on the BonaRes repo server and the metadata updated.
#
# metadata_xml <- list(
#   Identification = 
#     data.frame(doi = "https://doi.org/10.20387/bonares-3nqn-41vn",
#                title = "Long Term Experiment Seehausen: Duengungs-Kombinationsversuch Seehausen (F1-70)",
#                year = ),
#   Provenance =
#     data.frame(contact_name = c("Kurt-Jürgen Hülsbergen", "Robert Oliver Simon", "BonaRes Data Centre"),
#                #contact_email = c("###", "###", "###")
#                trial_institution = c("TU Munich", "IU Internationale Hochschule GmbH", "Leibniz Centre for Agricultural Landscape Research (ZALF)"),
#                XMLposition = c("Lehrstuhl für Ökologischen Landbau und Pflanzenbausysteme", NA, "Research Platform 'Data Analysis and Simulation' - WG Geodata"),
#                XMLstreet = c("Liesel-Beckmann Str. 2", "Juri-Gargarin-Ring 152", "Eberswalder Strasse 84"),
#                XMLcity = c("Freising", "Erfurt", "Müncheberg"),
#                XMLpostal_code = c(85354, 99084, 15374)),
#   Spatial_coverage =
#     data.frame(site = "Seehausen",
#                country = "Germany",
#                longitude = 12.41893,
#                latitude = 51.40188),
#   Temporal_coverage = 
#     data.frame(start_date = "1967",
#                status = "2002")
# )

metadata <- read_excel("./inst/extdata/lte_seehausen/0_raw/lte_seehausen_xls_metadata.xls")



# Crop experiment dataset identification and reshaping --------------


seehausen_fmt <- reshape_exp_data(db = db_list, metadata = metadata, mother_tbl = db_list$VERSUCHSAUFBAU)

# Advanced data transformation --------------------------------------

#' Files that are currently used for data mapping only handle exact matches between variables and unit conversion.
#' Producing advanced mapping standards and functions to handles more complex transformations (e.g., concatenation of
#' n columns, conditional variable naming, etc.) will be the main focus in early 2024. For now, we used the example
#' dataset to retrieve and manually transform the data. The information collected in such examples will be exploited
#' to design the mapping standards and functions. These specific adjustments are provided below with descriptions.

# ==== GENERAL section ----------------------------------------------------


GENERAL <- seehausen_fmt$GENERAL %>%
  mutate(SITE_NAME = paste(SITE, COUNTRY, sep = ", "))



# ==== FIELDS table -------------------------------------------------------


FIELDS <- seehausen_fmt$FIELDS %>%
  mutate(FLELE = (PARZELLE.Hoehenlage_Min+PARZELLE.Hoehenlage_Max)/2) %>%  #? Make mutate fun that replaces components
  mutate(SOIL_ID = "IB00000001",  # Currently generic soil is used
         WEATHER_ID = "SEDE")  # Institute + Site: TU Munich, Muenchenberg
  


# ==== INITIAL CONDITIONS tables ------------------------------------------

INITIAL_CONDITIONS <- seehausen_fmt$OTHER_FRUCHTFOLGE %>% arrange(seehausen_fmt$OTHER_FRUCHTFOLGE, Year)

INITIAL_CONDITIONS$ICPCR <- NA
for (i in 2:nrow(INITIAL_CONDITIONS)) {
  if (INITIAL_CONDITIONS[["Year"]][i] - 1 == INITIAL_CONDITIONS[["Year"]][i-1]) {
    INITIAL_CONDITIONS$ICPCR[i] <- INITIAL_CONDITIONS$KULTUR.Kultur_Englisch[i-1]
  }
}

INITIAL_CONDITIONS <- INITIAL_CONDITIONS %>%
  group_by(Year, ICPCR) %>%
  mutate(IC_ID = cur_group_id()) %>% ungroup() %>%
  select(IC_ID, Year, KULTUR.Kultur_Englisch, ICPCR) %>%
  arrange(IC_ID)
  

# ==== HARVEST table ------------------------------------------------------

HARVEST <- bind_rows(
  seehausen_fmt$OBSERVED_TimeSeries %>%
    select(Year, Plot_id, TRTNO, starts_with(c("ERNTE","TECHNIK"))),
  seehausen_fmt$OBSERVED_Summary %>%
    select(Year, Plot_id, TRTNO, starts_with(c("ERNTE","TECHNIK")))
) %>%
  # Drop all records with only NAs in the harvest categories
  # Those were created when splitting the BNR Harvest table into Observed summary and time series 
  # and then merging with observed data from other tables
  filter(!if_all(all_of(setdiff(names(.), c("Plot_id", "Year", "TRTNO"))), is.na)) %>%
  # Rank different date within year and treatment in decreasing order to separate i-s and e-o-s harvests
  # Currently assumes that all plots have been harvested e-o-s.
  # TODO: add a crop specific control to check whether dates are realistic?
  group_by(Plot_id, Year, TRTNO) %>%
  mutate(HA_type = ifelse(
    dense_rank(desc(as_date(ERNTE.Termin))) > 1, "is", "eos")) %>% ungroup() %>%
  #mutate(HA_type = ifelse(ymd(ERNTE.Termin) == max(ymd(ERNTE.Termin)), "eos", "is")) %>%  # alternative
  # Keep only latest harvest date ("actual harvest")
  filter(HA_type == "eos") %>%
  # Drop Harvest sorting variable and keep unique records
  select(-c(Plot_id, TRTNO, HA_type)) %>%
  distinct() %>%
  # Generate harvest ID
  group_by(Year) %>%
  mutate(HA_ID = cur_group_id()) %>% ungroup() %>%
  relocate(HA_ID, .before = everything())  # split for updated matrix and mngt only



# ==== FERTILIZERS table --------------------------------------------------

# FERTILIZERS and ORGANIC_MATERIALS tables
FERTILIZERS_join <- seehausen_fmt$DUENGUNG %>%
  # Separate inorganic and organic fertilizers
  filter(DUENGUNG.Mineralisch == 1) %>%
  separate(DU_ID, into = c("OM_ID", "FE_ID"), remove = FALSE, sep = "_") %>%
  # Update the ID accordingly
  group_by(Year, FE_ID) %>% mutate(FE_ID = cur_group_id()) %>% ungroup() %>%
  # Drop unused columns
  select(c(where(~!all(is.na(.))), -DUENGUNG.Mineralisch, -DUENGUNG.Organisch, -DUENGUNG.Gesamt_Stickstoff, -OM_ID)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x)))

FERTILIZERS <- FERTILIZERS_join %>%
  select(-DU_ID) %>%
  arrange(FE_ID) %>%
  distinct()


# ==== ORGANIC_MATERIALS table --------------------------------------------

#' NB: one challenge here is that OM is applied only every 2-4 years, though it is considered a treatment for the 2-4 years
#' following application. For modelling this should be somewhat reflected into the initial conditions in the years with no
#' application. For now we just consider the OM only on the year when it is applied, which will lead to innacurate model
#' predictions in the years between applications

ORGANIC_MATERIALS_join <- seehausen_fmt$DUENGUNG %>%
  # Separate inorganic and organic fertilizers
  filter(DUENGUNG.Organisch == 1) %>%
  separate(DU_ID, into = c("OM_ID", "FE_ID"), remove = FALSE, sep = "_") %>%
  # Update the ID accordingly
  group_by(Year, OM_ID) %>% mutate(OM_ID = cur_group_id()) %>% ungroup() %>%
  # Calculate the amount of OM applied in each year based on average nitrogen concentration
  # source: https://www.epa.gov/nutrientpollution/estimated-animal-agriculture-nitrogen-and-phosphorus-manure
  # NB: this is a US estimate, we might need to add a routine to estimate based on experiment metadata
  mutate(OMNPC = 3,  # OM nitrogen concentration (3%)
         OMAMT = DUENGUNG.Stickstoff_org * OMNPC * 0.01) %>%
  # Drop unused columns
  select(c(where(~!all(is.na(.))), -DUENGUNG.Mineralisch, -DUENGUNG.Organisch, -DUENGUNG.Gesamt_Stickstoff, -FE_ID)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x)))

ORGANIC_MATERIALS <- ORGANIC_MATERIALS_join %>%
  select(-DU_ID) %>%
  arrange(OM_ID) %>%
  distinct()


# ==== CULTIVARS table ----------------------------------------------------

CULTIVARS <- seehausen_fmt$AUSSAAT %>% 
  select(Year, starts_with(c("SORTE","KULTUR"))) %>% ## TODO: not only update ID by year but also by crop
  distinct() %>%
  # Generate cultivar ID
  group_by(Year) %>%
  mutate(CU_ID = cur_group_id()) %>% ungroup() %>%
  relocate(CU_ID, .before = everything()) # split for updated matrix and mngt only


# ==== PLANTINGS table ----------------------------------------------------

# AUSSAAT.Keimfaehige_Koerner has variable units depending on crop
POT_years <- unique(INITIAL_CONDITIONS[which(INITIAL_CONDITIONS$KULTUR.Kultur_Englisch == "Potato"), "Year"])

PLANTINGS <- seehausen_fmt$AUSSAAT %>%
  select(-starts_with("SORTE")) %>%
  mutate(AUSSAAT.Keimfaehige_Koerner = ifelse(Year %in% POT_years,
                                              AUSSAAT.Keimfaehige_Koerner * 0.0001, AUSSAAT.Keimfaehige_Koerner),
         PLMA = "S",
         PLDS = "R")


# ==== TREATMENTS matrix --------------------------------------------------

TREATMENTS <- seehausen_fmt$TREATMENTS %>%
  left_join(INITIAL_CONDITIONS %>% select(IC_ID, Year), by = "Year") %>%
  left_join(HARVEST %>% select(HA_ID, Year), by = "Year") %>%
  left_join(CULTIVARS %>% select(CU_ID, Year), by = "Year") %>%
  left_join(FERTILIZERS_join %>% select(FE_ID, DU_ID, Year), by = c("DU_ID", "Year"))  %>%
  left_join(ORGANIC_MATERIALS_join %>% select(OM_ID, DU_ID, Year), by = c("DU_ID", "Year")) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>%
  select(-DU_ID) %>%
  # Add treatment name (concatenate both factors)
  # Should be handled in is_treatment function in the future
  mutate(TRT_NAME = paste0(Faktor1_Stufe_ID, " | ", Faktor2_Stufe_ID)) %>%
  relocate(TRT_NAME, .after = "Year") %>%
  distinct()


# ==== OBSERVED_TimeSeries table ------------------------------------------

OBSERVED_TimeSeries <- seehausen_fmt$OBSERVED_TimeSeries %>%
  # Rank different date within year and treatment in decreasing order to separate i-s and e-o-s harvests
  # as different variables characterize is and eos harvests in icasa
  group_by(Year, TRTNO) %>%
  mutate(HA_type = ifelse(
    dense_rank(desc(as_date(ERNTE.Termin))) > 1, "is", "eos")) %>% ungroup() %>%
  relocate(HA_type, .before = everything())


# ==== OBSERVED_Summary table --------------------------------------------

#' Observed summary data is (currently?) not fully exploitable, as data collection dates are missing for the different
#' analyses (soil and plant samples). For example, soil N content is provided for some years but without sampling 
#' dates, it is not possible to determine whether this corresponds to initial conditions (before the growing season)
#' or to in-season measurements to control the influence of the fertilization treatments, and therefore not possible
#' to assign it to the adequate ICASA section (INITIAL CONDITIONS / SOIL ANALYSES).
#' Perhaps the missing information can be retrieved from the metadata or associated publications?

OBSERVED_Summary <- seehausen_fmt$OBSERVED_Summary 



# Mapping to ICASA --------------------------------------------------------

BNR_full <- list(GENERAL = GENERAL,
                 FIELDS = FIELDS,
                 TREATMENTS = TREATMENTS,
                 INITIAL_CONDITIONS = INITIAL_CONDITIONS,
                 TILLAGE = seehausen_fmt$BODENBEARBEITUNG,
                 PLANTING_DETAILS = PLANTINGS,
                 CULTIVARS = CULTIVARS,
                 FERTILIZERS = FERTILIZERS,
                 RESIDUES = ORGANIC_MATERIALS, 
                 IRRIGATION = seehausen_fmt$BEREGNUNG,
                 CHEMICALS = seehausen_fmt$PFLANZENSCHUTZ,
                 HARVEST = HARVEST,
                 OBSERVED_Summary = OBSERVED_Summary,
                 OBSERVED_TimeSeries = OBSERVED_TimeSeries)

# Transfer metadata attributes to new dataframe
attr(BNR_full, "EXP_DETAILS") <- attr(seehausen_fmt, "EXP_DETAILS")
attr(BNR_full, "SITE_CODE") <- attr(seehausen_fmt, "SITE_CODE")


# Apply mappings (currently, only exaxt matches headers, codes and unit conversions)
BNR_mapped <- BNR_full
for (i in seq_along(names(BNR_full))) {
  BNR_mapped[[i]] <- map_data(df = BNR_full[[i]],
                              tbl_name = names(BNR_full)[i],
                              map = bnr_seehausen_icasa,
                              keep_unmapped = FALSE,
                              col_exempt = "Year")
}

# TODO: mapping to integrate somewhat into reshaping for observed data?
# Some data may be identified as time series but no longer qualified after mapping if the variable
# causing the time series tag is not mappable into ICASA.
# Other reason: depending on crops, certain variable will be mapped to different ICASA vars (e.g., yield)
# Challenge: would require a function that can identify different types of observed data
# e.g., plant phenology, growth, yield, soil measurements, nitrogen... AI probably required
# Alternatives are post-hoc corrections (tedious)


# Download and format corresponding weather data --------------------

WEATHER_raw  <- get_weather(
  lat = unique(FIELDS$FL_LAT),
  lon = unique(FIELDS$FL_LON),
  years = sort(unique(TREATMENTS[["Year"]])),
  src = "dwd",
  map_to = "icasa",
  vars = c("air_temperature", "precipitation", "solar_radiation", "dewpoint", "relative_humidity", "wind_speed"),
  res = list("hourly", c("daily", "hourly"), c("daily", "hourly"), "hourly", "hourly", "hourly") ,
  max_radius = c(50, 10, 50, 20, 20, 20)
)

#' This might take 5-10 minutes to run
#' rdwd downloads DWD data files into tempdir() before they are loaded into the environment
#' You can clear tempdir() with the following function:
#' unlink(tempdir(), recursive = TRUE)

WEATHER_Daily <- WEATHER_raw$data %>% 
  mutate(WST_ID = "SEDE") %>%
  relocate(WST_ID, .before = everything())

# TODO: implement metadata mapping inside get_weather() function
WEATHER_Header <- lapply(names(WEATHER_raw$metadata), function(df_name){
  
  df <- WEATHER_raw$metadata[[df_name]]
  
  if (length(unique(df$wst_id)) == 1) {
    data.frame(Year = gsub("Y", "", df_name),
               WST_ID = "SEDE",
               WST_LAT = df$wst_lat[1],
               WST_LONG = df$wst_lon[1],
               WST_ELEV = df$wst_elev[1],
               TAV = df$TAV[1],
               TAMP = df$AMP[1],
               REFHT = 2, WNDHT = 2)  # so far not extractable for DWD metadata
  } else {
    data.frame(Year = gsub("Y", "", df_name),
               WST_ID = "SEDE",
               WST_LAT = mean(df$wst_lat),
               WST_LONG = mean(df$wst_lon),
               WST_ELEV = mean(df$wst_elev),  # TODO: add note that data is drawn from multiple stations
               TAV = df$TAV[1],
               TAMP = df$AMP[1],
               REFHT = 2, WNDHT = 2)  # so far not extractable for DWD metadata
  }
}) %>%
  do.call(rbind, .)

WEATHER_comments <- lapply(WEATHER_raw$metadata, function(df) {
  df <- df %>%
    select(-res) %>%
    collapse_cols("var") %>%
    mutate(comments = paste0(var, ": Station ", wst_id, " - ", wst_name, " (", wst_lat, " ",
                             wst_lon, "; ELEV = ", wst_elev, " m); Distance from site: ",
                             round(dist, 1), " km"))
  return(df$comments)
} )

BNR_mapped$WEATHER_Daily <- WEATHER_Daily  # TODO: ?integrate weather mapping with other data?
BNR_mapped$WEATHER_Header <- WEATHER_Header



# Download and format corresponding soil data -----------------------


# TODO: SOIL <- funsoil() SoilGrids, National Agcultural Soil Information System (NASIS), ISRIC, etc.

# For now, use generic DSSAT soil (provided with the DSSAT CSM)
SOIL_generic <- read_sol(file_name = "./inst/extdata/SOIL.SOL", id_soil = "IB00000001")

# Generic soils are already in DSSAT format. For now we are using vMapper and the AgMIP translators
# so we will map them to ICASA to handle the entire dataset at once
SOIL_dssat_icasa <- read.csv("./data/soil_dssat_icasa.csv", fileEncoding = "latin1")

for (i in seq_along(colnames(SOIL_generic))) {
  for (j in 1:nrow(SOIL_dssat_icasa)){
    # Does not work with SCS FAMILY for some reason (likely because of space in colname)'
    # Not mapped for now, problem should be addressed
    if (colnames(SOIL_generic)[i] == SOIL_dssat_icasa$dssat_header[j]){
      colnames(SOIL_generic)[i] <- SOIL_dssat_icasa$icasa_header[j]
    }
  }
}

# Split header and profile data
SOIL_Header <- as.data.frame(SOIL_generic[1:20])  # Also make soil metadata???
SOIL_Layers <- unnest(SOIL_generic[21:ncol(SOIL_generic)],
                       cols = colnames(SOIL_generic)[21:ncol(SOIL_generic)]) %>%
  mutate(SOIL_ID = SOIL_Header$SOIL_ID) %>%
  relocate(SOIL_ID, .before = everything())

#
BNR_mapped$SOIL_Layers <- SOIL_Layers  # TODO: ?integrate weather mapping with other data?
BNR_mapped$SOIL_Header <- SOIL_Header



# Estimate missing phenological dates -------------------------------


pheno_estimates <- mapply(function(x, y){
  estimate_phenology(sdata <- BNR_mapped$OBSERVED_Summary,
                     wdata <- WEATHER_Daily,
                     crop <- y,
                     lat <- unique(BNR_mapped$FIELDS$FL_LAT),
                     lon <- unique(BNR_mapped$FIELDS$FL_LONG),
                     year <- x,
                     irrigated = FALSE)
}, BNR_mapped$CULTIVARS$Year, BNR_mapped$CULTIVARS$CRID, SIMPLIFY = FALSE)  # ignore warnings
# TODO: estimation in TimeSeries?

BNR_mapped$OBSERVED_Summary <- as_tibble(do.call(rbind, pheno_estimates))



# ICASA to DSSAT variable mapping -----------------------------------


# Map from ICASA to DSSAT
BNR_dssat <- BNR_mapped
for (i in seq_along(names(BNR_mapped))) {
  BNR_dssat[[i]] <- map_data(df = BNR_mapped[[i]],
                             tbl_name = names(BNR_mapped)[i],
                             map = icasa_dssat,
                             keep_unmapped = FALSE,
                             col_exempt = "Year")
}


# DSSAT data formatting ---------------------------------------------------


BNR_dssat_yr <- split_by_year(BNR_dssat)

# Append non-year-specific data
BNR_dssat_yr <- lapply(BNR_dssat_yr, function(x)
  append(x, list(GENERAL = BNR_dssat$GENERAL, FIELDS = BNR_dssat$FIELDS))
)



# Build FILE X ------------------------------------------------------------


BNR_yr_filex <- lapply(BNR_dssat_yr, function(ls) {
  build_filex(ls,
              title = attr(BNR_dssat, "EXP_DETAILS"),
              site_code = attr(BNR_dssat, "SITE_CODE"))
})



# Build FILE A ------------------------------------------------------------


BNR_yr_filea <- lapply(BNR_dssat_yr, function(ls) {
  build_filea(ls,
              title = attr(BNR_dssat, "EXP_DETAILS"),
              site_code = attr(BNR_dssat, "SITE_CODE"))
})
BNR_yr_filea <- BNR_yr_filea[lengths(BNR_yr_filea) > 0]


# Build FILE T ------------------------------------------------------------


BNR_yr_filet <- lapply(BNR_dssat_yr, function(ls) {
  build_filet(ls,
              title = attr(BNR_dssat, "EXP_DETAILS"),
              site_code = attr(BNR_dssat, "SITE_CODE"))
})
BNR_yr_filet <- BNR_yr_filet[lengths(BNR_yr_filet) > 0]


# Build SOL FILE ----------------------------------------------------------


BNR_soil <- list(SOIL_Header = BNR_dssat$SOIL_Header, SOIL_Layers = BNR_dssat$SOIL_Layers)
BNR_sol <- build_sol(BNR_soil)



# Build WTH FILE ----------------------------------------------------------


# Append weather station metadata in the comment section for each year
BNR_dssat_yr <- mapply(function(x, y) {
  attr(x[["WEATHER_Daily"]], "comments") <- 
    c(paste0("Source data downloaded from: DWD Open Data Server on ", Sys.Date(), " with csmTools"), y)
  return(x)
}, BNR_dssat_yr, WEATHER_comments)

BNR_yr_wth <- lapply(BNR_dssat_yr, function(ls) build_wth(ls))



# Export data -------------------------------------------------------------


# Merge the outputs
BNR_yr_merged <- list()
for (i in names(BNR_yr_filex)) {
  BNR_yr_merged[[i]] <- list(FILEX = BNR_yr_filex[[i]],
                             FILEA = BNR_yr_filea[[i]],
                             FILET = BNR_yr_filet[[i]],
                             WTH = BNR_yr_wth[[i]])
}

# Drop missing tables
BNR_yr_merged <- lapply(BNR_yr_merged, function(ls) ls[lengths(ls) > 0])

# Export the data
path <- paste0("./inst/extdata/", db_name, "/1_out")
if (dir.exists(path) == FALSE) {
  dir.create(path)
}

for (i in names(BNR_yr_merged)) {
  dir.create(paste0(path, "/", i))
  write_dssat(BNR_yr_merged[[i]], path = paste0(path, "/", i))
}

BNR_sol <- BNR_sol %>% rename(`SCS FAMILY` = SCS.FAMILY)  # problematic variable name with space
write_sol(BNR_sol, title = "General DSSAT Soil Input File", file_name = paste0(path, "/SEDE.SOL"),
          append = FALSE)
#TODO: generate file_name and title in the build_sol function



# Simulations -------------------------------------------------------------


# Specify the location of the DSSAT CSM executable (NB: should not be built into the package)
options(DSSAT.CSM = "C:\\DSSAT48\\DSCSM048.EXE")

# Specify dir for simulations: input files, batch files and simulations all stored there
dssat_dir <- "C:/DSSAT48"
old_wd <- getwd()
sim_wd <- paste0(old_wd, "./inst/extdata/lte_seehausen/2_sim")
setwd(sim_wd)


# ==== Input data adjustments ---------------------------------------------

# Set missing required variables
# NB: this is a temporary fix, should be done in the build functions with robust estimation methods + warning
# (imputation sould be documented in the input files as notes)
lteSe_1995_filex <- BNR_yr_merged$Y1995$FILEX
lteSe_1995_filex$PLANTING_DETAILS$PLDP <- 5.5  # planting depth

lteSe_1995_filex$TILLAGE$TDEP <- 
  ifelse(lteSe_1995_filex$TILLAGE$TIMPL == "TI038", 2.5, lteSe_1995_filex$TILLAGE$TDEP)  # missing tillage depth
lteSe_1995_filex$FERTILIZERS$FDEP <- 10  # fertilizer application depth

# Add cultivar to the cultivar file
# NB: parameter fitting script should be used eventually, for now we use a median cultivar based on the provided
# minima and maxima for genetic parameters

whaps_cul <- read_cul(paste0(dssat_dir, "/Genotype/WHAPS048.CUL"))

cul_median_pars <- apply(whaps_cul[1:2, 5:ncol(whaps_cul)], 2, function(x) sum(x)/2)

whaps_cul <- add_cultivar(whaps_cul,  # if the cultivar is still in the file (error) just ignore and move on
                          ccode = "IB9999",
                          cname = "Borenos",
                          ecode = "IB0001",
                          ppars = as.numeric(cul_median_pars[1:5]),
                          gpars = as.numeric(cul_median_pars[6:ncol(whaps_cul)])
)
lteSe_1995_filex$CULTIVARS$INGENO <- "IB9999"  # cultivat code in file X links to cultivar file

write_cul(whaps_cul, paste0(dssat_dir, "/Genotype/WHAPS048.CUL"))  # export the updated file


# Set simulation controls

# Simulation start date: by default at the earliest management event carried out
all_dates <- na.omit(  # ignore warning
  as.POSIXct(
    as.numeric(
  unlist(lapply(lteSe_1995_filex, function(df) {
    df[grepl("DAT", colnames(df))]
  }), use.names = FALSE)
)))
lteSe_1995_filex$SIMULATION_CONTROLS$SDATE <- min(all_dates)

# Set simulation options
lteSe_1995_filex$SIMULATION_CONTROLS$WATER <- "Y"  # water (rain/irrigation)
lteSe_1995_filex$SIMULATION_CONTROLS$NITRO <- "Y"  # nitrogen
lteSe_1995_filex$SIMULATION_CONTROLS$CHEM <- "Y"  # chemicals
lteSe_1995_filex$SIMULATION_CONTROLS$TILL <- "Y"  # tillage

# Set management settings
lteSe_1995_filex$SIMULATION_CONTROLS$FERTI <- "R"  # fertilizer application: on reported dates (R)
lteSe_1995_filex$SIMULATION_CONTROLS$HARVS <- "R"  # harvest: on reported dates (R)

# Set model (for this example we use NWheat)
lteSe_1995_filex$SIMULATION_CONTROLS$SMODEL <- "WHAPS"  # NWheat code, from DSSAT source code
lteSe_1995_filex$SIMULATION_CONTROLS$PHOTO <- "C"  # photosynthesis method set to canopy curve as required by NWheat

# Set output files options (only growth for this example)
lteSe_1995_filex$SIMULATION_CONTROLS$GROUT <- "Y"
lteSe_1995_filex$SIMULATION_CONTROLS$VBOSE <- "Y"  # verbose 

# Write example data files (X, A, T) in the simulation directory
# Prototype data: seehausen LTE, year 1995 (wheat - rainfed)
write_filex(lteSe_1995_filex, paste0(sim_wd, "/SEDE9501.WHX"))  # ignore warnings
write_filea(BNR_yr_merged$Y1995$FILEA, paste0(sim_wd, "/SEDE9501.WHA")) 

# Weather, soil and cultivar files must be located within the DSSAT CSM directory (locally installed)
# For weather files, two years may be required if management events took place in the fall/winter preceding
# the harvest years (typically planting/tillage)
unique(year(all_dates))  # 1994, 1995 ==> two weather files required

write_wth2(BNR_yr_merged$Y1994$WTH, paste0(dssat_dir, "/Weather/SEDE9401.WTH"))
write_wth2(BNR_yr_merged$Y1995$WTH, paste0(dssat_dir, "/Weather/SEDE9501.WTH"))

# Soil profile not copied as generic soil was used in this example(already in DSSAT Soil directory)
#write_sol(BNR_yr_merged$Y1995$WTH, "C:/Program Files (x86)/DSSAT48/Soil/SEDE.SOL")  # soil profile


# ==== Simulation runs ----------------------------------------------------

# Write batch file
batch_tbl <- data.frame(FILEX = "SEDE9501.WHX",
                        TRTNO = 1:4,
                        RP = 1,
                        SQ = 0,
                        OP = 0,
                        CO = 0)

# Write example batch file
write_dssbatch(batch_tbl)

# Run simulations
run_dssat(run_mode = "B")

setwd(old_wd)  # reset wd


# ==== Result plots -------------------------------------------------------


# TODO: eventually a wrapper for plotting essential results likes the obs vs. sim comparisons
# Should link to the input data to retrieve treatment names and levels
# Plot results: phenology

lteSe_sim_growth <- read_output(file_name = "./inst/extdata/lte_seehausen/2_sim/PlantGro.OUT")

# Format observed data for plotting
lteSe_obs_growth <- BNR_yr_merged$Y1995$FILEA %>%
  filter(TRTNO %in% 1:4) %>%
  mutate(MDAT = as.POSIXct(as.Date(MDAT, format = "%y%j")),
         ADAT = as.POSIXct(as.Date(ADAT, format = "%y%j")))

# Plot results: yield
lteSe_sim_growth %>%
  mutate(TRNO = as.factor(TRNO)) %>%
  ggplot(aes(x = DATE, y = GWAD)) +
  # Line plot for simulated data
  geom_line(aes(group = TRNO, colour = TRNO, linewidth = "Simulated")) +
  # Points for observed data
  geom_point(data = lteSe_obs_growth, aes(x = MDAT, y = HWAH, colour = as.factor(TRTNO), size = "Observed"), 
             shape = 20) +  # obs yield at harvest
  # General appearance
  scale_colour_manual(name = "Fertilization (kg[N]/ha)",
                    breaks = c("1","2","3","4"),
                    labels = c("0","100","200","300"),
                    values = c("#20854E","#FFDC91", "#E18727", "#BC3C29")) +
  scale_size_manual(values = c("Simulated" = 1, "Observed" = 2), limits = c("Simulated", "Observed")) +
  scale_linewidth_manual(values = c("Simulated" = 1, "Observed" = 2), limits = c("Simulated", "Observed")) +
  labs(size = NULL, linewidth = NULL, y = "Yield (kg/ha)") +
  guides(
    size = guide_legend(
      override.aes = list(linetype = c("solid", "blank"), shape = c(NA, 16))
    )
  ) +
  theme_bw() + 
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 9, colour = "black"))

# Plot results: phenology
lteSe_sim_growth %>%
  mutate(TRNO = as.factor(TRNO)) %>%
  ggplot(aes(x = DATE, y = DCCD)) +
  # Zadoks lines for comparison
  geom_hline(yintercept = 69, linetype = "dashed", colour = "black") +  # anthesis date (Zadoks65)
  geom_hline(yintercept = 95, linetype = "dashed", colour = "black") +  # maturity date (Zadoks95)
  # Line plot for simulated data
  geom_line(aes(group = TRNO, colour = TRNO, linewidth = "Simulated")) +
  # Points for observed data
  geom_point(data = lteSe_obs_growth, aes(x = ADAT, y = 69, colour = as.factor(TRTNO), size = "Observed"),
             shape = 20) +  # obs anthesis date (Zadosk65)
  geom_point(data = lteSe_obs_growth, aes(x = MDAT, y = 95, colour = as.factor(TRTNO), size = "Observed"),
             shape = 20) +  # obs maturity data (Zadoks95)
  # General appearance
  scale_colour_manual(name = "Fertilization (kg[N]/ha)",
                      breaks = c("1","2","3","4"),
                      labels = c("0","100","200","300"),
                      values = c("#20854E","#FFDC91", "#E18727", "#BC3C29")) +
  scale_size_manual(values = c("Simulated" = 1, "Observed" = 2), limits = c("Simulated", "Observed")) +
  scale_linewidth_manual(values = c("Simulated" = 1, "Observed" = 2), limits = c("Simulated", "Observed")) +
  labs(size = NULL, linewidth = NULL, y = "Zadoks scale") +
  guides(
    size = guide_legend(
      override.aes = list(linetype = c("solid", "blank"), shape = c(NA, 16))
    )
  ) +
  theme_bw() + 
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 9, colour = "black"))

# Results are off, but it works!

