

# Crop experiment dataset identification and reshaping --------------

### FUN TEMPSCRIPT

# Advanced data transformation --------------------------------------

#' Files that are currently used for data mapping only handle exact matches between variables and unit conversion.
#' Producing advanced mapping standards and functions to handles more complex transformations (e.g., concatenation of
#' n columns, conditional variable naming, etc.) will be the main focus in early 2024. For now, we used the example
#' dataset to retrieve and manually transform the data. The information collected in such examples will be exploited
#' to design the mapping standards and functions. These specific adjustments are provided below with descriptions.

# ==== EXPERIMENTS table --------------------------------------------------

EXPERIMENTS <- EXPERIMENTS %>%
  mutate(SOIL_ID = "IB00000001",  # Currently generic soil is used
         WEATHER_ID = "DWMB") %>%  # Institute + Site: DWD, Muenchenberg
  relocate(SOIL_ID, WEATHER_ID, .after = NOTES)


# ==== HARVEST table ------------------------------------------------------

HARVEST <- OBSERVED$Time_series %>%
  # Drop yield variables (not management data)
  select(all_of(YEARS_nm), TRTNO, starts_with(c("ERNTE","TECHNIK"))) %>%
  distinct() %>%
  # Rank different date within year and treatment in decreasing order to separate i-s and e-o-s harvests
  group_by_at(c(YEARS_nm, "TRTNO")) %>%
  mutate(HA_type = ifelse(
    dense_rank(desc(as_date(ERNTE.Termin))) > 1, "is", "eos")) %>% ungroup() %>%
  # Keep only latest harvest date ("actual harvest")
  filter(HA_type == "eos") %>%
  # Generate harvest ID
  group_by_at(YEARS_nm) %>%
  mutate(HA_ID = cur_group_id()) %>% ungroup() %>%
  relocate(HA_ID, .before = everything())  # split for updated matrix and mngt only


# ==== FERTILIZERS table --------------------------------------------------

# FERTILIZERS and ORGANIC_MATERIALS tables
FERTILIZERS_join <- MANAGEMENT$DUENGUNG %>%
  # Separate inorganic and organic fertilizers
  filter(DUENGUNG.Mineralisch == 1) %>%
  separate(DU_ID, into = c("OM_ID", "FE_ID"), remove = FALSE, sep = "_") %>%
  # Update the ID accordingly
  group_by_at(c(YEARS_nm, "FE_ID")) %>% mutate(FE_ID = cur_group_id()) %>% ungroup() %>%
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

ORGANIC_MATERIALS_join <- MANAGEMENT$DUENGUNG %>%
  # Separate inorganic and organic fertilizers
  filter(DUENGUNG.Organisch == 1) %>%
  separate(DU_ID, into = c("OM_ID", "FE_ID"), remove = FALSE, sep = "_") %>%
  # Update the ID accordingly
  group_by_at(c(YEARS_nm, "OM_ID")) %>% mutate(OM_ID = cur_group_id()) %>% ungroup() %>%
  # Drop unused columns
  select(c(where(~!all(is.na(.))), -DUENGUNG.Mineralisch, -DUENGUNG.Organisch, -DUENGUNG.Gesamt_Stickstoff, -FE_ID)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x)))

ORGANIC_MATERIALS <- ORGANIC_MATERIALS_join %>%
  select(-DU_ID) %>%
  arrange(OM_ID) %>%
  distinct()


# ==== CULTIVARS table ----------------------------------------------------

CULTIVARS <- OBSERVED$Time_series %>% ##!! TODO: Retrieve which one contains ERNTE 
  select(YEARS_nm, TRTNO, starts_with("SORTE")) %>% ## TODO: not only update ID by year but also by crop
  distinct() %>%
  # Generate cultivar ID
  group_by_at(YEARS_nm) %>%
  mutate(CU_ID = cur_group_id()) %>% ungroup() %>%
  relocate(CU_ID, .before = everything()) # split for updated matrix and mngt only


# ==== PLANTINGS table ----------------------------------------------------

PLANTINGS <- MANAGEMENT$AUSSAAT %>%
  select(YEARS_nm, TRTNO, starts_with("SORTE"))


# ==== TREATMENTS matrix --------------------------------------------------

TREATMENTS <- DATA_out$TREATMENTS %>%
  left_join(HARVEST %>%
              dplyr::select(HA_ID, TRTNO, all_of(YEARS_nm)),
            by = c("TRTNO", YEARS_nm)) %>%
  left_join(CULTIVARS %>%
              dplyr::select(CU_ID, TRTNO, all_of(YEARS_nm)),
            by = c("TRTNO", YEARS_nm)) %>%
  left_join(FERTILIZERS_join %>%
              dplyr::select(FE_ID, DU_ID, all_of(YEARS_nm)),
            by = c("DU_ID", YEARS_nm))  %>%
  left_join(ORGANIC_MATERIALS_join %>%
              dplyr::select(OM_ID, DU_ID, all_of(YEARS_nm)),
            by = c("DU_ID", YEARS_nm)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>%
  dplyr::select(-DU_ID) %>%
  distinct()


# ==== OBSERVED_TimeSeries table ------------------------------------------

OBSERVED_TimeSeries <- DATA_out$OBSERVED_TimeSeries %>%
  # Rank different date within year and treatment in decreasing order to separate i-s and e-o-s harvests
  # as different variables characterize is and eos harvests in icasa
  group_by_at(c(YEARS_nm, "TRTNO")) %>%
  mutate(HA_type = ifelse(
    dense_rank(desc(as_date(ERNTE.Termin))) > 1, "is", "eos")) %>%
  relocate(HA_type, .before = everything())


# ==== OBSERVED_Summary table --------------------------------------------

#' Observed summary data is (currently?) not exploitable, as data collection dates are missing for the different
#' analyses (soil and plant samples). For example, soil N content is provided for some years but without sampling 
#' dates, it is not possible to determine whether this corresponds to initial conditions (before the growing season)
#' or to in-season measurements to control the influence of the fertilization treatments, and therefore not possible
#' to assign it to the adequate ICASA section (INITIAL CONDITIONS / SOIL ANALYSES).
#' Perhaps the missing information can be retrieved from the metadata or associated publications?


# Mapping to ICASA --------------------------------------------------------

BNR_full <- list(EXPERIMENTS = EXPERIMENTS,
                 TREATMENTS = TREATMENTS,
                 TILLAGE = MANAGEMENT$BODENBEARBEITUNG,
                 PLANTINGS = PLANTINGS,
                 CULTIVARS = CULTIVARS,
                 FERTILIZERS = FERTILIZERS,
                 ORGANIC_MATERIALS = ORGANIC_MATERIALS, 
                 IRRIGATION = MANAGEMENT$BEREGNUNG,
                 CHEMICALS = MANAGEMENT$PFLANZENSCHUTZ,
                 OBSERVED_Summary = DATA_out$OBSERVED_Summary,
                 OBSERVED_TimeSeries = OBSERVED_TimeSeries)


# Soil and weather data retrieval and mapping -----------------------

WEATHER <- get_weather(
  lat = unique(EXPERIMENTS$FL_LAT),
  lon = unique(EXPERIMENTS$FL_LON),
  years = sort(unique(EXPERIMENTS[[YEARS_nm]])),
  src = "dwd",
  map_to = "icasa",
  vars = c("air_temperature", "precipitation", "solar_radiation", "dewpoint", "relative_humidity", "wind_speed"),
  res = list("hourly", c("daily", "hourly"), c("daily", "hourly"), "hourly", "hourly", "hourly") ,
  max_radius = c(50, 10, 50, 20, 20, 20)
)

# This might take 5-10 minutes to run
# rdwd downloads DWD data files into tempdir() before they are loaded into the environment
# You can clear tempdir() with the following function:
# unlink(tempdir(), recursive = TRUE)

#SOIL <- funsoil()


# File output -------------------------------------------------------

