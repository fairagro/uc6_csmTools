
# Libraries ---------------------------------------------------------------


library(units)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(rdwd)
library(zoo) # na.approx, linear interpolation
library(xml2)
library(readxl)
library(zip)
library(jsonlite)
library(OSMscale)
library(spsUtil)
library(rlang)
library(cropCalendars)  # Estimate phenology
library(DSSAT)
library(rlang)
library(countrycode)
library(ggplot2)

# Package data ------------------------------------------------------------


load(file = "./data/wth_data_maps.Rdata")
load(file = "./data/icasa_dssat.Rdata")
load(file = "./data/BnR_seehausen_icasa.Rdata")



# Generate package data (not read) ----------------------------------------


# nasa_dssat <- data.frame(
#   std_var = c("SRAD","TMAX","TMIN","RAIN","DEWP","WIND","PAR","EVAP","RHUM"),
#   repo_var = c("ALLSKY_SFC_SW_DWN","T2M","T2M","PRECTOTCORR","T2MDEW","WS2M","ALLSKY_SFC_PAR_TOT",NA,"RH2M"),
#   agg_fun = c("sum","max","min","sum","mean","mean","sum",NA,"mean"),
#   conv_fct = c(1,1,1,1,1,60*60*24/1000,4.6*3600/1000000,NA,1)
# )
# 
# nasa_icasa <- data.frame(
#   std_var = c("SRAD","TMAX","TMIN","RAIN","TDEW","WIND","PARD","EVAP","RHAVD"),
#   repo_var = c("ALLSKY_SFC_SW_DWN","T2M","T2M","PRECTOTCORR","T2MDEW","WS2M","ALLSKY_SFC_PAR_TOT",NA,"RH2M"),
#   agg_fun = c("sum","max","min","sum","mean","mean","sum",NA,"mean"),
#   conv_fct = c(1,1,1,1,1,60*60*24/1000,4.6*3600/1000000,NA,1)
# )
# 
# dwd_dssat <- data.frame(
#   std_var = c("DATE","SRAD","SRAD","SRAD","TMAX","TMIN","TMEAN","RAIN","RAIN","DEWP","WIND","PAR","EVAP","RHUM"),
#   repo_var = c("MESS_DATUM","FG","FG_LBERG","FG_STRAHL","TT_TU","TT_TU","TT_TU","R1","RS","TT","F",NA,NA,"RF_STD"), #WIND TO SOLVE
#   repo_var_res = c("","hourly","hourly","daily","hourly","hourly","hourly","hourly","daily","hourly","hourly",NA,NA,"hourly"),
#   agg_fun = c("identity","sum","sum","identity","max","min","mean","sum","identity","mean","mean",NA,NA,"mean"),
#   conv_fct = c(NA,0.01,0.01,0.01,1,1,1,1,1,1,60*60*24/1000,NA,NA,1)
# )
# 
# dwd_icasa <- data.frame(
#   std_var = c("W_DATE","SRAD","SRAD","SRAD","TMAX","TMIN","TMEAN","RAIN","RAIN","TDEW","WIND","PARD","EVAP","RHAVD"),
#   repo_var = c("MESS_DATUM","FG","FG_LBERG","FG_STRAHL","TT_TU","TT_TU","TT_TU","R1","RS","TT","F",NA,NA,"RF_STD"), #WIND TO SOLVE
#   repo_var_res = c("","hourly","hourly","daily","hourly","hourly","hourly","hourly","daily","hourly","hourly",NA,NA,"hourly"),
#   agg_fun = c("identity","sum","sum","identity","max","min","mean","sum","identity","mean","mean",NA,NA,"mean"),
#   conv_fct = c(NA,0.01,0.01,0.01,1,1,1,1,1,1,60*60*24/1000,NA,NA,1)
# )
# 
# save(nasa_dssat, nasa_icasa, dwd_dssat, dwd_icasa, file = "./data/wth_data_maps.Rda")
#
# icasa_dssat <- read.csv(
#   "C:/Users/bmlle/Documents/0_DATA/TUM/HEF/2_FAIRagro/UseCases/UC6_IntegratedModeling/Vocabularies/vocabMaps/data/ICASA_DSSAT_maps.csv"
#   )
# 
# save(icasa_dssat, file = "./data/icasa_dssat.Rda")
#
#
# bnr_seehausen_icasa <- read.csv(
#   "C:/Users/bmlle/Documents/0_DATA/TUM/HEF/2_FAIRagro/UseCases/UC6_IntegratedModeling/Workflows/csmTools/inst/extdata/lte_seehausen/lte_seehausen_ICASA_map.csv",
#   fileEncoding = "latin1"
# ) %>% mutate_all(., ~ifelse(is.na(.x), "", .x))
# 
# save(bnr_seehausen_icasa, file = "./data/BnR_seehausen_icasa.Rda")
