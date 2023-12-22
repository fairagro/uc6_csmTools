
# Libraries ---------------------------------------------------------------


library(units)
library(purrr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(rdwd)
library(zoo) # na.approx, linear interpolation
library(xml2)
library(zip)
library(jsonlite)
library(OSMscale)
library(spsUtil)



# Package functions -------------------------------------------------------


source("./R/crop_data_types.R")
source("./R/etl_utils.R")
source("./R/get_weather_data.R")
source("./R/identify_data_relationships.R")
source("./R/identify_exp_design.R")
source("./R/merge_tbls.R")
source("./R/read_metadata.R")



# Package data ------------------------------------------------------------


load(file = "./data/wth_data_maps.Rda")



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
#   std_var = c("DATE","SRAD","SRAD","SRAD","TMAX","TMIN","RAIN","RAIN","DEWP","WIND","PAR","EVAP","RHUM"),
#   repo_var = c("MESS_DATUM","FG","FG_LBERG","FG_STRAHL","TT_TU","TT_TU","R1","RS","TT","F",NA,NA,"RF_STD"), #WIND TO SOLVE
#   repo_var_res = c("","hourly","hourly","daily","hourly","hourly","hourly","daily","hourly","hourly",NA,NA,"hourly"),
#   agg_fun = c("identity","sum","sum","identity","max","min","sum","identity","mean","mean",NA,NA,"mean"),
#   conv_fct = c(NA,0.01,0.01,0.01,1,1,1,1,1,60*60*24/1000,NA,NA,1)
# )
# 
# dwd_icasa <- data.frame(
#   std_var = c("W_DATE","SRAD","SRAD","SRAD","TMAX","TMIN","RAIN","RAIN","TDEW","WIND","PARD","EVAP","RHAVD"),
#   repo_var = c("MESS_DATUM","FG","FG_LBERG","FG_STRAHL","TT_TU","TT_TU","R1","RS","TT","F",NA,NA,"RF_STD"), #WIND TO SOLVE
#   repo_var_res = c("","hourly","hourly","daily","hourly","hourly","hourly","daily","hourly","hourly",NA,NA,"hourly"),
#   agg_fun = c("identity","sum","sum","identity","max","min","sum","identity","mean","mean",NA,NA,"mean"),
#   conv_fct = c(NA,0.01,0.01,0.01,1,1,1,1,1,60*60*24/1000,NA,NA,1)
# )
# 
# save(nasa_dssat, nasa_icasa, dwd_dssat, dwd_icasa, file = "./data/wth_data_maps.Rda")
