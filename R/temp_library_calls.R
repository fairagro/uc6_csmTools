###------------------ LIBRARIES --------------------------------------------------------------------------------

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

###------------------ FUNCTIONS --------------------------------------------------------------------------------

source("./R/crop_data_types.R")
source("./R/etl_utils.R")
#source("./R/get_weather_data")
source("./R/identify_data_relationships.R")
source("./R/identify_exp_design.R")
source("./R/merge_tbls.R")
source("./R/read_metadata.R")
