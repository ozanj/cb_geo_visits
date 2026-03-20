# CREATING INDICATORS OF DISTANCE FROM GEOMARKETS

### SETTINGS
#rm(list = ls())
options(max.print = 1000)
#options(width = 160)

library(tidyverse)

####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES
getwd()
source(file = file.path("scripts", "create_cb_geo_hs_visits.R"))
getwd()

allyr_anal_eps_sf %>% count(year)


y2020_anal_eps_sf <- allyr_anal_eps_sf %>% filter(year==2020)

y2020_anal_eps_sf %>% glimpse()

pubprivhs_df %>% glimpse()
