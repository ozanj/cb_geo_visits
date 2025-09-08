################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < rq3_lpm.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 9/4/2025
## [ DESC ] < RQ3 modeling relationship between Geomarket [Z], school disadvantage [X] and probability of a high school getting a visit [Y]>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1500)
#options(width = 160)

library(tidyverse)
library(forcats)
library(scales)
library(fixest)
library(modelsummary)


#rm(list = ls())

####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES


getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()

# remove objects from cb_geo (ajs manuscript) mapping
rm(create_rq1_map,format_vars,get_palette)

####### RUN SCRIPT THAT CREATES OBJECT WITH ONE OBSERVATION PER UNIVERSITY, EPS THAT HAS VARIABLES ABOUT NUMBER OF SCHOOLS AND NUMBER OF VISITS TO THOSE SCHOOOLS

getwd()
source(file = file.path('scripts', 'create_univ_geo_df.R'))
getwd()

# load school-level CA SAT data from 2016-17
load(file = file.path('.','data','ca_sat_data','ca_sat16_17.RData'))

ca_sat16_17 %>% glimpse()
pubprivhs_df %>% glimpse()


pubprivhs_univ_ca <- pubprivhs_univ_df %>% left_join(
  y = ca_sat16_17 %>% select(-sname) %>% rename(hs_g12_sat = hs_g12) %>% mutate(one = 1),
  by = c('hs_ncessch')
) %>% filter(hs_state_code == 'CA',hs_control == 'public',hs_school_type == 'regular school') %>% 
  select(-one) %>% 
  


  #pubprivhs_univ_ca %>% filter(univ_abbrev == 'Emory') %>% select(hs_eps_codename,hs_sch_name,hs_free_reduced_lunch,hs_free_reduced_reg_lunch,hs_tot_students,hs_pct_free_reduced_lunch) %>% print(n=500)

pubprivhs_univ_ca %>% glimpse()

rhs_pub <- c(
  "hs_g11",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade","hs_magnet01","hs_school_type",
  "hs_pct_prof_sat_ela","hs_pct_prof_sat_math",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)
# add hs_eps_codename to predictors
predictors <- c(rhs_pub, "hs_eps_codename")

# build the formula
rhs_formula <- reformulate(termlabels = predictors, response = "visit01")
rhs_formula

# run the model
model <- feols(
  fml = rhs_formula,
  data    = pubprivhs_univ_ca %>% filter(univ_id == "126614") %>% filter(hs_g12 >=100) # replace with your dataset
)

# look at results
summary(model)

# University ID reference table (univ_info)
# -----------------------------------------
# univ_name                                  univ_id
# Middlebury College                         230959
# Swarthmore College                         216287
# Scripps College                            123165
# Occidental College                         120254
# Harvey Mudd College                        115409
# Colorado College                           126678
# Sewanee-The University of the South        221519
# Oberlin College                            204501
# Macalester College                         173902
# Connecticut College                        128902
# Smith College                              167835
# Williams College                           168342
# Northwestern University                    147767
# University of Notre Dame                   152080
# Case Western Reserve University            201645
# Emory University                           139658
# Baylor University                          223232
# Tulane University of Louisiana             160755
# Southern Methodist University              228246
# University of Denver                       127060
# Tufts University                           168148
# Marquette University                       239105
# Villanova University                       216597
# Boston College                             164924
# Texas Christian University                 228875
# Stevens Institute of Technology            186867
# The University of Alabama                  100751
# University of South Carolina-Columbia      218663
# University of Georgia                      139959
# University of Nebraska-Lincoln             181464
# University of Cincinnati-Main Campus       201885
# University of Pittsburgh-Pittsburgh Campus 215293
# Rutgers University-New Brunswick           186380
# University of California-Berkeley          110635
# University of California-Irvine            110653
# University of Colorado Boulder             126614
# University of Kansas                       155317
# University of Arkansas                     106397
# University of Massachusetts-Amherst        166629
# University of California-Riverside         110671
# University of California-San Diego         110680
# Stony Brook University                     196097
