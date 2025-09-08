################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_ca_sat_vars.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/7/2025
## [ DESC ] < create school-level measures of SAT scores>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1000)
#options(width = 160)

### LIBRARIES
library(tidyverse)

# read in sat score data and cds nces crosswalk
getwd()
cds_nces_xwalk <- read_csv(file = file.path('.','data','ca_sat_data','CDS_NCES_crosswalk.csv')) %>% 
  rename_with(tolower, .cols = everything()) %>% 
  select(cdscode, ncesdist, ncesschool,school) %>%
  filter(ncesschool != "No Data") %>%
  mutate(ncesid = paste0(ncesdist, str_pad(ncesschool, 5, pad = "0")))

cds_nces_xwalk %>% print(n=50)

cds_nces_xwalk %>% glimpse()

ca_sat16_17 <- read_csv(file = file.path('.','data','ca_sat_data','sat16-17.csv')) %>% 
  rename_with(tolower, .cols = everything()) %>% 
  # exclude data that is not school-level
  filter(scode != '0000000') %>% 
  # merge in nces id
  left_join(
    y = cds_nces_xwalk %>% select(cdscode,ncesid) %>% mutate(one = 1),
    by = c('cds' = 'cdscode')
  ) 
  ca_sat16_17 %>% glimpse()
  # schools without nces id

  rm(cds_nces_xwalk)
  ca_sat16_17 %>% count(one)
  ca_sat16_17 %>% filter(is.na(one)) %>%  select(ncesid,cds,ccode,cdcode,scode,rtype,sname,dname,cname,enroll12,numtsttakr)  %>% print(n=10)

# nces codes for the 7 schools that did not merge
nces_lookup <- tribble(
  ~cds,            ~ncesid_v2,
  "12629271230150","060005208486",
  "19647330100776","069107811339",
  "19647330115030","069107812010",
  "19647330115212","069107811842",
  "19647336119945","069107810524",
  "19764970115725","060962012119",
  "36679343630761","069102905058"
)

ca_sat16_17 <- ca_sat16_17 %>% left_join(nces_lookup, by = "cds") %>% 
  mutate(
    ncesid = if_else(is.na(one),ncesid_v2,ncesid)
  ) %>% select(-c(one,ncesid_v2)) %>% 
  #filter(is.na(one)) %>%  select(ncesid,ncesid_v2,cds,ccode,cdcode,scode,rtype,sname,dname,cname,enroll12,numtsttakr)  %>% print(n=10)
  select(-c(cds,ccode,cdcode,scode,rtype,dname,cname,numcurrelabenchmark,numpreelabenchmark,numcurrmathbenchmark,numpremathbenchmark)) %>% 
  rename(hs_g12 = enroll12,hs_num_took_sat = numtsttakr, hs_num_prof_sat_ela = totnumelabenchmark, hs_pct_prof_sat_ela = pctelabenchmark,
         hs_num_prof_sat_math = totnummathbenchmark, hs_pct_prof_sat_math = pctmathbenchmark, hs_num_prof_sat_ela_math = totnumbothbenchmark, 
         hs_pct_prof_sat_ela_math = pctbothbenchmark, hs_ncessch = ncesid) %>% 
  relocate(hs_ncessch)

  rm(nces_lookup)
# convert variables to numeric
cols_to_num <- c(
  "hs_num_prof_sat_ela","hs_pct_prof_sat_ela",
  "hs_num_prof_sat_math","hs_pct_prof_sat_math",
  "hs_num_prof_sat_ela_math","hs_pct_prof_sat_ela_math"
)

ca_sat16_17 <- ca_sat16_17 %>% mutate(across(all_of(cols_to_num), as.numeric)) # you. get warnings but it's fine.
rm(cols_to_num)

ca_sat16_17 %>% glimpse()
ca_sat16_17 %>% print(n = 500)

# save

getwd()
save(ca_sat16_17, file = file.path('.','data','ca_sat_data','ca_sat16_17.RData'))

rm(ca_sat16_17)

load(file = file.path('.','data','ca_sat_data','ca_sat16_17.RData'))

