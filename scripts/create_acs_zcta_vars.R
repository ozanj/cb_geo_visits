################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_acs_zcta_vars.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 7/30/2025
## [ DESC ] < create zip-code level SES variables from tidycensus package; to be merged with school-level data>
################################################################################

#rm(list = ls())

### LIBRARIES
library(tidyverse)
library(lubridate)
library(haven)
library(labelled)
library(tidycensus)
# get census api key
#http://api.census.gov/data/key_signup.html
# set census api key, and install for future use
#census_api_key('aff360d1fe8a919619776f48e975f03b8bb1379e', install = TRUE)
Sys.getenv("CENSUS_API_KEY") # retreive API key
library(sf)
# Enable caching for tigris
options(tigris_use_cache = TRUE)
library(tigris)

getwd()
### DIRECTORY PATHS

data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

############ GET TRACT-LEVEL 2020 ACS DATA ON INCOME AND POVERTY FROM TIDYCENSUS

# Get the list of variables for the 2016-2020 5-year ACS
acs2020_vars <- load_variables(2020, "acs5", cache = TRUE) #
acs2020_vars %>% glimpse()

acs2020_vars %>% filter(name == "B19001_001") %>% print()

acs2020_vars %>% filter(name == "B01001A_001") %>% print()

acs2020_vars %>% filter(name == "B11001_001") %>% print() # total number of households

acs2020_vars %>% filter(name == "B19001_001") %>% print() # # "Total households" — This variable gives the total number of households that reported income data at the census tract level.

acs2020_vars %>% filter(name == "S1901_C01_013") %>% print() # 


#### get educational attainment variables

# Filter for variable names
edu_variables <- acs2020_vars %>%
  filter(str_detect(name, "^B15003") | str_detect(name, "^C15002"))

edu_variables %>% glimpse()
edu_variables
edu_var_names <- edu_variables$name

# WHY USE B11001_001E. RATHER THAN B19001_001E AS DENOMINATOR FOR CALCULATING MEAN HOUSEHOLD INCOME. from ChatGPT o3:
  # Mean HH income = B19025_001E / B11001_001E.
  # Use B11001_001E (all occupied households) because it matches the ACS control
  # universe; B19001_001E drops households with unclassified income, shrinking the
  # denominator and nudging the mean a bit high.

other_variables <- c(
  "B19025_001E",  # Aggregate household income (from table B19025)
  "B11001_001E",   # total number of households; use for calculating mean household income
  "B19001_001E",  # "Total households" — This variable gives the total number of households that reported income data at the census tract level.
  "B19013_001E",  # "Median Household Income in the Past 12 Months (in 2020 Inflation-Adjusted Dollars)"
  "B17017_002E",  # "Number of households living below poverty level" 
  "B17017_001E",  # "Total number of households that were considered for poverty status determination" 
  "B03002_001E",  # Total population
  "B03002_002E",  # Not Hispanic or Latino
  "B03002_003E",  # Not Hispanic or Latino: White alone
  "B03002_004E",  # Not Hispanic or Latino: Black or African American alone
  "B03002_005E",  # Not Hispanic or Latino: American Indian and Alaska Native alone
  "B03002_006E",  # Not Hispanic or Latino: Asian alone
  "B03002_007E",  # Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
  "B03002_008E",  # Not Hispanic or Latino: Some other race alone
  "B03002_009E",  # Not Hispanic or Latino: Two or more races
  "B03002_010E",  # Not Hispanic or Latino: Two races including Some other race
  "B03002_011E",  # Not Hispanic or Latino: Two races excluding Some other race, and three or more races
  "B03002_012E",  # Hispanic or Latino
  "B03002_013E",  # Hispanic or Latino: White alone
  "B03002_014E",  # Hispanic or Latino: Black or African American alone
  "B03002_015E",  # Hispanic or Latino: American Indian and Alaska Native alone
  "B03002_016E",  # Hispanic or Latino: Asian alone
  "B03002_017E",  # Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
  "B03002_018E",  # Hispanic or Latino: Some other race alone
  "B03002_019E",  # Hispanic or Latino: Two or more races
  "B03002_020E",  # Hispanic or Latino: Two races including Some other race
  "B03002_021E"   # Hispanic or Latino: Two races excluding Some other race, and three or more races
)
variables <- c(other_variables,edu_var_names)
variables

acs2020_vars %>% filter(name %in% variables) %>% print()

# READ IN ZCTA-LEVEL 2016-2020 ACS DATA FROM TIDYCENSUS

 # zcta_acs20 <- get_acs(
 #   geography = "zcta",
 #   variables  = variables,   # your character vector of variable IDs
 #   year       = 2020,
 #   survey     = "acs5",
 #   output     = "wide"
 # )
 # save(zcta_acs20, file = file.path(data_dir, 'zcta_acs20.RData'))

# RETREIVE MEAN INCOME, WHICH IS PART OF SUBJECT-TABLE DATASET

# get the variable list for 2020 ACS 5‑year *subject* tables
sub_vars <- load_variables(2020, "acs5/subject", cache = TRUE)

sub_vars %>% glimpse()
acs2020_vars %>% glimpse()
acs2020_vars %>% count(geography)

# confirm the mean‑income field
mean_inc_vars <- sub_vars %>% filter(name == "S1901_C01_013") 
mean_inc_vars$label
mean_inc_vars$concept

#append income variable description to description of other ACS variables
acs2020_vars <- acs2020_vars %>% bind_rows(mean_inc_vars)
rm(mean_inc_vars,sub_vars)

# zcta_acs20_mean_inc <- get_acs(
#   geography = "zcta",          # or "county", "state", etc.
#   variables = "S1901_C01_013",  # mean household income ($)
#   year      = 2020,
#   survey    = "acs5",
#   dataset   = "acs5/subject",    # <- key difference
#   output = 'wide'
# )
# zcta_acs20_mean_inc %>% glimpse()
# save(zcta_acs20_mean_inc, file = file.path(data_dir, 'zcta_acs20_mean_inc.RData'))

# remove un-needed objects
  rm(edu_variables,edu_var_names,other_variables,variables)

# load saved objects
  load(file.path(data_dir, 'zcta_acs20.RData'))
  load(file.path(data_dir, 'zcta_acs20_mean_inc.RData'))
  
  zcta_acs20_mean_inc %>% glimpse()
  
# merge calculated mean income data into other measures
  zcta_acs20 <- zcta_acs20 %>% inner_join(
    y = zcta_acs20_mean_inc %>% select(-NAME),
    by = 'GEOID'
  )

rm(zcta_acs20_mean_inc)

zcta_acs20 %>% glimpse()

# ADD VARIABLE LABELS FROM load_variables() FUNCTION TO ACS DATA

# Use str_subset to get the variables matching the pattern, vars that end with E
matching_variables <- str_subset(string = zcta_acs20 %>% names(), pattern = "_\\d+E$")

# Remove the 'E' suffix to match with the 'name' column in acs2020_vars
matching_variables <- str_remove(matching_variables, "E$")

matching_variables  

# Create a mapping dataframe
label_mapping <- acs2020_vars %>% 
  filter(name %in% matching_variables) %>% 
  select(name, label)
label_mapping

# Create a named vector for labels, including both 'E' and 'M' suffixes
labels <- setNames(rep(label_mapping$label, each = 2), 
                   paste0(rep(label_mapping$name, each = 2), c("E", "M")))

labels

# Replace "Estimate!!" with "Margin of error!!" for margin of error variables
labels <- setNames(
  c(label_mapping$label, str_replace(label_mapping$label, "^Estimate!!", "Margin of error!!")),
  c(paste0(label_mapping$name, "E"), paste0(label_mapping$name, "M"))
)
labels

# Create placeholders for GEOID and NAME
placeholders <- setNames(rep("", 2), c("GEOID", "NAME"))

# Combine the placeholders with the existing labels
full_labels <- c(placeholders, labels)
full_labels

# Reorder full_labels to match the order of var_names
reordered_labels <- full_labels[names(zcta_acs20)]
reordered_labels

# Assign the labels using labelled::var_label
var_label(zcta_acs20) <- reordered_labels

zcta_acs20 %>% glimpse()
zcta_acs20 %>% var_label()

# remove objects
rm(placeholders,full_labels,labels,label_mapping,matching_variables,reordered_labels)
rm(acs2020_vars)

# rename vars to lowercase
names(zcta_acs20) <- zcta_acs20 %>% names %>% tolower()

# drop the margin of error variables
zcta_acs20 <- zcta_acs20 %>% select(-c(matches('_\\d+m$')))

zcta_acs20 %>% var_label()

zcta_acs20 %>% glimpse()

zcta_acs20_anal <- zcta_acs20 %>% as.data.frame() %>% 
  # RACE/ETHNICITY VARIABLES
  # drop variables you don't need (after running checks)
  # non-hispanic, two-or more races: amp3e009 always == amp3e010+amp3e011
  # amp3e009: "Estimates: Not Hispanic or Latino: Two or more races"
  # amp3e010: "Estimates: Not Hispanic or Latino: Two or more races: Two races including Some other race"
  # amp3e011: [1] "Estimates: Not Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"
  # hispanic, two or more races: amp3e019 always == amp3e020+amp3e021
  # amp3e019. "Estimates: Hispanic or Latino: Two or more races"
  # amp3e020. "Estimates: Hispanic or Latino: Two or more races: Two races including Some other race"
  # amp3e021. "Estimates: Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"  
  select(-c(name,b03002_010e,b03002_011e,b03002_020e,b03002_021e)) %>% 
  # RENAME AND CREATE VARS OF POPULATION BY RACE, ETHNICITY, AND AGE
  rename(
    tot_all = b03002_001e,
    nhisp_all = b03002_002e,
    hisp_all = b03002_012e,
    
    nhisp_white = b03002_003e,
    nhisp_black = b03002_004e,
    nhisp_native = b03002_005e,
    nhisp_asian = b03002_006e,
    nhisp_nhpi = b03002_007e,
    nhisp_other = b03002_008e,
    nhisp_multi = b03002_009e,
    
    hisp_white = b03002_013e,
    hisp_black = b03002_014e,
    hisp_native = b03002_015e,
    hisp_asian = b03002_016e,
    hisp_nhpi = b03002_017e,
    hisp_other = b03002_018e,
    hisp_multi = b03002_019e,
  ) %>% 
  # api = asian + nhpi
  mutate(
    # api vars
    nhisp_api = rowSums(select(., nhisp_asian, nhisp_nhpi), na.rm = TRUE),
    hisp_api =  rowSums(select(., hisp_asian, hisp_nhpi), na.rm = TRUE),
  ) %>% 
  # INCOME AND POVERTY VARIABLES
  rename(
    #income
    households_tot = b11001_001e, # "all occupied households; chatGPT says that CENSUS uses this var as denominator rather than B19001_001E, which drops households with unclassified income, shrinking the denominator and nudging the mean a bit high.
    inc_house_agg = b19025_001e, # "Estimate!!Aggregate household income in the past 12 months (in 2020 inflation-adjusted dollars)"
    inc_house_med = b19013_001e, # "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)"
    inc_house_mean = s1901_c01_013e, # Estimate!!Households!!Mean income (dollars); "INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)"
    
    # poverty
    pov_yes = b17017_002e,
    pov_denom = b17017_001e
  ) %>%
  # EDUCATIONAL ATTAINMENT
  mutate(
    # all races
    edu_lths_all = rowSums(select(., b15003_002e:b15003_016e), na.rm = TRUE),  # No schooling to 12th grade, no diploma
    edu_hs_all = rowSums(select(., b15003_017e, b15003_018e), na.rm = TRUE),  # Regular high school diploma, GED or alternative credential
    edu_ltassoc_all = rowSums(select(., b15003_019e, b15003_020e), na.rm = TRUE),  # Some college, less than 1 year, 1+ years, no degree
    edu_assoc_all = rowSums(select(., b15003_021e), na.rm = TRUE),  # Associate's degree
    edu_ltba_all = edu_ltassoc_all + edu_assoc_all,  # Less than bachelor's (some college + associate's)
    edu_ba_all = rowSums(select(., b15003_022e), na.rm = TRUE),  # Bachelor's degree
    edu_ma_all = rowSums(select(., b15003_023e), na.rm = TRUE),  # Master's degree
    edu_fprof_all = rowSums(select(., b15003_024e), na.rm = TRUE),  # Professional school degree
    edu_doct_all = rowSums(select(., b15003_025e), na.rm = TRUE),  # Doctorate degree
    edu_baplus_all = rowSums(select(., b15003_022e:b15003_025e), na.rm = TRUE),  # Bachelor's degree and higher
    edu_tot_all = rowSums(select(., b15003_001e), na.rm = TRUE),  # Total population (summing all educational attainment levels)
    
    # white alone (includes hispanic and non-hispanic)
    edu_lths_white = rowSums(select(., c15002a_003e, c15002a_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_white = rowSums(select(., c15002a_004e, c15002a_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_white = rowSums(select(., c15002a_005e, c15002a_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_white = rowSums(select(., c15002a_006e, c15002a_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_white = rowSums(select(., c15002a_002e, c15002a_007e), na.rm = TRUE),     # Total population
    
    # black alone (includes hispanic and non-hispanic)
    edu_lths_black = rowSums(select(., c15002b_003e, c15002b_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_black = rowSums(select(., c15002b_004e, c15002b_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_black = rowSums(select(., c15002b_005e, c15002b_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_black = rowSums(select(., c15002b_006e, c15002b_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_black = rowSums(select(., c15002b_002e, c15002b_007e), na.rm = TRUE),     # Total population
    
    # native (includes hispanic and non-hispanic)
    edu_lths_native = rowSums(select(., c15002c_003e, c15002c_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_native = rowSums(select(., c15002c_004e, c15002c_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_native = rowSums(select(., c15002c_005e, c15002c_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_native = rowSums(select(., c15002c_006e, c15002c_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_native = rowSums(select(., c15002c_002e, c15002c_007e), na.rm = TRUE),     # Total population    
    
    # asian (includes hispanic and non-hispanic)
    edu_lths_asian = rowSums(select(., c15002d_003e, c15002d_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_asian = rowSums(select(., c15002d_004e, c15002d_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_asian = rowSums(select(., c15002d_005e, c15002d_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_asian = rowSums(select(., c15002d_006e, c15002d_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_asian = rowSums(select(., c15002d_002e, c15002d_007e), na.rm = TRUE),      # Total population    
    
    # nhpi (includes hispanic and non-hispanic)
    edu_lths_nhpi = rowSums(select(., c15002e_003e, c15002e_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_nhpi = rowSums(select(., c15002e_004e, c15002e_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_nhpi = rowSums(select(., c15002e_005e, c15002e_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_nhpi = rowSums(select(., c15002e_006e, c15002e_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_nhpi = rowSums(select(., c15002e_002e, c15002e_007e), na.rm = TRUE),     # Total population    
    
    # API (Asian + NHPI) (includes hispanic and non-hispanic)
    edu_lths_api = rowSums(select(., c15002d_003e, c15002d_008e, c15002e_003e, c15002e_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_api = rowSums(select(., c15002d_004e, c15002d_009e, c15002e_004e, c15002e_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_api = rowSums(select(., c15002d_005e, c15002d_010e, c15002e_005e, c15002e_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_api = rowSums(select(., c15002d_006e, c15002d_011e, c15002e_006e, c15002e_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_api = rowSums(select(., c15002d_002e, c15002d_007e, c15002e_002e, c15002e_007e), na.rm = TRUE),      # Total population    
    
    # multi (includes hispanic and non-hispanic)
    edu_lths_multi = rowSums(select(., c15002g_003e, c15002g_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_multi = rowSums(select(., c15002g_004e, c15002g_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_multi = rowSums(select(., c15002g_005e, c15002g_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_multi = rowSums(select(., c15002g_006e, c15002g_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_multi = rowSums(select(., c15002g_002e, c15002g_007e), na.rm = TRUE),     # Total population
    
    # nhisp_white (Non-Hispanic White alone)
    edu_lths_nhisp_white = rowSums(select(., c15002h_003e, c15002h_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_nhisp_white = rowSums(select(., c15002h_004e, c15002h_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_nhisp_white = rowSums(select(., c15002h_005e, c15002h_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_nhisp_white = rowSums(select(., c15002h_006e, c15002h_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_nhisp_white = rowSums(select(., c15002h_002e, c15002h_007e), na.rm = TRUE),     # Total population
    
    # hisp (Hispanic or Latino)
    edu_lths_hisp = rowSums(select(., c15002i_003e, c15002i_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_hisp = rowSums(select(., c15002i_004e, c15002i_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_hisp = rowSums(select(., c15002i_005e, c15002i_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_hisp = rowSums(select(., c15002i_006e, c15002i_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_hisp = rowSums(select(., c15002i_002e, c15002i_007e), na.rm = TRUE)     # Total population    
  ) %>% 
  # drop input vars
  select(-c(starts_with('c15002')),-c(starts_with('b15003'))) %>% 
# create tract-level analysis sf dataset that has percent variables
  mutate(
    
    # create percent race and ethnicity variables
    pct_nhisp_white = nhisp_white / tot_all * 100,
    pct_nhisp_black = nhisp_black / tot_all * 100,
    pct_nhisp_native = nhisp_native / tot_all * 100,
    pct_nhisp_asian = nhisp_asian / tot_all * 100,
    pct_nhisp_nhpi = nhisp_nhpi / tot_all * 100,
    pct_nhisp_other = nhisp_other / tot_all * 100,
    pct_nhisp_multi = nhisp_multi / tot_all * 100,
    pct_hisp_white = hisp_white / tot_all * 100,
    pct_hisp_black = hisp_black / tot_all * 100,
    pct_hisp_native = hisp_native / tot_all * 100,
    pct_hisp_asian = hisp_asian / tot_all * 100,
    pct_hisp_nhpi = hisp_nhpi / tot_all * 100,
    pct_hisp_other = hisp_other / tot_all * 100,
    pct_hisp_multi = hisp_multi / tot_all * 100,
    pct_nhisp_api = nhisp_api / tot_all * 100,
    pct_hisp_api = hisp_api / tot_all * 100,
    pct_hisp_all = hisp_all / tot_all * 100,
    pct_nhisp_all = nhisp_all / tot_all * 100,
    
    # create mean income variable
    # sum of agg inc across all tracts/ (sum of total households across all tracts)
    inc_house_mean_calc = inc_house_agg/households_tot,
    
    # Convert 2020 dollars to 2024 dollars using CPI data
    # CPI for 2020: 258.811
    # CPI for 2024: 314.175
    # Formula: Adjusted Amount = Original Amount * (CPI in 2024 / CPI in 2020) = (og amoun)t * (314.175/258.811) = (og amount) * 1.213917
    inc_house_med = inc_house_med*1.213917,    
    inc_house_mean = inc_house_mean*1.213917,
    inc_house_mean_calc = inc_house_mean_calc*1.213917,
    
    # create percent poverty variables
    pct_pov_yes = pov_yes/pov_denom*100,  
    
    # education vars; focus on percent of households w/ a BA or higher
    pct_edu_baplus_all = edu_baplus_all / edu_tot_all * 100,
    pct_edu_baplus_white = edu_baplus_white / edu_tot_white * 100,
    pct_edu_baplus_black = edu_baplus_black / edu_tot_black * 100,
    pct_edu_baplus_native = edu_baplus_native / edu_tot_native * 100,
    pct_edu_baplus_asian = edu_baplus_asian / edu_tot_asian * 100,
    pct_edu_baplus_nhpi = edu_baplus_nhpi / edu_tot_nhpi * 100,
    pct_edu_baplus_api = edu_baplus_api / edu_tot_api * 100,
    pct_edu_baplus_multi = edu_baplus_multi / edu_tot_multi * 100,
    pct_edu_baplus_hisp = edu_baplus_hisp / edu_tot_hisp * 100,
    pct_edu_baplus_nhisp_white = edu_baplus_nhisp_white / edu_tot_nhisp_white * 100,  
    
  )

rm(zcta_acs20)
zcta_acs20_anal %>% glimpse()

save(zcta_acs20_anal, file = file.path(data_dir, 'zcta_acs20_anal.RData'))

# load saved objects
load(file.path(data_dir, 'zcta_acs20_anal.RData'))
