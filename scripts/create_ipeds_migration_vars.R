################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_ipeds_migration_vars.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/7/2025
## [ DESC ] < create IPEDS measures of freshman enrollment by state>
################################################################################

rm(list = ls())

### LIBRARIES
library(tidyverse)
library(lubridate)
library(haven)
library(labelled)

getwd()
### DIRECTORY PATHS

data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

ipeds_migration_dir <- file.path('.',data_dir,'ipeds_migration')
list.files(path = ipeds_migration_dir)

list.files(path = file.path('.',ipeds_migration_dir,'non-collapse'))

#### read in ipeds directory data

# University data from IPEDS
univ_data <- readRDS(file.path('.','..','recruiting-chapter','data','ipeds_1718.RDS'))
univ_info <- read.csv(file.path('.','..','recruiting-chapter','data','univ_data.csv'), header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'zip_code' = 'character')) %>% as_tibble() %>% 
  filter(!(univ_id %in% c('168218','199193','149222')))  # Wellesley, NCSU, UCI, SIU-Carbondale

#### NEXT STEPS
  # MERGE OPEID5 TO UNIV_INFO
  # MERGE THE UNIV_INFO DATA TO THE FALL ENROLLMENT DATA. 
    # MAKE SURE YOU HAVE GOOD FALL ENROLLMENT DATA FOR ALL 42 UNIVERSITIES
    # COMPARE HOW THE NUMBERS LOOK FOR THE 42 UNIVERSITIES ACROSS THE THREE ALTERNATIVE VERSIONS OF THE IPEDS DATA

# -------- 0. vector of collapse types --------
types <- c("non", "unitid", "opeid")

# -------- 1. container to store the data frames --------
ipeds_migration <- vector("list", length(types))
names(ipeds_migration) <- paste0("ipeds_migration_", types, "_collapse")

# -------- 2. loop over the types and read each file --------
for (i in seq_along(types)) {
  t <- types[i]
  
  # build path and read
  df <- read_dta(
    file.path(
      ipeds_migration_dir,
      paste0(t, "-collapse"),
      paste0("append-migration-", t, "-collapse.dta")
    )
  ) %>% filter(endyear>=2001) %>% 
  # rename vars
  select(-matches("^freshst(?!hs)", perl = TRUE),-c(freshinstpct,freshinst)) %>% # ,parentefm
  select(-starts_with('ifresh')) %>%     
  rename_with(
    ~ paste0("freshhs_", tolower(str_remove(.x, "^freshsths"))),
    .cols = matches("^freshsths[A-Z]+$")
  )    
  
  # store in list
  ipeds_migration[[i]] <- df
  
  # quick checks
  #df %>% count(endyear) %>% print(n = 40)
  #glimpse(df)
}

# ipeds_migration is a named list of the three data frames
# unpack into individual objects:
  list2env(ipeds_migration, .GlobalEnv)
  rm(df,i,types,t,ipeds_migration)
  
ipeds_migration_non_collapse %>% glimpse()
ipeds_migration_unitid_collapse %>% glimpse()
ipeds_migration_opeid_collapse %>% glimpse()

ipeds_migration_unitid_collapse %>% count(endyear,freshsthsBAL) %>% print(n=100)

ipeds_migration_unitid_collapse %>%
  select(-matches("^freshst(?!hs)", perl = TRUE),-c(freshinstpct,freshinst,parentefm)) %>%
  rename_with(
    ~ paste0("freshhs_", tolower(str_remove(.x, "^freshsths"))),
    .cols = matches("^freshsths[A-Z]+$")
  ) %>% glimpse()

rm(ipeds_migration_unitid_clean)


ipeds_migration_unitid_collapse %>% 
  arrange(unitid,endyear) %>% 
  filter(unitid == 100751) %>% 
  select(unitid,endyear,freshsthsAL,freshstAL,freshsthsCA,freshstCA,freshsthsTOT,freshstTOT,freshinst,freshinstpct) %>% print(n=100)

ipeds_migration_unitid_collapse %>% count(endyear) %>% print(n=50)