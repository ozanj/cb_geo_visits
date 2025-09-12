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
  rm(ipeds_migration_unitid_collapse,ipeds_migration_opeid_collapse)
  
ipeds_migration_non_collapse %>% glimpse()
ipeds_migration_non_collapse %>% count(endyear)

univ_info %>% glimpse()


ipeds_migration_non_collapse_1617 <- ipeds_migration_non_collapse %>% filter(endyear==2017) %>% 
  mutate(unitid = as.character(unitid)) %>% 
  inner_join(
    y = univ_info %>% select(univ_id,univ_abbrev,state_code) %>% rename(univ_state_code = state_code),
    by = c('unitid' = 'univ_id')
  ) %>% 
  select(-c(freshhs_bal,contains('opeid'),munitid,opeflag,stat_ef,prch_ef,idx_ef,imp_ef,lock_ef,rev_ef,ptc_ef,fice,endyear)) %>% 
  relocate(unitid,univ_abbrev,univ_state_code,freshhs_tot,freshhs_uk) %>% 
  # replace NAs with zeros for enrollment vars
  mutate(
    across(
      .cols = -c(unitid, univ_abbrev), 
      .fns  = ~ replace_na(., 0)
    )
  ) %>% 
  # check that sum of all enrollment vars always equals freshhs_tot
  rowwise() %>%
  mutate(
    total_check = sum(c_across(starts_with("freshhs_"))) - freshhs_tot == freshhs_tot
  ) %>%
  ungroup() %>% 
  # ipeds_migration_non_collapse_1617 %>% count(total_check)
  # total_check always == TRUE, so remove
  select(-c(total_check)) %>% 
  # subtract freshhs_uk (unknown) from freshhs_tot. either 0 or very small numbers for all universities
  # ipeds_migration_non_collapse_1617 %>% select(univ_abbrev,freshhs_uk) %>% print(n=50)
  mutate(freshhs_tot = freshhs_tot - freshhs_uk) %>% select(-c(freshhs_uk)) %>% 
  # add puerto rico enrollment to foreign enrollment. PR enrollment is either zero or very small numbers
  # ipeds_migration_non_collapse_1617 %>% select(univ_abbrev,freshhs_pr) %>% print(n=50)
  mutate(freshhs_for = freshhs_for + freshhs_pr) %>% select(-c(freshhs_pr)) %>% 
  # create in-state freshman enrollment
  rowwise() %>%
  mutate(
    freshhs_inst = get(paste0("freshhs_", tolower(univ_state_code)))
  ) %>%
  ungroup() %>% 
  # create university EPS region
  mutate(
    univ_eps_region = case_when(
      univ_state_code %in% c('CT','ME','MA','NH','RI','VT') ~ "new_england",
      univ_state_code %in% c('NY','PA','DE','DC','MD','NJ') ~ "middle_states",
      univ_state_code %in% c('IL','IN','IA','KS','MI','MN','MO',
                             'NE','ND','OH','SD','WV','WI') ~ "midwest",
      univ_state_code %in% c('AL','FL','GA','KY','LA','MS',
                             'NC','SC','TN','VA')           ~ "south",
      univ_state_code %in% c('AR','NM','OK','TX')           ~ "southwest",
      univ_state_code %in% c('AK','AZ','CA','CO','HI','ID','MT',
                             'NV','OR','UT','WA','WY')      ~ "west",
      TRUE ~ NA_character_
    ),
    univ_eps_region = factor(
      univ_eps_region,
      levels = c("new_england", "middle_states", "midwest", "south", "southwest", "west")
    )
  ) %>% 
  # ipeds_migration_non_collapse_1617 %>% arrange(univ_eps_region,univ_state_code) %>% select(univ_abbrev,univ_state_code,univ_eps_region) %>% print(n=50)
  # create variables of freshman enrollment by eps region
  mutate(
    freshhs_eps_new_england = rowSums(across(c(freshhs_ct, freshhs_me, freshhs_ma,
                                               freshhs_nh, freshhs_ri, freshhs_vt)), na.rm = TRUE),
    freshhs_eps_middle_states = rowSums(across(c(freshhs_ny, freshhs_pa, freshhs_de,
                                                 freshhs_dc, freshhs_md, freshhs_nj)), na.rm = TRUE),
    freshhs_eps_midwest = rowSums(across(c(freshhs_il, freshhs_in, freshhs_ia, freshhs_ks, freshhs_mi,
                                           freshhs_mn, freshhs_mo, freshhs_ne, freshhs_nd,
                                           freshhs_oh, freshhs_sd, freshhs_wv, freshhs_wi)), na.rm = TRUE),
    freshhs_eps_south = rowSums(across(c(freshhs_al, freshhs_fl, freshhs_ga, freshhs_ky, freshhs_la,
                                         freshhs_ms, freshhs_nc, freshhs_sc, freshhs_tn, freshhs_va)), na.rm = TRUE),
    freshhs_eps_southwest = rowSums(across(c(freshhs_ar, freshhs_nm, freshhs_ok, freshhs_tx)), na.rm = TRUE),
    freshhs_eps_west = rowSums(across(c(freshhs_ak, freshhs_az, freshhs_ca, freshhs_co, freshhs_hi,
                                        freshhs_id, freshhs_mt, freshhs_nv, freshhs_or, freshhs_ut,
                                        freshhs_wa, freshhs_wy)), na.rm = TRUE)
  ) %>% 
  # create a logical variable that checks whether the sum of these six variables + freshhs_for equals the value of freshhs_tot
  mutate(
    eps_region_check = (
      freshhs_eps_new_england + freshhs_eps_middle_states + freshhs_eps_midwest + freshhs_eps_south + freshhs_eps_southwest +
      freshhs_eps_west + freshhs_for
    ) == freshhs_tot
  ) %>% 
  # ipeds_migration_non_collapse_1617 %>% count(eps_region_check)
  # always true. so delete check variable
  select(-c(eps_region_check)) %>% 
  mutate(
    # create total enrollment from US
    freshhs_us = freshhs_tot - freshhs_for,
    # create domestic out-of-state enrollment
    freshhs_usoutst = freshhs_us - freshhs_inst
  ) %>% 
  # create variables of percent of total freshman enrollment for: _inst; usoutst; _for
  mutate(
    across(
      .cols = c(freshhs_for, freshhs_inst, freshhs_usoutst),
      .fns  = ~ .x / freshhs_tot * 100,
      .names = "{.col}_pct"
    )
  ) %>% 
  # create variables of percent of us domestic enrollment
  mutate(
    across(
      .cols = c(freshhs_eps_new_england,freshhs_eps_middle_states, freshhs_eps_midwest,
                freshhs_eps_south, freshhs_eps_southwest, freshhs_eps_west),
      .fns  = ~ .x / freshhs_us * 100,
      .names = "{.col}_us_pct"
    )
  ) %>% 
  # assertions
    # create variable that asserts that freshhs_usoutst+ freshhs_inst + freshhs_for == freshhs_tot
    # create variable that asserts that freshhs_usoutst_pct + freshhs_inst_pct + freshhs_for_pct == 100%
    # create variable that asserts that sum of freshhs_eps_new_england,freshhs_eps_middle_states, freshhs_eps_midwest,freshhs_eps_south, freshhs_eps_southwest, freshhs_eps_west == freshhs_us
    # create variable that asserts that sum of freshhs_eps_new_england_us_pct,freshhs_eps_middle_states_us_pct, freshhs_eps_midwest_us_pct,freshhs_eps_south_us_pct, freshhs_eps_southwest_us_pct, freshhs_eps_west_us_pct == 100%
  mutate(
    # does in-state + out-of-state + foreign equal total?
    check_tot_parts = near(freshhs_usoutst + freshhs_inst + freshhs_for,
                           freshhs_tot),
    
    # do the corresponding percentages sum to 100?
    check_tot_pct = near(freshhs_usoutst_pct + freshhs_inst_pct + freshhs_for_pct,
                         100),
    
    # do the six EPS region counts equal total U.S. enrollment?
    check_us_parts = near(freshhs_eps_new_england + freshhs_eps_middle_states +
                            freshhs_eps_midwest + freshhs_eps_south +
                            freshhs_eps_southwest + freshhs_eps_west,
                          freshhs_us),
    
    # do the six EPS region %s sum to 100?
    check_us_pct = near(freshhs_eps_new_england_us_pct + freshhs_eps_middle_states_us_pct +
                          freshhs_eps_midwest_us_pct + freshhs_eps_south_us_pct +
                          freshhs_eps_southwest_us_pct + freshhs_eps_west_us_pct,
                        100)
  ) %>% 
  # ipeds_migration_non_collapse_1617 %>% count(check_tot_parts)
  # ipeds_migration_non_collapse_1617 %>% count(check_tot_pct)
  # ipeds_migration_non_collapse_1617 %>% count(check_us_parts)
  # ipeds_migration_non_collapse_1617 %>% count(check_us_pct)
  select(-c(check_tot_parts,check_tot_pct,check_us_parts,check_us_pct)) %>% 
  relocate(
    unitid, univ_abbrev, univ_state_code, univ_eps_region,
    matches("^freshhs_[a-z]{2}$"),   # exactly two-letter state codes
    freshhs_tot,freshhs_for, freshhs_inst, freshhs_usoutst,
    freshhs_for_pct, freshhs_inst_pct, freshhs_usoutst_pct,
    freshhs_us,
    freshhs_eps_new_england, freshhs_eps_middle_states, freshhs_eps_midwest,
    freshhs_eps_south, freshhs_eps_southwest, freshhs_eps_west,
    freshhs_eps_new_england_us_pct, freshhs_eps_middle_states_us_pct,
    freshhs_eps_midwest_us_pct, freshhs_eps_south_us_pct,
    freshhs_eps_southwest_us_pct, freshhs_eps_west_us_pct
  )

ipeds_migration_non_collapse_1617 %>% glimpse()
rm(ipeds_migration_non_collapse,univ_data,univ_info)

getwd()
save(ipeds_migration_non_collapse_1617, file = file.path('.','data','ipeds_migration','ipeds_migration_non_collapse_1617'))

rm(ipeds_migration_non_collapse_1617)

load(file = file.path('.','data','ipeds_migration','ipeds_migration_non_collapse_1617'))
