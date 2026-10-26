################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_cb_geo_hs_visits_v2.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/22/2025
## [ DESC ] < load data on HS characteristics, visits to HS, and geomarkets>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1000)
#options(width = 160)

### LIBRARIES
library(tidyverse)
library(haven)
library(sf)
options(tigris_use_cache = TRUE)
library(tigris)
library(geodist)          # super-fast great-circle distances

### DIRECTORY PATHS
data_dir <- file.path('.','data') # main data directory

####### READ IN ZIP-CODE LEVEL VARIABLES FROM ACS 5-YR 2020 (DATA COLLECTION YEARS 2016-2020)
  # created by script create_acs_zcta_vars.R

  load(file.path(data_dir, 'zcta_acs20_anal.RData'))

####### RUN SCRIPT TO CREATE OBJECTS FROM PRIVATE SCHOOL NETWORK MANUSCRIPT

# remember where we are: create_event_hs_data.R setwd()s into ../recruiting-chapter
proj_dir <- getwd()
source(file = file.path('scripts', 'create_event_hs_data.R'))
setwd(proj_dir)   # come back, regardless of what this repo's folder is named
rm(proj_dir)
#free_lunch,reduced_lunch,free_reduced_lunch,member_lunch,pct_free_lunch,pct_reduced_lunch,pct_free_reduced_lunch,
  # NOTE: THE RACIAL PERCENT VARIABLESN FOR BOTH PUBLIC HS AND PRIVATE HS ARE BASED ON TOTAL NUMBER OF STUDENTS AT THE SCHOOL; NOT BASED ON 12TH GRADE RACIAL PERCENT

#events_data, events_df, niche_data, niche_df, niche_overrides, privhs_data, privhs_data_1314, privhs_data_1516, privhs_data_1718, privhs_df, privhs_events, privhs_overrides, pubhs_data, pubhs_data_1415, pubhs_data_1718, univ_data, univ_df, univ_info, univ_ipeds, usnews_data, usnews_df 

# remove objects you don't need
#vectors

#keep

#remove vectors
rm(ccd_missing_ncessch,pss_1314_ncessch,pss_1516_ncessch,pss_missing_ncessch)

#remove functions
rm(get_pss_override,v_get_pss_override)

# remove data frame objects
rm(pubhs_data_1415,pubhs_data_1718)
rm(niche_data)
rm(niche_df)

# drop events_data because events_df has improved school id [and I *think* it drops HS that don't meet criteria but need to double-check]
rm(events_data)
rm(privhs_events)

# keep only visits to public hs or private hs
events_df <- events_df %>% filter(event_type %in% c('priv_hs','pub_hs'))

# drop unneeded datasets about private schools

rm(privhs_data,privhs_data_1314,privhs_data_1516,privhs_data_1718)
rm(niche_overrides,privhs_overrides)
# add US News rankings data to univ_info df

univ_info <- univ_info %>% left_join(y = univ_df %>% select(univ_id,type,rank) %>% rename(usnwr_type = type, usnwr_rank = rank), by=c('univ_id'))

# drop university-level data frames you don't need for now
rm(usnews_data,usnews_df,univ_ipeds,univ_data,univ_df)

# remove special education 
  # hardly any visits there. but a couple of schools that should be moved to regular.
  # public high schools
    # schools to be reclassified as "regular schools"
      # 120168001836 2017-09-06 PINE VIEW SCHOOL. this is a school for the gifted, also a magnet: https://en.wikipedia.org/wiki/Pine_View_School_for_the_Gifted
    # schools to be reclassified as "alternative education school"
      # 220003601499 2017-09-12 New Orleans Center for Creative Arts; https://en.wikipedia.org/wiki/New_Orleans_Center_for_Creative_Arts
      # 220001701975 2017-11-08 Louisiana School for Math Science & the Arts. also a magnet https://en.wikipedia.org/wiki/Louisiana_School_for_Math,_Science,_and_the_Arts

    
    # save the original factor levels
    pub_lvls <- levels(pubhs_df$school_type)     # "regular school" … "alternative education school"
    
    pubhs_df <- pubhs_df %>% 
      mutate(
        # recode selected rows
        school_type = case_when(
          ncessch == "120168001836" ~ "regular school",
          ncessch == "220003601499" ~ "alternative education school",
          ncessch == "220001701975" ~ "alternative education school",
          TRUE                      ~ as.character(school_type)   # keep existing value
        ),
        # turn it back into a factor with the original ordering
        school_type = factor(school_type, levels = pub_lvls)
      )

    
  # private high schools
  
  # 2. add the two extra private-school levels you need
  new_lvls <- c(pub_lvls, "montessori", "special program emphasis")
  
  privhs_df <- privhs_df %>% 
    # fix hs_state_code for griggs international academy; state listed as MD but should be MI
    mutate(
      state_code = if_else(ncessch == 'A1301558','MI',state_code)
    ) %>% 
    mutate(
      ## labelled → character
      school_type_chr = as.character(as_factor(school_type, levels = "default")),
      ## recode
      school_type_chr = case_when(
        school_type_chr == "Regular elementary or secondary"  ~ "regular school",
        #school_type_chr == "Montessori"                       ~ "montessori", # decision: call montessori alternative education school
        school_type_chr == "Special education"                ~ "special education school",
        school_type_chr == "Career/technical/vocational"      ~ "career and technical school",
        school_type_chr %in% c("Alternative/other","Montessori")                ~ "alternative education school",
        school_type_chr == "Special program emphasis"         ~ "special program emphasis",
        TRUE ~ school_type_chr
      ),
      ## character → factor with the full level set
      school_type = factor(school_type_chr, levels = new_lvls)
    ) %>% 
    select(-school_type_chr)    # drop helper    
  rm(pub_lvls,new_lvls)
  
  # remove recruiting visits to public and private high schools with school_type = "special education school" or 'alternative education school' or "career and technical school"
    # public hs
    events_pubhs <- events_df %>% filter(event_type == 'pub_hs') %>% 
      left_join(
        y = pubhs_df %>% select(ncessch,school_type),
        by = c('school_id' = 'ncessch')
      ) 
    events_pubhs <- events_pubhs %>% filter(!school_type %in% c("special education school","alternative education school","career and technical school"))
    
    
    # private hs
    events_privhs <- events_df %>% filter(event_type == 'priv_hs') %>% 
      left_join(
        y = privhs_df %>% select(ncessch,school_type),
        by = c('school_id' = 'ncessch')
      ) 
    
    events_privhs <- events_privhs %>% filter(!school_type %in% c("special education school","alternative education school","career and technical school"))
    
    # recreate events_df
    rm(events_df)
    events_df <- bind_rows(events_pubhs, events_privhs) %>%  select(-school_type)
    rm(events_privhs,events_pubhs)
    
  # filter out observations in the pubhs_df and the privhs_df that have school type == "special education school" or "alternative education school"
  pubhs_df <- pubhs_df %>% filter(!school_type %in% c("special education school","alternative education school","career and technical school"))
  
  privhs_df <- privhs_df %>% filter(!school_type %in% c("special education school","alternative education school","career and technical school"))
  

# CREATE HS DATAFRAME THAT HAS PUBLIC AND PRIVATE
  
  ###### select which zip-code level measures to assignto high schools by zip code
  
  
  zcta_acs20_anal_v2 <- zcta_acs20_anal %>% 
    select(geoid,inc_house_agg,households_tot,inc_house_med,inc_house_mean,inc_house_mean_calc,pov_yes,pov_denom,pct_pov_yes,
           tot_all, nhisp_all, nhisp_white, nhisp_black, nhisp_native, nhisp_asian, nhisp_nhpi, nhisp_other, nhisp_multi, 
           hisp_all, hisp_white, hisp_black, hisp_native, hisp_asian, hisp_nhpi, hisp_other, hisp_multi, nhisp_api, hisp_api,
           pct_nhisp_white, pct_nhisp_black, pct_nhisp_native, pct_nhisp_asian, pct_nhisp_nhpi, pct_nhisp_other, pct_nhisp_multi,
           pct_hisp_white, pct_hisp_black, pct_hisp_native, pct_hisp_asian, pct_hisp_nhpi, pct_hisp_other, pct_hisp_multi, pct_nhisp_api,
           pct_hisp_api, pct_hisp_all, pct_nhisp_all,
           pct_edu_baplus_all, pct_edu_baplus_white, pct_edu_baplus_black, pct_edu_baplus_native, pct_edu_baplus_asian, 
           pct_edu_baplus_nhpi, pct_edu_baplus_api, pct_edu_baplus_multi, pct_edu_baplus_hisp, pct_edu_baplus_nhisp_white) %>% 
    rename_with(~ paste0("hs_zip_", .x), .cols = -geoid) %>%
    rename(hs_zip5 = geoid)
  
  
  
  pubprivhs_df <- bind_rows(pubhs_df,privhs_df) %>%
    # variable "type" and "control" are the same
    select(-type) %>% 
    # replace varnames total_ with tot_
    rename_with(~ str_replace(.x, "^total_", "tot_"), starts_with("total_")) %>% 
    # add prefix hs_ to all variable names
    rename_with(~ if_else(str_starts(.x, "hs_"),      # already prefixed? keep it
                          .x,                         # yes → leave as-is
                          paste0("hs_", .x))) %>%          # no  → add prefix
    # create eps region high school is located in
    mutate(
      hs_eps_region = case_when(
        hs_state_code %in% c('CT','ME','MA','NH','RI','VT') ~ "new_england",
        hs_state_code %in% c('NY','PA','DE','DC','MD','NJ') ~ "middle_states",
        hs_state_code %in% c('IL','IN','IA','KS','MI','MN','MO',
                             'NE','ND','OH','SD','WV','WI') ~ "midwest",
        hs_state_code %in% c('AL','FL','GA','KY','LA','MS',
                             'NC','SC','TN','VA')           ~ "south",
        hs_state_code %in% c('AR','NM','OK','TX')           ~ "southwest",
        hs_state_code %in% c('AK','AZ','CA','CO','HI','ID','MT',
                             'NV','OR','UT','WA','WY')      ~ "west",
        TRUE ~ NA_character_
      ) %>% 
        factor(levels = c("new_england",
                          "middle_states",
                          "midwest",
                          "south",
                          "southwest",
                          "west"))
    ) %>% 
    # exclude schools on native american reservations (state_code = BI, which is Bureau of Indian Affairs)
    #filter(!is.na(hs_eps_region)) %>% 
    # merge zip-code level measures to high school data
    left_join(
      y = zcta_acs20_anal_v2 %>% mutate(hs_zip_acs20_merge = 1),
      by = c('hs_zip5')
    ) %>% 
    mutate(hs_zip_acs20_merge = if_else(is.na(hs_zip_acs20_merge),0,hs_zip_acs20_merge))
  
    rm(zcta_acs20_anal,zcta_acs20_anal_v2)
  
  
  
  rm(pubhs_df,privhs_df)
  
# create university level df that has desired variable names

  univ_df <- univ_info %>% 
    # remove variables 
    select(-c(fips_state_code,school_url,fips_county_code,county_name,cbsa_code,locale,sector,search_sector,search_length)) %>% 
    # create eps region each university is located in
    mutate(
      eps_region = case_when(
        state_code %in% c('CT','ME','MA','NH','RI','VT') ~ "new_england",
        state_code %in% c('NY','PA','DE','DC','MD','NJ') ~ "middle_states",
        state_code %in% c('IL','IN','IA','KS','MI','MN','MO',
                             'NE','ND','OH','SD','WV','WI') ~ "midwest",
        state_code %in% c('AL','FL','GA','KY','LA','MS',
                             'NC','SC','TN','VA')           ~ "south",
        state_code %in% c('AR','NM','OK','TX')           ~ "southwest",
        state_code %in% c('AK','AZ','CA','CO','HI','ID','MT',
                             'NV','OR','UT','WA','WY')      ~ "west",
        TRUE ~ NA_character_
      ) %>% 
        factor(levels = c("new_england",
                          "middle_states",
                          "midwest",
                          "south",
                          "southwest",
                          "west"))
    ) %>%       
    # add prefix univ_ to all variable names
    rename_with(~ if_else(str_starts(.x, "univ_"),      # already prefixed? keep it
                          .x,                         # yes → leave as-is
                          paste0("univ_", .x)))          # no  → add prefix

  
  rm(univ_info)

###### LOAD 2020 GEOMARKET (EPS) SHAPES
load(file.path('.', '..', 'cb_geomarket_shape', 'analysis_data', 'allyr_anal_eps_sf.RData'))

###### ADD GEOMARKET TO HIGH SCHOOL DATA AND TO UNIVERSITY DATA
# WILL NEED CRYSTAL TO CHECK HERE.
# ARE WE USING THE UPDATED EPS MAPS? 
# CHECK SCHOOLS/UNIVERSITIES THAT DO NOT HAVE AN EPS CODE AND FIGURE OUT HOW TO PROCEED

# ADD GEOMARKET TO HIGH SCHOOL DATA 

# ── 1. Prep the 2020 EPS polygons (WGS-84 CRS) ─────────────────────

eps20 <- allyr_anal_eps_sf %>%          # master polygons
  filter(year == 2020) %>%              # keep one vintage
  st_transform(4326) %>%                # force long/lat CRS
  select(eps, eps_name)                 # retain only ID + name

# ── 2. Convert high schools to sf POINT layer ──────────────────────

pubprivhs_sf <- pubprivhs_df %>% 
  select(hs_ncessch,hs_latitude,hs_longitude) %>% 
  filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>%  # drop missing coords
  st_as_sf(coords = c("hs_longitude", "hs_latitude"), 
           crs = 4326, remove = FALSE)                   # keep numeric columns

# ── 3. Spatially join points ↔ polygons ────────────────────────────
#     • st_within() ⇒ point must be strictly inside polygon  
#     • left = TRUE  ⇒ keep all schools; unmatched get NA

pubprivhs_sf <- pubprivhs_sf %>% 
  st_join(eps20, join = st_within, left = TRUE) %>% 
  # create var that appends eps + eps_name for prettier display
  mutate(eps_codename = str_c(eps,eps_name, sep = " - ")) %>% 
  # add prefix hs_ to all variable names that don't yet have it
  rename_with(~ if_else(str_starts(.x, "hs_"),      # already prefixed? keep it
                        .x,                         # yes → leave as-is
                        paste0("hs_", .x)))         # no  → add prefix

# ── 5. Quick sanity-check ──────────────────────────────────────────

# merge eps vars back to full high school-level data

  # add these vars
  
  pubprivhs_df <- pubprivhs_df %>% inner_join(
    y = pubprivhs_sf %>% select(hs_ncessch,hs_geometry,hs_eps,hs_eps_name,hs_eps_codename),
    by = c('hs_ncessch')
  ) 

  
  rm(pubprivhs_sf)
  

# ADD COUNTY TO VARIABLE TO SCHOOL DATASET
  
  # 1. Re-establish sf class on pubprivhs_df (lost during inner_join upstream)
  #    Coordinates are WGS84 — assign EPSG:4326
  pubprivhs_df <- pubprivhs_df %>%
    st_set_geometry("hs_geometry") %>%
    st_set_crs(4326)
  
  # 2. Pull county shapefile from Census TIGER (cb = TRUE = simplified boundaries,
  #    fine for point-in-polygon; only need to do this once per session)
  
  counties <- counties(cb = TRUE, resolution = "20m", year = 2020) %>%
    st_as_sf() %>%
    st_transform(4326)          # transform from NAD83 to WGS84 to match schools

  
  # 3. Quick validity check on county polygons before joining
  
  
  # 4. Spatial join — st_within keeps only points strictly inside a polygon;
  #    left = TRUE retains all schools (unmatched get NA county)
  pubprivhs_df <- pubprivhs_df %>%
    st_join(
      counties %>% select(GEOID, NAME),
      join = st_within,
      left = TRUE
    ) %>%
    rename(
      hs_county_geoid = GEOID,    # 5-digit county FIPS (state + county)
      hs_county_name  = NAME,     # county name
    )
  
  # 5. Sanity checks
  # a handful is normal (bad coords, territories)
  # investigate if > ~50  
  
  # ── INVESTIGATE THE 12 UNMATCHED SCHOOLS ──────────────────────────────────────
  
  # 1. Pull the unmatched schools and look at key fields
  #    Most common causes: territories (PR/VI/GU), bad coordinates, points on borders
  
  # ── FIX: ASSIGN COUNTY TO THE 12 UNMATCHED COASTAL/BORDER SCHOOLS ─────────────
  # All 12 schools have coordinates landing in water (bays, lakes, ocean) rather
  # than on land — st_within found no polygon beneath them. st_nearest_feature
  # snaps each point to the closest county polygon, which is correct in all cases.
  
  unmatched <- pubprivhs_df %>%
    filter(is.na(hs_county_geoid))
  
  nearest_idx <- st_nearest_feature(unmatched, counties)
  
  unmatched <- unmatched %>%
    mutate(
      hs_county_geoid = counties$GEOID[nearest_idx],
      hs_county_name  = counties$NAME[nearest_idx]
    )
  
  # Patch the fixed values back into the main dataframe
  # rows_update() doesn't work well with sf objects
  # Use match() to locate the 12 rows by ncessch and overwrite directly
  
  idx <- match(unmatched$hs_ncessch, pubprivhs_df$hs_ncessch)
  
  pubprivhs_df$hs_county_geoid[idx] <- unmatched$hs_county_geoid
  pubprivhs_df$hs_county_name[idx]  <- unmatched$hs_county_name
  
  # Confirm zero NAs remain

  #drop temporary objects
  rm(idx,nearest_idx,unmatched)
  
# ADD GEOMARKET TO UNIVERSITY DATA 

# ── 1. Prep the 2020 EPS polygons (WGS-84 CRS) ─────────────────────

# ── 2. Convert high schools to sf POINT layer ──────────────────────

univ_sf <- univ_df %>% 
  select(univ_id,univ_latitude,univ_longitude) %>% 
  filter(!is.na(univ_latitude) & !is.na(univ_longitude)) %>%  # drop missing coords
  st_as_sf(coords = c("univ_longitude", "univ_latitude"), 
           crs = 4326, remove = FALSE)                   # keep numeric columns

# ── 3. Spatially join points ↔ polygons ────────────────────────────
#     • st_within() ⇒ point must be strictly inside polygon  
#     • left = TRUE  ⇒ keep all schools; unmatched get NA

univ_sf <- univ_sf %>% 
  st_join(eps20, join = st_within, left = TRUE) %>% 
  # create var that appends eps + eps_name for prettier display
  mutate(eps_codename = str_c(eps,eps_name, sep = " - ")) %>% 
  # add prefix univ_ to all variable names that don't yet have it
  rename_with(~ if_else(str_starts(.x, "univ_"),      # already prefixed? keep it
                        .x,                         # yes → leave as-is
                        paste0("univ_", .x)))         # no  → add prefix

# ── 5. Quick sanity-check ──────────────────────────────────────────

# merge eps vars back to univ_df dataset

# add these vars

univ_df <- univ_df %>% inner_join(
  y = univ_sf %>% select(univ_id,univ_geometry,univ_eps,univ_eps_name,univ_eps_codename),
  by = c('univ_id')
) 

rm(univ_sf)

### ADD COUNTY GEOID AND NAME TO univ_df

# ── ADD COUNTY TO UNIVERSITY DATA ─────────────────────────────────────────────

# 1. Re-establish sf class on univ_df (lost during inner_join upstream, same
#    issue as pubprivhs_df). Coordinates are WGS84 — assign EPSG:4326.
univ_df <- univ_df %>%
  st_set_geometry("univ_geometry") %>%
  st_set_crs(4326)

# counties is already loaded and transformed to WGS84 from the hs steps above
# no need to reload or re-transform

# 2. Spatial join — st_within keeps only points strictly inside a polygon;
#    left = TRUE retains all universities (unmatched get NA county)
univ_df <- univ_df %>%
  st_join(
    counties %>% select(GEOID, NAME),
    join = st_within,
    left = TRUE
  ) %>%
  rename(
    univ_county_fips = GEOID,   # 5-digit county FIPS — use this for fixed effects
    univ_county_name = NAME     # human-readable name — keep for verification
  )

# 3. Sanity checks

# 5. Fix any unmatched with nearest-feature fallback
unmatched_univ <- univ_df %>%
  filter(is.na(univ_county_fips))

if (nrow(unmatched_univ) > 0) {
  nearest_idx <- st_nearest_feature(unmatched_univ, counties)
  unmatched_univ <- unmatched_univ %>%
    mutate(
      univ_county_fips = counties$GEOID[nearest_idx],
      univ_county_name = counties$NAME[nearest_idx]
    )
  idx <- match(unmatched_univ$univ_id, univ_df$univ_id)
  univ_df$univ_county_fips[idx] <- unmatched_univ$univ_county_fips
  univ_df$univ_county_name[idx] <- unmatched_univ$univ_county_name
}

# 6. Confirm zero NAs remain

###### CREATE DATAFRAME THAT HAS ONE OBS PER HIGH SCHOOL i AND UNIVERSITY j
  
# create dataframe that has one obs per high school i and university j
  pubprivhs_univ_df <- pubprivhs_df %>%
    tidyr::crossing(univ_df %>% select(univ_id)) %>% arrange(univ_id,hs_ncessch)
  
  
  
# merge desired university-level variables into pubprivhs_univ_df
  
  pubprivhs_univ_df <- pubprivhs_univ_df %>% inner_join(
    y = univ_df %>% select(univ_id,univ_name,univ_abbrev,univ_state_code,univ_eps_region,univ_latitude,univ_longitude,univ_classification,
                           univ_usnwr_rank,univ_eps_region,univ_geometry,univ_eps,univ_eps_name,univ_eps_codename,univ_county_fips,univ_county_name),
    by = c('univ_id')
  ) %>% 
    mutate(
      hs_univ_ineps = if_else(hs_eps == univ_eps,1,0), # high school is located in same local geomarket as the university
      hs_univ_instate = if_else(hs_state_code == univ_state_code,1,0), # high school is located in same state as university 
      hs_univ_inregion = if_else(hs_eps_region == univ_eps_region,1,0), # high school is located in same EPS region as university 
      ) %>% 
        # create a factor categorical variable about where is the high school in relation to the college that that has following values: 
        # in same Geomarket # in different geomarket but same state 
        # in different state but same region 
        # in different region 
    mutate( 
      hs_univ_market = case_when( 
        hs_univ_inregion == 0 ~ "national", 
        hs_univ_inregion == 1 & hs_univ_instate == 0 ~ "regional", 
        hs_univ_instate == 1 & hs_univ_ineps == 0 ~ "in_state", 
        hs_univ_ineps == 1 ~ "local", 
        TRUE ~ NA_character_ # catch-all 
        ), 
      # turn it into an ordered factor 
      hs_univ_market = factor(hs_univ_market, 
                              levels = c("local", "in_state", "regional", "national"), 
                              ordered = TRUE) 
      ) %>%  # create distance between high school and university in miles
    mutate(
      # geodist_vec expects lat-lon order; divide by 1 609.344 to get miles
      hs_univ_dist = geodist::geodist_vec(
        hs_latitude,  hs_longitude,
        univ_latitude, univ_longitude,
        paired = TRUE,                # row-by-row pairing
        measure = "haversine"        # or "vincenty" / "geodesic"
      ) / 1609.344
    )  # select(hs_ncessch,hs_sch_name,hs_longitude,hs_latitude,hs_state_code,univ_id,univ_state_code,univ_longitude,univ_latitude, hs_univ_dist) %>% print(n=100)
  

  
# create dependent variables
  
  # create data frame that has one obs per school-university and lists number of times the university visited that school. only created for school-university combinations that had at least one visit
  yvar_temp <- events_df %>% group_by(school_id,univ_id) %>% summarize(num_visits = n(), .groups = 'drop') %>% 
    # create var that always equals 1 for examining subsequent merge
    mutate(yvar_merge =1)
  
  # merge yvar_temp to dataframe w/ one obs per school-university
  pubprivhs_univ_df <- pubprivhs_univ_df %>% 
    left_join(
    y = yvar_temp,
    by = c('hs_ncessch' = 'school_id', 'univ_id')
    ) %>% 
    # create indicators of 0/1 got a visit and number of visits
    mutate(
      visit01 = if_else(is.na(yvar_merge),0,yvar_merge),
      num_visits = if_else(is.na(num_visits),0,num_visits)
    )
    rm(yvar_temp)

# CREATE TOTALS ACROSS ALL UNIVERSITIES AND ADD IT TO pubprivhs_univ_df
pubprivhs_all_vis <- pubprivhs_univ_df %>%
  group_by(hs_ncessch) %>%
  summarise(
    # all visits, by all colleges
    num_visits_all   = sum(coalesce(num_visits, 0), na.rm = TRUE),
    visit01_all      = sum(coalesce(visit01, 0), na.rm = TRUE),
    # in-state visits, by all colleges
    num_visits_inst = sum(if_else(hs_univ_market %in% c("local", "in_state"), coalesce(num_visits, 0), 0, missing = 0), na.rm = TRUE),
    visit01_inst    = sum(if_else(hs_univ_market %in% c("local", "in_state"), coalesce(visit01, 0), 0, missing = 0), na.rm = TRUE),
    # in-state visits, by public universities
    num_visits_inst_pubu = sum(if_else((hs_univ_market %in% c("local", "in_state") & univ_classification == 'public_research'), coalesce(num_visits, 0), 0, missing = 0), na.rm = TRUE),
    visit01_inst_pubu    = sum(if_else((hs_univ_market %in% c("local", "in_state") & univ_classification == 'public_research'), coalesce(visit01, 0), 0, missing = 0), na.rm = TRUE),
    # out-state
    num_visits_outst = sum(if_else(hs_univ_market %in% c("regional", "national"), coalesce(num_visits, 0), 0, missing = 0), na.rm = TRUE),
    visit01_outst    = sum(if_else(hs_univ_market %in% c("regional", "national"), coalesce(visit01, 0), 0, missing = 0), na.rm = TRUE),
    # regional
    num_visits_reg = sum(if_else(hs_univ_market %in% c("regional"), coalesce(num_visits, 0), 0, missing = 0), na.rm = TRUE),
    visit01_reg    = sum(if_else(hs_univ_market %in% c("regional"), coalesce(visit01, 0), 0, missing = 0), na.rm = TRUE),
    # national
    num_visits_nat = sum(if_else(hs_univ_market %in% c("national"), coalesce(num_visits, 0), 0, missing = 0), na.rm = TRUE),
    visit01_nat    = sum(if_else(hs_univ_market %in% c("national"), coalesce(visit01, 0), 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # merge in rest of the variables from the high school dataset
  inner_join(
    y = pubprivhs_df,
    by = c('hs_ncessch')
  ) %>% 
  # create some variables for university
  mutate(
    univ_id = 'all',
    univ_name = 'all',
    univ_abbrev = 'all',
    univ_classification = 'all'
  )

# append pubprivhs_univ_df and pubprivhs_all_vis
pubprivhs_univ_df <- dplyr::bind_rows(pubprivhs_univ_df, pubprivhs_all_vis)

# 1051522/43 # = 24454

rm(pubprivhs_all_vis)

pubprivhs_univ_df <- pubprivhs_univ_df %>% 
  mutate(
    # percent of students in hs who are black, hispanic, american indian or NHPI
    hs_pct_bl_hisp_nat = (hs_tot_amerindian + hs_tot_black + hs_tot_nativehawaii + hs_tot_hispanic)/hs_tot_students*100
  ) %>% 
  # string variables should be changed to factor variables. make this change upstream
  mutate(
    #hs_pct_free_reduced_lunch = hs_free_reduced_lunch/hs_tot_students*100,
    hs_school_type = factor(hs_school_type),
    hs_magnet01 = factor(hs_magnet01),
    hs_eps = factor(hs_eps),
    hs_eps_name = factor(hs_eps_name),
    hs_eps_codename = factor(hs_eps_codename),
    hs_state_code = factor(hs_state_code) %>% relevel(ref = "CT"), # make CT the reference group state
    hs_control = factor(hs_control,
                        levels = c("public", "private")),
    hs_overall_niche_letter_grade = case_when(
      is.na(hs_overall_niche_letter_grade) ~ "unrank_na",
      hs_overall_niche_letter_grade == "Unranked" ~ "unrank_na",
      TRUE ~ hs_overall_niche_letter_grade
    ),
    hs_overall_niche_letter_grade = factor(
      hs_overall_niche_letter_grade,
      levels = c(
        "unrank_na", "A+", "A", "A-", 
        "B+", "B", "B-", 
        "C+", "C", "C-"
      )
    )    
  ) %>% 
  # make decile variables for hs_zip SES variables
  mutate(
    hs_zip_inc_house_mean_decile = factor(
      cut(
        hs_zip_inc_house_mean,
        breaks = quantile(hs_zip_inc_house_mean, probs = seq(0, 1, 0.1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = paste0("D", 1:10)
      )
    ),
    hs_zip_pct_edu_baplus_all_decile = factor(
      cut(
        hs_zip_pct_edu_baplus_all,
        breaks = quantile(hs_zip_pct_edu_baplus_all, probs = seq(0, 1, 0.1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = paste0("D", 1:10)
      )
    ),
    hs_zip_pct_pov_yes_decile = factor(
      cut(
        hs_zip_pct_pov_yes,
        breaks = quantile(hs_zip_pct_pov_yes, probs = seq(0, 1, 0.1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = paste0("D", 1:10)
      )
    ),
    hs_pct_free_reduced_lunch_decile = factor(
      cut(
        hs_pct_free_reduced_lunch,
        breaks = quantile(hs_pct_free_reduced_lunch, probs = seq(0, 1, 0.1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = paste0("D", 1:10)
      )
    ),
    # make decile variable for percent free reduced lunch
    hs_pct_bl_hisp_nat_decile = factor(
      cut(
        hs_pct_bl_hisp_nat,
        breaks = quantile(hs_pct_bl_hisp_nat, probs = seq(0, 1, 0.1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = paste0("D", 1:10)
      )
    ),
    
  ) %>%   
  # create indicator of whether any high schools in the state received a visit
  # need to check on creation of this variable
    group_by(univ_id, hs_state_code) %>%
    mutate(
      univ_state_any_visit = as.integer(any(visit01 == 1, na.rm = TRUE)),  # 0/1
      univ_state_n_visit   = sum(num_visits, na.rm = TRUE),              # count
      state_n_schools      = dplyr::n()                                    # count
    ) %>%
    ungroup()

