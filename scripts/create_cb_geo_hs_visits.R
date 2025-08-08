################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_cb_geo_hs_visits.R >
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
library(readxl)
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
#library(stars)
#library(spatstat)
#library(rgeos)
library(lwgeom) # library has checks/vixes for valid geometries
library(leaflet)
library(tidyverse)
library(formattable)
library(htmlwidgets)
library(geodist)          # super-fast great-circle distances


### DIRECTORY PATHS
data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

####### READ IN ZIP-CODE LEVEL VARIABLES FROM ACS 5-YR 2020 (DATA COLLECTION YEARS 2016-2020)
  # created by script create_acs_zcta_vars.R

  load(file.path(data_dir, 'zcta_acs20_anal.RData'))

####### RUN SCRIPT TO CREATE OBJECTS FROM PRIVATE SCHOOL NETWORK MANUSCRIPT


getwd()
source(file = file.path('scripts', 'create_event_hs_data.R'))
getwd()

  # NOTE: THE RACIAL PERCENT VARIABLESN FOR BOTH PUBLIC HS AND PRIVATE HS ARE BASED ON TOTAL NUMBER OF STUDENTS AT THE SCHOOL; NOT BASED ON 12TH GRADE RACIAL PERCENT

# this script was originally made from the recruiting chapter repo
setwd(dir = file.path('.','..','cb_geo_visits'))
getwd()

privhs_df %>% mutate(zip_len = str_length(zip5)) %>% count(zip_len)

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

univ_info %>% glimpse()

# remove special education 
  # hardly any visits there. but a couple of schools that should be moved to regular.
  # public high schools
    # schools to be reclassified as "regular schools"
      # 120168001836 2017-09-06 PINE VIEW SCHOOL. this is a school for the gifted, also a magnet: https://en.wikipedia.org/wiki/Pine_View_School_for_the_Gifted
    # schools to be reclassified as "alternative education school"
      # 220003601499 2017-09-12 New Orleans Center for Creative Arts; https://en.wikipedia.org/wiki/New_Orleans_Center_for_Creative_Arts
      # 220001701975 2017-11-08 Louisiana School for Math Science & the Arts. also a magnet https://en.wikipedia.org/wiki/Louisiana_School_for_Math,_Science,_and_the_Arts

    pubhs_df %>% glimpse()
    pubhs_df %>% count(school_type)
    
    # save the original factor levels
    pub_lvls <- levels(pubhs_df$school_type)     # "regular school" … "alternative education school"
    pub_lvls
    
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

    
    pubhs_df %>% count(school_type)
  # private high schools
  privhs_df %>% glimpse()
  privhs_df %>% count(school_type)
  
  # 2. add the two extra private-school levels you need
  new_lvls <- c(pub_lvls, "montessori", "special program emphasis")
  
  privhs_df <- privhs_df %>% 
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
  
  # remove recruiting visits to public and private high schools with school_type = "special education school"
    # public hs
    events_pubhs <- events_df %>% filter(event_type == 'pub_hs') %>% 
      left_join(
        y = pubhs_df %>% select(ncessch,school_type),
        by = c('school_id' = 'ncessch')
      ) 
    events_pubhs %>% count(school_type)
    events_pubhs <- events_pubhs %>% filter(school_type != 'special education school') # %>% select(-school_type)
    events_pubhs %>% count(school_type)
  
    # private hs
    events_privhs <- events_df %>% filter(event_type == 'priv_hs') %>% 
      left_join(
        y = privhs_df %>% select(ncessch,school_type),
        by = c('school_id' = 'ncessch')
      ) 
    
    events_privhs %>% count(school_type)
    events_privhs <- events_privhs %>% filter(school_type != 'special education school') # %>% select(-school_type)
    events_privhs %>% count(school_type)
    
    # recreate events_df
    rm(events_df)
    events_df <- bind_rows(events_pubhs, events_privhs) %>%  select(-school_type)
    rm(events_privhs,events_pubhs)
    
  # filter out observations in the pubhs_df and the privhs_df that have school type == "special education school"
  pubhs_df %>% count(school_type)
  pubhs_df <- pubhs_df %>% filter(school_type != 'special education school')
  pubhs_df %>% count(school_type)
  
  privhs_df %>% count(school_type)
  privhs_df <- privhs_df %>% filter(school_type != 'special education school')
  privhs_df %>% count(school_type)
  
# Mike Hurwitz school
# wyoming seminary in PA
# https://michaeldhurwitz.com/
# temp_pubhs %>% filter(str_match(str_to_lower('wyoming seminary')))) %>% arrange(school_id,event_date) %>% select(school_id,event_date,sch_name,school_type,magnet01, state_code,univ_state) %>% print(n=300)

  privhs_df %>% glimpse()
  pubhs_df %>% glimpse()
  pubhs_df %>% count(niche_school_type)

# CREATE HS DATAFRAME THAT HAS PUBLIC AND PRIVATE
  
  ###### select which zip-code level measures to assignto high schools by zip code
  
  zcta_acs20_anal %>% glimpse()
  
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
  
  zcta_acs20_anal_v2 %>% glimpse()
  
  zcta_acs20_anal_v2  %>% group_by(hs_zip5) %>% summarize(n_per_id = n()) %>% ungroup %>% count(n_per_id) # zip code uniquely identifies obs
  
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
  
  pubprivhs_df %>% glimpse()
  
  pubprivhs_df %>% count(hs_zip_acs20_merge)
  
  
# create university level df that has desired variable names
  univ_info %>% glimpse()

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

  univ_df %>% glimpse()
  
  rm(univ_info)

###### RUN STUFF TO CREATE GEOMARKET/CENSUS DATA FROM CB_GEO PROJECT

setwd(dir = file.path('.','..','cb_geo'))
getwd()

# Load data

source(file.path('scripts', 'map_functions.R'))


# Map functions

create_rq1_map <- function(metros, shared_legend = F) {
  js <- read_file(file.path(scripts_dir, 'rq1_maps.js'))
  
  choices <- list(region_choices = regions_data %>% filter(region %in% metros) %>% select(region, region_name, latitude, longitude))
  
  highlight_shp <- highlightOptions(weight = 2, color = '#606060', dashArray = '', bringToFront = T, sendToBack = T)
  
  yrs <- c(1980, 2000, 2020)
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) # %>%
  
  # addEasyButton(easyButton(
  #   icon = 'fa-globe', title = 'Select Metro Area',
  #   onClick = JS("function(btn, map){ $('.custom-control').not('#metro-control').slideUp(); $('#metro-control').slideToggle(); }")))
  
  for (metro in metros) {
    print(metro)
    
    region <- regions_data %>% filter(region == metro)
    eps <- eps_data %>% filter(eps %in% region$eps[[1]])
    tract <- tract_data %>% filter(eps %in% region$eps[[1]])
    
    m <- m %>%
      
      # Metro outline
      addPolygons(data = eps %>% filter(year == 2020), opacity = 1, color = 'purple', fillOpacity = 0, weight = 2, label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b>') %>% lapply(htmltools::HTML), group = 'MSA', options = c(className = paste0('metro-shape metro-line metro-', metro)))
    
    for (y in yrs) {
      m <- m %>%
        
        # EPS outline
        addPolylines(data = eps %>% filter(year == y), opacity = 1, color = 'purple', weight = 2, options = c(className = paste0('metro-shape metro-', metro, ' level-tract year-', y)))
    }
    
    for (v in names(base_vars)) {
      
      if (shared_legend && v != 'sum_tot_all') {
        color_pal_shared <- get_palette(v, c(eps[[v]], tract[[v]]), 'tract')
        color_pal_eps <- color_pal_shared
        color_pal_tract <- color_pal_shared
      } else {
        color_pal_eps <- get_palette(v, eps[[v]], 'eps')
        color_pal_tract <- get_palette(v, tract[[v]], 'tract')
      }
      
      group_name <- if_else(str_detect(base_vars[[v]]$name, 'Hispanic'), base_vars[[v]]$name, paste0('MSA by ', base_vars[[v]]$name))
      
      for (y in yrs) {
        m <- m %>% 
          
          # Shapes
          addPolygons(data = eps %>% filter(year == y), opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_eps$palette(get(v)), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b><br>', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro, ' level-eps year-', y))) %>% 
          addPolygons(data = tract %>% filter(year == y), opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_tract$palette(get(v)), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b><br><b>Tract ', tract_code, '</b>: ', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro, ' level-tract year-', y)))
      }
      
      class_name <- paste0('info legend legend-', base_vars[[v]]$abbrev, '-', metro)
      
      m <- m %>%
        addLegend(data = eps,
                  position = 'topright', pal = color_pal_eps$palette, values = ~get(v),
                  title = base_vars[[v]]$name,
                  className = paste0(class_name, '-eps'),
                  labFormat = color_pal_eps$label_format,
                  na.label = 'N/A',
                  opacity = 1) %>%
        
        addLegend(data = tract,
                  position = 'topright', pal = color_pal_tract$palette, values = ~get(v),
                  title = base_vars[[v]]$name,
                  className = paste0(class_name, '-tract'),
                  labFormat = color_pal_tract$label_format,
                  na.label = 'N/A',
                  opacity = 1)
    }
  }
  
  m %>% 
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', flatten_chr(map(names(base_vars), \(x) if_else(str_detect(base_vars[[x]]$name, 'Hispanic'), base_vars[[x]]$name, str_c('MSA by ', base_vars[[x]]$name))))),
      options = layersControlOptions(collapsed = F)
    ) %>% 
    htmlwidgets::onRender(js, choices)
}

# Run/save maps

names(all_codes)

#create_rq1_map(c('bay_area'))

#create_rq1_map(c('philadelphia', 'chicago'))

#create_rq1_map(c('philadelphia'))
#source(file.path('scripts', 'directories.R'))
#source(file.path(scripts_dir, 'append_census.R'))
#source(file.path(scripts_dir, 'metro_eps_codes.R'))

#create_rq1_map(c('dc_maryland_virginia'))

setwd(dir = file.path('.','..','cb_geo_visits'))
getwd()

all_codes

# how to replicate objects for specific metro eps codes
all_codes$los_angeles
all_codes$los_angeles$eps
los_angeles_eps_codes

# remove un-needed objects

  # remove objects for specific metro eps codes
  rm(atl_eps_codes,bay_area_eps_codes,boston_eps_codes,ca_eps_codes,chi_eps_codes,cleveland_eps_codes,dallas_eps_codes,detroit_eps_codes,dmv_eps_codes,houston_eps_codes,los_angeles_eps_codes,miami_eps_codes,nj_eps_codes,nj_north_metro_eps_codes,nyny_metro_eps_codes,orange_county_eps_codes,philly_eps_codes,san_diego_eps_codes,socal_eps_codes)
  rm(long_island_eps_codes)

  # remove file path objects that are for cb_geo repo
  rm(acs2020_data_dir,analysis_data_dir,d1980_data_dir,d2000_data_dir,shape_dir,eps_data_dir)
  rm(graphs_dir,tables_dir)

  # remove un-needed data frames
  base_vars %>% glimpse()
  rm(base_vars)
  
  eps_data %>% glimpse() # might need this later. I dunno...
  rm(eps_data)
  
  regions_data %>% glimpse() # might need this later. I dunno...
  rm(regions_data)
  
  tract_data %>% glimpse()
  tract_data %>% as_tibble() %>% count(year) # might need this later. I dunno...
  rm(tract_data)
  
allyr_anal_tract_sf %>% glimpse()

###### ADD GEOMARKET TO HIGH SCHOOL DATA AND TO UNIVERSITY DATA
# WILL NEED CRYSTAL TO CHECK HERE.
# ARE WE USING THE UPDATED EPS MAPS? 
# CHECK SCHOOLS/UNIVERSITIES THAT DO NOT HAVE AN EPS CODE AND FIGURE OUT HOW TO PROCEED
# (see below) pubprivhs_sf %>% count(is.na(hs_eps))           # how many schools did NOT match a geomarket? 5

# ADD GEOMARKET TO HIGH SCHOOL DATA 

# ── 1. Prep the 2020 EPS polygons (WGS-84 CRS) ─────────────────────

eps20 <- allyr_anal_eps_sf %>%          # master polygons
  filter(year == 2020) %>%              # keep one vintage
  st_transform(4326) %>%                # force long/lat CRS
  select(eps, eps_name)                 # retain only ID + name

eps20 %>% glimpse()
eps20 %>% class()

# ── 2. Convert high schools to sf POINT layer ──────────────────────

pubprivhs_df %>% glimpse()

pubprivhs_sf <- pubprivhs_df %>% 
  select(hs_ncessch,hs_latitude,hs_longitude) %>% 
  filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>%  # drop missing coords
  st_as_sf(coords = c("hs_longitude", "hs_latitude"), 
           crs = 4326, remove = FALSE)                   # keep numeric columns

pubprivhs_sf %>% glimpse()
pubprivhs_sf %>% class()

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

pubprivhs_sf %>% glimpse()

# ── 5. Quick sanity-check ──────────────────────────────────────────
pubprivhs_sf %>% count(is.na(hs_eps))           # how many schools did NOT match a geomarket? 5

# merge eps vars back to full high school-level data

  # add these vars
  pubprivhs_sf %>% select(hs_ncessch,hs_geometry,hs_eps,hs_eps_name,hs_eps_codename) %>% glimpse()
  
  pubprivhs_df <- pubprivhs_df %>% inner_join(
    y = pubprivhs_sf %>% select(hs_ncessch,hs_geometry,hs_eps,hs_eps_name,hs_eps_codename),
    by = c('hs_ncessch')
  ) 

  pubprivhs_df %>% glimpse()
  pubprivhs_df %>% class()
  
  rm(pubprivhs_sf)
  
# ADD GEOMARKET TO UNIVERSITY DATA 

# ── 1. Prep the 2020 EPS polygons (WGS-84 CRS) ─────────────────────

eps20 %>% glimpse()
eps20 %>% class()

# ── 2. Convert high schools to sf POINT layer ──────────────────────

univ_df %>% glimpse()

univ_sf <- univ_df %>% 
  select(univ_id,univ_latitude,univ_longitude) %>% 
  filter(!is.na(univ_latitude) & !is.na(univ_longitude)) %>%  # drop missing coords
  st_as_sf(coords = c("univ_longitude", "univ_latitude"), 
           crs = 4326, remove = FALSE)                   # keep numeric columns

univ_sf %>% glimpse()
univ_sf %>% class()

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

univ_sf %>% glimpse()

# ── 5. Quick sanity-check ──────────────────────────────────────────
univ_sf %>% count(is.na(univ_eps))           # how many schools did NOT match a geomarket? 0

# merge eps vars back to univ_df dataset

# add these vars
univ_sf %>% select(univ_id,univ_geometry,univ_eps,univ_eps_name,univ_eps_codename) %>% glimpse()

univ_df <- univ_df %>% inner_join(
  y = univ_sf %>% select(univ_id,univ_geometry,univ_eps,univ_eps_name,univ_eps_codename),
  by = c('univ_id')
) 

univ_df %>% glimpse()
univ_df %>% class()
rm(univ_sf)

###### CREATE DATAFRAME THAT HAS ONE OBS PER HIGH SCHOOL i AND UNIVERSITY j
  
# create dataframe that has one obs per high school i and university j
  pubprivhs_univ_df <- pubprivhs_df %>%
    tidyr::crossing(univ_df %>% select(univ_id)) %>% arrange(univ_id,hs_ncessch)
  
  pubprivhs_univ_df %>% glimpse()
  
  pubprivhs_df  %>% group_by(hs_ncessch) %>% summarize(n_per_id = n()) %>% ungroup %>% count(n_per_id) # uniquely identifies obs
  pubprivhs_univ_df  %>% group_by(hs_ncessch) %>% summarize(n_per_id = n()) %>% ungroup %>% count(n_per_id) # does not uniquely identifies obs
  pubprivhs_univ_df  %>% group_by(hs_ncessch,univ_id) %>% summarize(n_per_id = n()) %>% ungroup %>% count(n_per_id) # uniquely identifies obs
  
# merge desired university-level variables into pubprivhs_univ_df
  univ_df %>% glimpse()
  
  pubprivhs_univ_df <- pubprivhs_univ_df %>% inner_join(
    y = univ_df %>% select(univ_id,univ_name,univ_state_code,univ_eps_region,univ_latitude,univ_longitude,univ_classification,
                           univ_usnwr_rank,univ_eps_region,univ_geometry,univ_eps,univ_eps_name,univ_eps_codename),
    by = c('univ_id')
  ) %>% 
  # create measure of whether high school is "in-state" vs a vis the university and whether it is "in-region" vis-a-vis the university
  mutate(
    hs_univ_ineps = if_else(hs_eps == univ_eps,1,0), # high school is located in same local geomarket as the university
    hs_univ_instate = if_else(hs_state_code == univ_state_code,1,0), # high school is located in same state as university
    hs_univ_inregion = if_else(hs_eps_region == univ_eps_region,1,0), # high school is located in same EPS region as university
  ) %>% 
    # create a factor categorical variable about where is the high school in relation to the college that that has following values:
    # in same Geomarket
    # in different geomarket but same state
    # in different state but same region
    # in different region
    mutate(
      hs_univ_market = case_when(
        hs_univ_inregion == 0                                   ~ "national",
        hs_univ_inregion == 1 & hs_univ_instate == 0            ~ "regional",
        hs_univ_instate  == 1 & hs_univ_ineps   == 0            ~ "in_state",
        hs_univ_ineps    == 1                                   ~ "local",
        TRUE                                                   ~ NA_character_    # catch-all
      ),
      # turn it into an ordered factor
      hs_univ_market = factor(hs_univ_market,
                              levels = c("local", "in_state", "regional", "national"),
                              ordered = TRUE)
    ) %>% 
  # create distance between high school and university in miles
    mutate(
      # geodist_vec expects lat-lon order; divide by 1 609.344 to get miles
      hs_univ_dist = geodist::geodist_vec(
        hs_latitude,  hs_longitude,
        univ_latitude, univ_longitude,
        paired = TRUE,                # row-by-row pairing
        measure = "haversine"        # or "vincenty" / "geodesic"
      ) / 1609.344
    )  # select(hs_ncessch,hs_sch_name,hs_longitude,hs_latitude,hs_state_code,univ_id,univ_state_code,univ_longitude,univ_latitude, hs_univ_dist) %>% print(n=100)
  
  pubprivhs_univ_df %>% glimpse()

  # INVESTIGATE VARIABLES THAT INDICATE WHETHER SCHOOL AND COLLEGE ARE IN SAME GEOMARKET/STATE/REGION
  
  # 2142 missing obs. this is because 51 high schools have missing state because they are in Bureau of Indian Affairs land. 
    #51*42 = 2142
    #CRYSTAL please assign state to these 51 HS obs. this needs to be done upstream
  pubprivhs_univ_df %>% count(hs_univ_inregion) # 2142 missing obs 
    #univ_df %>% count(univ_eps_region)
    #pubprivhs_df %>% count(hs_eps_region)
  pubprivhs_univ_df %>% count(hs_univ_instate) # no missing obs
  pubprivhs_univ_df %>% count(hs_univ_ineps) # 210 missing obs. 5 high schools have missing values for EPS. 5*42 = 210. CRYSTAL find EPS code for the 5 missing obs
  
  # CRYSTAL -- AFTER ELIMINATING MISSING VALUES IN THE INPUT VARIABLES (SEE ABOVE) CHECK TO SEE IF THESE CROSSTABS LOOK RIGHT
  pubprivhs_univ_df$hs_univ_market %>% class()
  pubprivhs_univ_df %>% count(hs_univ_market)
  pubprivhs_univ_df %>% count(hs_univ_inregion,hs_univ_market)
  pubprivhs_univ_df %>% count(hs_univ_instate,hs_univ_market)
  pubprivhs_univ_df %>% count(hs_univ_ineps,hs_univ_market)
  
# create dependent variables
  
  # create data frame that has one obs per school-university and lists number of times the university visited that school. only created for school-university combinations that had at least one visit
  yvar_temp <- events_df %>% group_by(school_id,univ_id) %>% summarize(num_visits = n(), .groups = 'drop') %>% 
    # create var that always equals 1 for examining subsequent merge
    mutate(yvar_merge =1)
  yvar_temp %>% glimpse()
  yvar_temp$num_visits %>% sum() # sums to number of obs in events_df, which is right
  
  # merge yvar_temp to dataframe w/ one obs per school-university
  #pubprivhs_univ_df %>% glimpse()
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
    # data checks on yvar. looks good
    #pubprivhs_univ_df %>% count(yvar_merge)
    #pubprivhs_univ_df %>% count(visit01)
    #pubprivhs_univ_df %>% count(num_visits)
    #pubprivhs_univ_df$num_visits %>% sum()
    #pubprivhs_univ_df %>% count(yvar_merge,visit01)
    #pubprivhs_univ_df %>% count(visit01,num_visits)
    pubprivhs_univ_df %>% glimpse() 
    rm(yvar_temp)

    pubprivhs_univ_df %>% count(univ_classification)
  
