################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_cb_geo_hs_visits.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/22/2025
## [ DESC ] < load data on HS characteristics, visits to HS, and geomarkets>
################################################################################

### SETTINGS
rm(list = ls())
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

    pubhs_data %>% glimpse()
    pubhs_data %>% count(school_type)
    
    # save the original factor levels
    pub_lvls <- levels(pubhs_data$school_type)     # "regular school" … "alternative education school"
    pub_lvls
    
    pubhs_df <- pubhs_data %>% 
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
    rm(pubhs_data)
    
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

  pubhs_df %>% glimpse()

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

create_rq1_map(c('bay_area'))

create_rq1_map(c('philadelphia', 'chicago'))

create_rq1_map(c('philadelphia'))
#source(file.path('scripts', 'directories.R'))
#source(file.path(scripts_dir, 'append_census.R'))
#source(file.path(scripts_dir, 'metro_eps_codes.R'))

create_rq1_map(c('dc_maryland_virginia'))

setwd(dir = file.path('.','..','cb_geo_visits'))
getwd()


base_vars
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
  
  eps_data %>% glimpse() # might need this later. I dunno...
  rm(eps_data)
  
  regions_data %>% glimpse() # might need this later. I dunno...
  rm(regions_data)
  
  tract_data %>% glimpse()
  tract_data %>% as_tibble() %>% count(year) # might need this later. I dunno...
  rm(tract_data)
  
allyr_anal_tract_sf %>% glimpse()


# order of operations
  # left-merge events_df to univ_info
  # left-merge events_df to pubhs_data
  # left-merge events_df to privhs_data
  # then merge to dataset that has tract + geomarket data

events_df %>% glimpse()
univ_info %>% count(classification)
univ_info %>% glimpse()

events_df2 <- events_df %>% 
  left_join(
    y = univ_info %>% select(univ_id,univ_name,latitude,longitude,classification,usnwr_type,usnwr_rank), 
    by = c('univ_id')
  ) %>% 
  rename(univ_latitude = latitude, univ_longitude = longitude, univ_usnwr_type = usnwr_type, univ_usnwr_rank = usnwr_rank, univ_classification = classification)
  rm(events_df)

  pubhs_df %>% glimpse()
  # public school data
    # TO DO
    # NOTE: doesn't have zip code variable; if you want to create that will have to investigate and modify the datasets that create pubhs_data in the create_igraph_objects script
    # note: add free/reduced lunch
  events_df2_pubhs <- events_df2 %>% inner_join(
    y = pubhs_df %>% select(ncessch,sch_name,state_code,g12,starts_with('pct_'),latitude,longitude,magnet01,school_type,-pct_white_cat,-pct_blacklatinxnative,-pct_blacklatinxnative_cat,zip5) %>% rename(total_12 = g12) %>% rename_with(~ paste0("hs_", .x), -1) %>%  mutate(hs_one = 1),
    by = c('school_id' = 'ncessch')
  ) %>% select(-hs_one) # note: perfect merge between event_df and pubhs_data by school_id/ncessch!

  # private school data
  # TO DO
  privhs_df %>% glimpse()
  events_df2_privhs <- events_df2 %>% inner_join(
    y = privhs_df %>% select(ncessch,name,zip5,latitude,longitude,overall_niche_letter_grade,rank_within_category,total_12,state_code,starts_with('pct_'),religion_5,religion,school_type,-pct_to_4yr) %>%  rename(sch_name = name, niche_grade = overall_niche_letter_grade,  niche_rank = rank_within_category) %>% rename_with(~ paste0("hs_", .x), -1) %>%  mutate(hs_one = 1),
    by = c('school_id' = 'ncessch')
  ) %>% select(-hs_one) # note: perfect merge between event_df and privhs_data by school_id/ncessch!
  
  events_df2_privhs %>% glimpse()
  
  # recreate events_df2
  events_df2 <- bind_rows(events_df2_pubhs, events_df2_privhs, .id = "source") %>% arrange(univ_id,event_date) # what .id = "source" does: If they have different column names and you want to keep all columns:
  rm(events_df2_pubhs,events_df2_privhs)

# create dataframe -- pubprivhs_data -- that appends public high schools and private high schools
  
  
  events_df2 %>% glimpse()
  

##################
################## 7/29 CREATE YVAR AND XVARS FOR STACKED LOGISTIC REGRESSION
################## START HERE END OF JULY 2025!!!!!
  
  # observations: 
    # x_i,j = school i gets visit from college j 
  # y-var = number of visits that school i gets from college j [*]
  # X-vars
    # geomarket [*]
    # region [consistent w/ EPS] [*]
    # 0/1 whether college j is in same state as school i [*]
    # local SES characteristic vars
      # zip code median income  [*]
      # zip code educational attainment  [*]
    # high school vars    
      # distance from college [*]
      # grade 12 enrollment [*]
        # or should it be grade 11? [*]
      # racial composition of student body [*]
      # public vs. private [*]
      # school rank/grade [niche] [*]
    # public school only vars
      # magnet
      # school type
      # % free-reduced lunch
      # public school rank/grade
      # some measure of test scores???
    # private school only vars
      # religious affiliation
      # private school rank/grade
      
  pubhs_df %>% glimpse()
  
  # MIDDLE STATES REGION	
  # 'NY', 'PA', 'DE', 'DC', 'MD', 'NJ'
  # MIDWESTERN REGION	
  # 'IL', 'IN', 'IA', 'KS', 'MI', 'MN', 'MO', 'NE', 'ND', 'OH', 'SD', 'WV', 'WI'
  # NEW ENGLAND	
  # 'CT', 'ME', 'MA', 'NH', 'RI', 'VT'
  # SOUTH	
  # 'AL', 'FL', 'GA', 'KY', 'LA', 'MS', 'NC', 'SC', 'TN', 'VA'
  # SOUTHWEST	
  # 'AR', 'NM', 'OK', 'TX'
  # WEST	
  # 'AK', 'AZ', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'OR', 'UT', 'WA', 'WY'

  
  pubhs_df %>%
  # create EPS region high school is located in
  mutate(hs_eps_region = case_when(
    state_code %in% c()
  )
              )
privhs_df %>% glimpse()
pubhs_df %>% glimpse()
  privhs_df %>% count(school_type)
  

############
############ descriptive statistics of visits by Geomarket
############

# table layout
  # rows = geomarkets
  # columns = variables
    # high school characteristics
      # number of public high schools
      # number of private high schools
    # create next
      # total 12th grade enrollment
      # total public school enrollment
      # total private school enrollment
  
# data steps
  # start with pubprivhs_data
  # merge in geomarket using spatial join
  
allyr_anal_eps_sf %>% filter(year == 2020) %>% select(eps,geometry,eps_name) %>% glimpse()

### CREATE DATASET WITH ALL HIGH SCHOOLS THAT HAS GEOMARKET

# ── 1. Prep the 2020 EPS polygons (WGS-84 CRS) ─────────────────────
eps20 <- allyr_anal_eps_sf %>%          # master polygons
  filter(year == 2020) %>%              # keep one vintage
  st_transform(4326) %>%                # force long/lat CRS
  select(eps, eps_name)                 # retain only ID + name

eps20 %>% glimpse()

# ── 2. Convert high schools to sf POINT layer ──────────────────────
pubprivhs_sf <- pubprivhs_data %>% 
  filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>%  # drop missing coords
  st_as_sf(coords = c("hs_longitude", "hs_latitude"), 
           crs = 4326, remove = FALSE)                   # keep numeric columns

pubprivhs_sf %>% glimpse()
pubprivhs_sf %>% class()

# ── 3. Spatially join points ↔ polygons ────────────────────────────
#     • st_within() ⇒ point must be strictly inside polygon  
#     • left = TRUE  ⇒ keep all schools; unmatched get NA
pubprivhs_sf <- pubprivhs_sf %>% 
  st_join(eps20, join = st_within, left = TRUE) 

pubprivhs_sf <- pubprivhs_sf %>%  mutate(eps_codename = str_c(eps,eps_name, sep = " - "))

pubprivhs_sf %>% glimpse()
# ── 5. Quick sanity-check ──────────────────────────────────────────
pubprivhs_sf %>% 
  count(is.na(eps))           # how many schools did NOT match a geomarket?

############## CREATE A DATASET THAT ADDS GEOMARKET TO DATAFRAME OF VISITS TO PUBLIC AND PRIVATE HIGH SCHOOLS

events_df2 %>% glimpse()
eps20 %>% glimpse()

# ── 2. Convert high schools to sf POINT layer ──────────────────────
events_sf <- events_df2 %>% 
  filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>%  # drop missing coords
  st_as_sf(coords = c("hs_longitude", "hs_latitude"), 
           crs = 4326, remove = FALSE)                   # keep numeric columns

# ── 3. Spatially join points ↔ polygons ────────────────────────────
#     • st_within() ⇒ point must be strictly inside polygon  
#     • left = TRUE  ⇒ keep all schools; unmatched get NA
events_sf <- events_sf %>% 
  st_join(eps20, join = st_within, left = TRUE) 

events_sf <- events_sf %>%  mutate(eps_codename = str_c(eps,eps_name, sep = " - "))

events_sf %>% glimpse()
# ── 5. Quick sanity-check ──────────────────────────────────────────
events_sf %>% 
  count(is.na(eps))           # how many schools did NOT match a geomarket?

##############


geo_hs_visit_table <- function(geo_filter_expr) {
  
  df_hs <- pubprivhs_sf %>%
    st_drop_geometry() %>%
    filter({{ geo_filter_expr }}) %>%
    mutate(hs_control = factor(hs_control, levels = c("pub", "priv"))) %>%
    rename(hs_12 = hs_total_12) %>%
    group_by(eps_codename, hs_control) %>%
    summarise(
      n = n(),
      across(
        hs_12,
        .fns = list(
          s = ~sum(.x, na.rm = TRUE),
          m = ~mean(.x, na.rm = TRUE),
          p25 = ~quantile(.x, 0.25, na.rm = TRUE),
          p50 = ~median(.x, na.rm = TRUE),
          p75 = ~quantile(.x, 0.75, na.rm = TRUE)
        ),
        .names = "{fn}_12"
      ),
      .groups = "drop"
    ) %>%
    pivot_wider(
      id_cols = eps_codename,
      names_from = hs_control,
      values_from = where(is.numeric),
      names_glue = "{.value}_{hs_control}"
    ) %>%
    mutate(
      n_tot = rowSums(across(c(n_pub, n_priv)), na.rm = TRUE),
      s_12 = rowSums(across(c(s_12_pub, s_12_priv)), na.rm = TRUE)
    ) %>%
    relocate(eps_codename, n_tot, n_pub, n_priv, s_12, s_12_pub, s_12_priv)
  
  df_events <- events_sf %>%
    st_drop_geometry() %>%
    filter({{ geo_filter_expr }}) %>%
    mutate(event_type = factor(event_type, levels = c("pub_hs", "priv_hs"))) %>%
    rename(
      hs_12 = hs_total_12,
      hs_control = event_type
    ) %>%
    group_by(eps_codename, hs_control) %>%
    summarise(
      n_tot = n(),
      n_lib = sum(univ_classification == "private_libarts"),
      n_privu = sum(univ_classification == "private_national"),
      n_pubu = sum(univ_classification == "public_research"),
      across(
        hs_12,
        .fns = list(
          m = ~mean(.x, na.rm = TRUE),
          p25 = ~quantile(.x, 0.25, na.rm = TRUE),
          p50 = ~median(.x, na.rm = TRUE),
          p75 = ~quantile(.x, 0.75, na.rm = TRUE)
        ),
        .names = "{fn}_12"
      ),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    complete(
      eps_codename,
      hs_control,
      fill = list(
        n_tot = 0,
        n_lib = 0,
        n_privu = 0,
        n_pubu = 0,
        m_12 = NA_real_,
        p25_12 = NA_real_,
        p50_12 = NA_real_,
        p75_12 = NA_real_
      )
    ) %>%
    pivot_wider(
      id_cols = eps_codename,
      names_from = hs_control,
      values_from = -c(eps_codename, hs_control)
    ) %>%
    mutate(
      n_tot_hs = rowSums(across(c(n_tot_pub_hs, n_tot_priv_hs)), na.rm = TRUE),
      n_lib_hs = rowSums(across(c(n_lib_pub_hs, n_lib_priv_hs)), na.rm = TRUE),
      n_privu_hs = rowSums(across(c(n_privu_pub_hs, n_privu_priv_hs)), na.rm = TRUE),
      n_pubu_hs = rowSums(across(c(n_pubu_pub_hs, n_pubu_priv_hs)), na.rm = TRUE)
    ) %>%
    relocate(eps_codename, n_tot_hs)
  
  df_hs %>%
    select(-starts_with("p50")) %>%
    left_join(
      df_events %>% select(-c(starts_with("m"), matches("p\\d{2}"))),
      by = "eps_codename"
    ) %>%
    mutate(
      vpsch = n_tot_hs / n_tot,
      vp12k = n_tot_hs / (s_12 / 1000),
      vpsch_lib = n_lib_hs / n_tot,
      vp12k_lib = n_lib_hs / (s_12 / 1000),
      vpsch_privu = n_privu_hs / n_tot,
      vp12k_privu = n_privu_hs / (s_12 / 1000),
      vpsch_pubu = n_pubu_hs / n_tot,
      vp12k_pubu = n_pubu_hs / (s_12 / 1000)
    ) %>%
    select(
      eps_codename, n_tot_hs, n_tot, s_12,
      vpsch, vp12k,
      vpsch_lib, vp12k_lib,
      vpsch_privu, vp12k_privu,
      vpsch_pubu, vp12k_pubu
    ) %>% print(n=50)
}

# NEXT STEPS:
  # MAKE A BETTTER VERSION OF THE HIGH SCHOOL VARIABLES THAT EXCLUDE HIGH SCHOOLS THAT ARE LIKE TECH VOC AND STUFF; 
    # DO THIS FOR PUBLIC HIGH SCHOOLS
    # DO THIS FOR PRIVATE HIGH SCHOOLS
    # PROBABLY NEED TO COPY THE CREATE_IGRAPH SCRIPT OVER TO THIS REPO AND CUT SHIT YOU DON'T NEED (ALL THE NETWORK STUFF) AND MODIFY AS NECESSARY
  # CHECK ON YOUR 12TH GRADE ENROLLMENT VARIABLES
    # DO THIS FOR PUBLIC
    # DO THIS FOR PRIVATE
  # ONE REASON SOME SEEMINGLY POOR GEOMARKETS HAVE HIGHER RATIO OF VISITS PER 12TH GRADER IS BECAUSE THEY HAVE HIGHER DROPOUT RATES, SO FEWER 12TH GRADERS
    # MIGHT BE BETTER TO CHOOSE VISITS PER 9TH GRADER.
    # "FAILING" DISTRICTS DON'T HAVE THAT MANY STUDENTS
    # UGH.
  

# STATES
geo_hs_visit_table(geo_filter = hs_state_code == "IL")
geo_hs_visit_table(geo_filter = hs_state_code == "CT")

# METROS
geo_hs_visit_table(geo_filter = eps %in% los_angeles_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% boston_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% nyny_metro_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% cleveland_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% detroit_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% dmv_eps_codes)

geo_hs_visit_table(geo_filter = eps %in% bay_area_eps_codes)

geo_hs_visit_table(geo_filter = eps %in% nj_north_metro_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% houston_eps_codes)
geo_hs_visit_table(geo_filter = eps %in% dallas_eps_codes)

geo_hs_visit_table(geo_filter = eps %in% long_island_eps_codes)

all_codes

geo_hs_visit_table(geo_filter = hs_state_code == "MA")

geo_hs_visit_table(geo_filter = hs_state_code == "PA")
hs_state_code == "IL"

############
############ basic mapping shit, in a function
############
  
create_eps_map <- function(
  eps_codes,
  mapping_year  = 2020,
  univ_ids      = NULL,          # ← NEW: vector of IDs (character or numeric)
  eps_sf_data   = allyr_anal_eps_sf,
  tract_sf_data = allyr_anal_tract_sf,
  hs_data       = pubprivhs_data,
  events_data   = events_df2
) {

  # ── 0. Safety checks ─────────────────────────────────────────────
  stopifnot(length(eps_codes) > 0)

  # ── 1. EPS polygons ──────────────────────────────────────────────
  eps_sf <- eps_sf_data %>% 
    dplyr::filter(eps %in% eps_codes, year == mapping_year) %>% 
    dplyr::select(eps, eps_name)

  # ── 2. High-school master layer (unchanged) ──────────────────────
  race_labels <- c(
    hs_pct_white        = "White",
    hs_pct_asian        = "Asian",
    hs_pct_hispanic     = "Hispanic / Latino",
    hs_pct_black        = "Black",
    hs_pct_amerindian   = "Am. Indian / AK Native",
    hs_pct_nativehawaii = "Nat. Hawaiian / PI",
    hs_pct_tworaces     = "Two or More Races"
  )

  hs_pts <- hs_data %>% 
    dplyr::filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>% 
    sf::st_as_sf(coords = c("hs_longitude", "hs_latitude"),
                 crs = 4326, remove = FALSE) %>% 
    sf::st_join(eps_sf, join = sf::st_within, left = FALSE) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      label_html = paste0(
        "<b>", hs_sch_name, "</b><br>",
        "12th-grade enrollment: ", format(hs_total_12, big.mark = ","), "<br><hr>",
        paste(
          purrr::map_chr(
            names(race_labels),
            ~ sprintf("• %s: %s",
                      race_labels[[.x]],
                      scales::percent(get(.x) / 100, accuracy = 0.1))
          ),
          collapse = "<br>"
        )
      )
    ) %>% 
    dplyr::ungroup()

  # ── 3. Visits inside EPS (NOW university-filter aware) ───────────
  events_pts <- events_data %>% 
    dplyr::filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>% 
    { 
      if (!is.null(univ_ids)) dplyr::filter(., univ_id %in% univ_ids)
      else .
    } %>% 
    sf::st_as_sf(coords = c("hs_longitude", "hs_latitude"),
                 crs = 4326, remove = FALSE) %>% 
    sf::st_join(eps_sf, join = sf::st_within, left = FALSE) %>% 
    dplyr::mutate(event_date = lubridate::ymd(event_date))

  # ── 4. Collapse visits → one row / school ────────────────────────
  visits_summary <- events_pts %>% 
    dplyr::rename(ncessch = school_id) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::arrange(ncessch, event_date) %>% 
    dplyr::group_by(ncessch) %>% 
    dplyr::summarise(
      n_visits    = dplyr::n(),
      visits_html = paste(
        sprintf("%s: %s",
                format(event_date, "%Y-%m-%d"), univ_name),
        collapse = "<br>"
      ),
      .groups = "drop"
    )

  # ── 5. Merge school + visit info (unchanged) ─────────────────────
  hs_map <- hs_pts %>% 
    dplyr::left_join(visits_summary, by = "ncessch") %>% 
    dplyr::mutate(
      visited    = !is.na(n_visits),
      popup_html = ifelse(
        visited,
        paste0(label_html, "<br><hr style='margin:2px 0;'>", visits_html),
        label_html
      )
    )

  visited_pts   <- dplyr::filter(hs_map,  visited)
  unvisited_pts <- dplyr::filter(hs_map, !visited)

  # ── 6. Census-tract median-income layer (unchanged) ──────────────
  tract_income <- tract_sf_data %>% 
    dplyr::filter(year == mapping_year, !is.na(inc_house_med)) %>% 
    sf::st_transform(4326) %>% 
    sf::st_filter(eps_sf, .predicate = sf::st_intersects)

  income_pal <- leaflet::colorBin(
    palette = "YlGnBu",
    domain  = tract_income$inc_house_med,
    bins    = 7,
    pretty  = TRUE
  )

  # ── 7. Helper palettes & halo-radius (unchanged) ─────────────────
  circle_pal <- leaflet::colorFactor(
    palette = c(pub = "#00aa00", priv = "#cc0000"),
    domain  = c("pub", "priv")
  )
  halo_rad <- function(n) sqrt(n) * 6

  # ── 8. Leaflet map (unchanged) ───────────────────────────────────
  leaflet::leaflet(eps_sf) %>% 
    leaflet::addProviderTiles("CartoDB.Positron") %>% 

    ## EPS borders
    leaflet::addPolygons(
      fillColor = "transparent",
      color     = "#444444", weight = 2,
      label     = ~eps,
      group     = "EPS borders"
    ) %>% 

    ## 8a. Income choropleth
    leaflet::addPolygons(
      data        = tract_income,
      fillColor   = ~income_pal(inc_house_med),
      fillOpacity = 0.6,
      color       = "#999999", weight = 0.2,
      label       = ~paste0("Median HH income: $",
                            format(inc_house_med, big.mark = ",")),
      group       = "Median income"
    ) %>% 
    leaflet::addLegend(
      position = "bottomright",
      pal      = income_pal,
      values   = tract_income$inc_house_med,
      title    = "Median HH income",
      opacity  = 0.7,
      group    = "Median income"
    ) %>% 

    ## 8b–d. High-school markers
    leaflet::addCircleMarkers(
      data        = visited_pts,
      radius      = ~halo_rad(n_visits),
      fillColor   = "transparent",
      color       = ~circle_pal(hs_control),
      weight      = 0.75,
      fillOpacity = 0,
      group       = "High schools"
    ) %>% 
    leaflet::addCircleMarkers(
      data        = visited_pts,
      radius      = 4,
      fillColor   = ~circle_pal(hs_control),
      color       = "#FFFFFF", weight = 1,
      fillOpacity = 1,
      label       = lapply(visited_pts$popup_html, htmltools::HTML),
      group       = "High schools"
    ) %>% 
    leaflet::addCircleMarkers(
      data        = unvisited_pts,
      radius      = 2,
      fillColor   = ~circle_pal(hs_control),
      color       = "#FFFFFF", weight = 1,
      fillOpacity = 1,
      label       = lapply(unvisited_pts$popup_html, htmltools::HTML),
      group       = "High schools"
    ) %>% 

    ## 9. Controls & legends
    leaflet::addLayersControl(
      overlayGroups = c("High schools", "Median income", "EPS borders"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>% 
    leaflet::addLegend(
      position = "bottomleft",
      colors   = c("#00aa00", "#cc0000"),
      labels   = c("Public", "Private"),
      title    = "Large = visited · Small = not visited\nHalo size ∝ # visits",
      group    = "High schools"
    )
}
  
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

create_eps_map(boston_eps_codes, univ_ids = 204501)  # boston, oberlin
create_eps_map(boston_eps_codes, univ_ids = 126678)  # boston, colorado college


create_eps_map(philly_eps_codes)

create_eps_map(chi_eps_codes)
  
# 1. All 42 universities (default)
create_eps_map(philly_eps_codes)

# 2. Single university
create_eps_map(philly_eps_codes, univ_ids = "230959")                 # Middlebury

# 3. Several universities
create_eps_map(
  eps_codes = houston_eps_codes,
  univ_ids  = c("168148", "216597", "164924")                         # Tufts, Villanova, Boston College
)

events_df2 %>% count(univ_name,event_type) %>% print(n=100)


############
############ some basic mapping shit, not in a function
############


library(dplyr)
library(sf)
library(lubridate)
library(leaflet)
library(htmltools)
library(scales)
library(purrr)

# ── 1. EPS polygons ───────────────────────────────────────────────
philly_sf <- allyr_anal_eps_sf %>% 
  filter(eps %in% philly_eps_codes, year == 2020) %>% select(eps,eps_name)

# ── 2. High-school master layer ───────────────────────────────────
race_labels <- c(
  hs_pct_white        = "White",
  hs_pct_asian        = "Asian",
  hs_pct_hispanic     = "Hispanic / Latino",
  hs_pct_black        = "Black",
  hs_pct_amerindian   = "Am. Indian / AK Native",
  hs_pct_nativehawaii = "Nat. Hawaiian / PI",
  hs_pct_tworaces     = "Two or More Races"
)

hs_philly <- pubprivhs_data %>% 
  filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>% 
  st_as_sf(coords = c("hs_longitude", "hs_latitude"), crs = 4326, remove = FALSE) %>% 
  st_join(philly_sf, join = st_within, left = FALSE) %>% 
  rowwise() %>% 
  mutate(
    label_html = paste0(
      "<b>", hs_sch_name, "</b><br>",
      "12th-grade enrollment: ", format(hs_total_12, big.mark = ","), "<br><hr>",
      paste(
        map_chr(
          names(race_labels),
          ~ sprintf("• %s: %s",
                    race_labels[[.x]],
                    percent(get(.x) / 100, accuracy = 0.1))
        ),
        collapse = "<br>"
      )
    )
  ) %>% 
  ungroup()

# ── 3. Visits inside EPS ──────────────────────────────────────────
events_philly <- events_df2 %>% 
  filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>% 
  st_as_sf(coords = c("hs_longitude", "hs_latitude"), crs = 4326, remove = FALSE) %>% 
  st_join(philly_sf, join = st_within, left = FALSE) %>% 
  mutate(event_date = ymd(event_date))

# ── 4. Collapse visits → one row / school (no geometry) ───────────
visits_summary <- events_philly %>% 
  rename(ncessch = school_id) %>% 
  st_drop_geometry() %>% 
  arrange(ncessch, event_date) %>% 
  group_by(ncessch) %>% 
  summarise(
    n_visits    = n(),
    visits_html = paste(
      sprintf("%s: %s",
              format(event_date, "%Y-%m-%d"), univ_name),
      collapse = "<br>"
    ),
    .groups = "drop"
  )

# ── 5. Merge school + visit info ──────────────────────────────────
hs_map <- hs_philly %>% 
  left_join(visits_summary, by = "ncessch") %>% 
  mutate(
    visited    = !is.na(n_visits),
    popup_html = ifelse(
      visited,
      paste0(label_html, "<br><hr style='margin:2px 0;'>", visits_html),
      label_html
    )
  )

visited_pts   <- hs_map %>% filter(visited)
unvisited_pts <- hs_map %>% filter(!visited)

# ── 6. Census-tract median-income layer ───────────────────────────
tract_income <- allyr_anal_tract_sf %>% 
  filter(year == 2020, !is.na(inc_house_med)) %>% 
  st_transform(4326) %>%                        # ensure same CRS
  st_filter(philly_sf, .predicate = st_intersects)   # keep tracts in EPS area

income_pal <- colorBin(
  palette = "YlGnBu",
  domain  = tract_income$inc_house_med,
  bins    = 7,
  pretty  = TRUE
)

# ── 6. Helper palettes & halo-radius   fn ───────────────────────────
circle_pal <- colorFactor(
  palette = c(pub = "#00aa00", priv = "#cc0000"),
  domain  = c("pub", "priv")
)

halo_rad <- function(n) sqrt(n) * 6        # tweak constant to taste

# ── 8. Leaflet map ────────────────────────────────────────────────
leaflet(philly_sf) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  
  # EPS borders
  addPolygons(
    fillColor = "transparent",
    color     = "#444444", weight = 2,
    label     = ~eps,
    group     = "EPS borders"
  ) %>% 
  
  # ── 8a. Census-tract median income choropleth ───────────────────
  addPolygons(
    data        = tract_income,
    fillColor   = ~income_pal(inc_house_med),
    fillOpacity = 0.6,
    color       = "#999999", weight = 0.2,
    label       = ~paste0(
      "Median HH income: $",
      format(inc_house_med, big.mark = ",")
    ),
    group       = "Median income"
  ) %>% 
  
  addLegend(
    position = "bottomright",
    pal      = income_pal,
    values   = tract_income$inc_house_med,
    title    = "Median HH income",
    opacity  = 0.7,
    group    = "Median income"
  ) %>% 
  
  # ── 8b. HALO for visited schools ────────────────────────────────
  addCircleMarkers(
    data        = visited_pts,
    radius      = ~halo_rad(n_visits),
    fillColor   = "transparent",
    color       = ~circle_pal(hs_control),
    weight      = 0.75,
    fillOpacity = 0,
    group       = "High schools"
  ) %>% 
  
  # ── 8c. Visited schools – dots ──────────────────────────────────
  addCircleMarkers(
    data        = visited_pts,
    radius      = 4,
    fillColor   = ~circle_pal(hs_control),
    color       = "#FFFFFF", weight = 1,
    fillOpacity = 1,
    label       = lapply(visited_pts$popup_html, HTML),
    group       = "High schools"
  ) %>% 
  
  # ── 8d. Unvisited schools – smaller dots ────────────────────────
  addCircleMarkers(
    data        = unvisited_pts,
    radius      = 2,
    fillColor   = ~circle_pal(hs_control),
    color       = "#FFFFFF", weight = 1,
    fillOpacity = 1,
    label       = lapply(unvisited_pts$popup_html, HTML),
    group       = "High schools"
  ) %>% 
  
  # ── 9. Layer control & legends ──────────────────────────────────
  addLayersControl(
    overlayGroups = c("High schools", "Median income", "EPS borders"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  
  addLegend(
    position = "bottomleft",
    colors   = c("#00aa00", "#cc0000"),
    labels   = c("Public", "Private"),
    title    = "Large = visited · Small = not visited\nHalo size ∝ # visits",
    group    = "High schools"
  )

