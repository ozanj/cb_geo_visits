################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < rq3.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/22/2025
## [ DESC ] < post-modeling descriptive RQs, RQ3>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1500)
#options(width = 160)

library(tidyverse)
library(forcats)
library(scales)

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

##################################
##################################
##################################

# RQ3: DOES BEING IN AN ADVANTAGED VS. DISADVANTAGED GEOMARKET AFFECT THE PROBABILITY OF RECEIVING A VISIT FOR ADVANTAGED VS. DISADVANTAGED SCHOOLS?

# BROADLY, THREE WAYS TO EXAMINE RQ3

# 1 = regression where y = probability of visit; y = B0 + B1*advantaged_school + B2*advantaged_geomarket + B3*advantaged_schoolXadvantaged_geomarket

# 2 = interactive map
  # potential interesting metro areas: socal [cuz lots of UCs + CA private colleges];

# 3 = simple descriptives table



# =============================================================================
# Research Questions
# =============================================================================
# Do colleges go where the absolute number of college-ready students is high—
# even when those schools are disadvantaged (low-SES or high Black/Hispanic)?
# And does that “go where the students are” logic break down when the school
# sits in a low-visit geomarket?

# =============================================================================
# The Three Levers in the Question
# =============================================================================
# 1) Pipeline (efficiency): the count of students who pass the exit exam
#    (an absolute “potential yield” measure, not a rate). [school-level]
# 2) Equity (disadvantage): school composition (low-SES / high Black- or
#    Hispanic-enrollment). [school-level]
# 3) Place (gate): the geomarket’s visit richness (high- vs low-visit), i.e.,
#    whether the market is already on colleges’ recruiting circuits.[geo-market level]

# -----------------------------------------------------------------------------
# Focal Group
# -----------------------------------------------------------------------------
# Intersection: Disadvantaged + High-Pipeline schools
# (“high-volume, high-need” schools).

# =============================================================================
# Contrasts to Test
# =============================================================================
# A) Equity penalty at fixed pipeline:
#    Among high-pipeline schools, is the visit probability lower for
#    disadvantaged schools than for advantaged ones? [table-setting RQ]
#
# B) Place gate:
#    For those same high-pipeline disadvantaged schools, is visit probability
#    much higher if they’re in high-visit geomarkets than in low-visit ones?
#    (If yes, geomarkets act as a gate that can mute efficiency-based targeting.)
#     [RQ of interest]
#
# C) Who “rescues” whom:
#    Do local/regional colleges still reach high-pipeline disadvantaged schools
#    in low-visit markets, while national colleges don’t? [supplemental RQ]

# =============================================================================
# Simple Mental Model (2 × 2 × 2)
# =============================================================================
# Axis A: High-pipeline vs not
# Axis B: Advantaged vs Disadvantaged
# Axis C: High-visit geomarket vs Low-visit geomarket
#
# Main comparison cell:
#   High-pipeline × Disadvantaged, compared across High-visit vs Low-visit
#   geomarkets, and relative to High-pipeline × Advantaged.

# =============================================================================
# What Different Findings Would Mean
# =============================================================================
# • Pure efficiency:
#   Visits track pipeline counts strongly everywhere; equity and place matter
#   little → colleges go where the students are.
#
# • Place-gated efficiency:
#   Pipeline matters only in high-visit markets → being on the recruiting
#   circuit is a prerequisite.
#
# • Equity penalty:
#   Even at the same pipeline, disadvantaged schools get fewer visits →
#   bias/segmentation beyond pure efficiency.
#
# • Local rescue:
#   Equity penalty shrinks for local colleges but persists for national →
#   information/friction story.

# =============================================================================
# Bottom Line
# =============================================================================
# Test whether absolute student opportunity can overcome disadvantage and
# place-based frictions, or whether geomarket targeting and school composition
# still suppress visits—even when the payoff (lots of ready students) is high.



##################################
##################################
##################################

pubprivhs_univ_df %>% glimpse()

allyr_anal_eps_sf %>% filter(year == 2020) %>% glimpse()

# one observation per recruiting event
events_df %>% glimpse()

# one observation per high school

pubprivhs_df %>% count(hs_school_type)

# one observation per college (42 colleges)

univ_df %>% glimpse()

all_codes
# start with high-school level data and either CU Boulder (126614) or Colorado college (126678); and public HS

df_la <- pubprivhs_univ_df %>% 
  filter(univ_id == '126614', hs_control == 'public', hs_state_code == 'CA', hs_school_type == 'regular school') %>% 
  filter(hs_eps %in% c(all_codes$los_angeles$eps,all_codes$orange_county$eps,all_codes$san_diego$eps))

  #df_la %>% filter(hs_school_type == 'alternative education school') %>% count(is.na(hs_num_prof_math)) # almost always missing
  df_la %>% count()
  df_la %>% count(hs_magnet01)

df_la %>% select(hs_ncessch,hs_sch_name,hs_num_prof_math) %>% print(n=30)

# number of visits
  df_la %>% count(visit01)
  df_la %>% count(num_visits)

# number missing math proficiency
  df_la %>% count(is.na(hs_num_prof_math)) # missing for 28 out of 401 obs
  
# number missing percent free-reduced lunch
  df_la %>% count(is.na(hs_pct_free_reduced_lunch)) # always non-missing
  # this variable is messed up
  
# mean income in zip code school is located in
  df_la %>% select(hs_zip_inc_house_mean,hs_zip_inc_house_mean_calc,hs_zip_inc_house_mean_decile) %>% print(n=50)
  df_la %>% count(is.na(hs_zip_inc_house_mean)) # 1 missing obs
  
# box plot of number that scored proficient on math with mean in red
  # if not already defined
  mean_math <- mean(df_la$hs_num_prof_math, na.rm = TRUE)

df_la %>%
  ggplot(aes(y = hs_num_prof_math)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.25, na.rm = TRUE) +
  geom_hline(yintercept = mean_math,
             linetype = "dashed", linewidth = 0.7, color = "red") +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  coord_cartesian(ylim = c(0, 600)) +
  labs(title = "Distribution of hs_num_prof_math across high schools",
       subtitle = "Red dashed line = mean; box middle line = median",
       y = "Number proficient in math", x = NULL) +
  theme_minimal()

# box plot of percent free reduced lunch
mean_pfrl <- mean(df_la$hs_pct_free_reduced_lunch)

ggplot(df_la, aes(y = hs_pct_free_reduced_lunch)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.25) +
  geom_hline(yintercept = mean_pfrl,
             linetype = "dotted", linewidth = 0.8, color = "red") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Percent of students on free/reduced lunch",
    subtitle = "Red dotted line = mean; box middle line = median",
    y = "Free/reduced lunch (%)", x = NULL
  ) +
  theme_minimal()

df_la %>% select(hs_free_reduced_lunch,hs_tot_students,hs_pct_free_reduced_lunch) %>% print(n = 500)

# what we have so far
  # one obs per high school
  # measure of whether school got visit [starting with visits only from CU-Boulder]
    # variables = visit01, num_visits
  # measure of academic proficiency, hs_num_prof_math
    # num scoring proficient on math
  # measure of disadvantage, hs_zip_inc_house_mean
    # [percent free-reduced lunch]
      # ideally but this variable is fucked up
    # zip_code-level mean income


# create geomarket-level data

df_la %>% 
  group_by(hs_eps_codename) %>% 
  summarize(
    n_hs = n(),
    n_hs_visited = sum(visit01==1, na.rm = TRUE)
  ) %>% 
  left_join(
    y = allyr_anal_eps_sf %>% as_tibble() %>% filter(year ==2020) %>% 
      select(eps,eps_name,pct_nhisp_all,pct_hisp_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_other,pct_nhisp_asian,pct_nhisp_nhpi,pct_nhisp_multi,pct_nhisp_api,pct_hisp_api,mean_inc_house,med_inc_house,pct_pov_yes,pct_edu_baplus_all) %>% 
      mutate(hs_eps_codename = str_c(str_trim(eps), " - ", str_trim(eps_name)) |> as_factor()) %>% select(-c(eps,eps_name)),
    by = c('hs_eps_codename')
  ) %>% select(hs_eps_codename,n_hs,n_hs_visited,mean_inc_house,med_inc_house,pct_pov_yes,pct_edu_baplus_all) %>% print(n=15)
  
# next step -- merge geomarket level data back to high school level data
  # add the prefix eps to all geomarket level variables from allyr_anal_eps_sf
  # create 0/1 variable of whether the school and the geomarket have median income greater than 100k

df_la %>% 
  
# for each high school, want to know 

allyr_anal_eps_sf %>% as_tibble() %>% filter(year ==2020) %>% 
  select(eps,eps_name,pct_nhisp_all,pct_hisp_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_other,pct_nhisp_asian,pct_nhisp_nhpi,pct_nhisp_multi,pct_nhisp_api,pct_hisp_api,mean_inc_house,med_inc_house,pct_pov_yes,pct_edu_baplus_all) %>% 
  mutate(hs_eps_codename = str_c(str_trim(eps), " - ", str_trim(eps_name)) |> as_factor()) %>% select(-c(eps,eps_name)) %>% 
  glimpse()


##############
############## df socal; here making one obs per high school-univ + merged in geomarket level measures
##############

# DATA LEVELS FOR EACH SORT OF ANALYSIS THAT CAN BE USED TO ANSWER RQ3

  # The interactive map is Gemarket level layers plus high school level layers. 
  # The regression modeling is high school level data with some Geo market level variables
  # The descriptive table is Geo market level data with some high school level variables aggregated to the geomarket level 

df_socal <- pubprivhs_univ_df %>% 
  # socal
  filter(hs_eps %in% c(all_codes$los_angeles$eps,all_codes$orange_county$eps,all_codes$san_diego$eps)) %>% 
  # merge geomarket level data
  left_join(
    y = allyr_anal_eps_sf %>% as_tibble() %>% filter(year ==2020) %>% 
      select(eps,eps_name,pct_nhisp_all,pct_hisp_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_other,pct_nhisp_asian,pct_nhisp_nhpi,pct_nhisp_multi,pct_nhisp_api,pct_hisp_api,mean_inc_house,med_inc_house,pct_pov_yes,pct_edu_baplus_all) %>% 
      mutate(hs_eps_codename = str_c(str_trim(eps), " - ", str_trim(eps_name)) |> as_factor()) %>% select(-c(eps,eps_name)),
    by = c('hs_eps_codename')
  ) %>%  
  # create other geomarket-level measures
  group_by(univ_id,hs_eps_codename) %>% summarize(
    eps_n_sch_all = n(),
    eps_n_sch_pub = sum(hs_control =='public'),
    eps_n_sch_priv = sum(hs_control =='private'),
    eps_n_sch_all_vis01 = sum(visit01==1, na.rm = TRUE),
    eps_n_sch_pub_vis01 = sum(hs_control =='public' & visit01==1, na.rm = TRUE),
    eps_n_sch_priv_vis01 = sum(hs_control =='private' & visit01==1, na.rm = TRUE)
  ) %>% ungroup()


################
################ START HERE ON 9/3/2025. THIS DATA FRAME SHOULD HAVE ALL DESIRED GEOMARKET LEVEL VARIABLES. OR AT LEAST MOST OF THEM
################

df_by_univ_eps %>% 
  mutate(
  state_abbr = str_extract(hs_eps_codename, "^[A-Z]{2}"),
  eps_num    = str_extract(hs_eps_codename, "\\d+"),
  hs_eps     = if_else(as.integer(eps_num) >= 10,
                       paste0(state_abbr, eps_num),   # e.g., "CA10"
                       paste(state_abbr, eps_num))     # e.g., "CA 9"
  ) %>%
  select(-state_abbr, -eps_num) %>% 
  filter(hs_eps %in% c(all_codes$los_angeles$eps,all_codes$orange_county$eps,all_codes$san_diego$eps)) %>% 
  filter(univ_id == 'all') %>% 
  # all schools
  select(hs_eps_codename,n_sch_all,n_vis01_all,n_vis01_per_sch_all,n_sch_pub,n_vis01_pub,n_vis01_per_sch_pub,n_sch_priv,n_vis01_priv,n_vis01_per_sch_priv)
  # national
  #select(hs_eps_codename,n_sch_all_national,n_vis01_all_national,n_vis01_per_sch_all_national,n_sch_pub_national,n_vis01_pub_national,n_vis01_per_sch_pub_national,n_sch_priv_national,n_vis01_priv_national,n_vis01_per_sch_priv_national)
  # in-state
  #select(hs_eps_codename,n_sch_all_instate,n_vis01_all_instate,n_vis01_per_sch_all_instate,n_sch_pub_instate,n_vis01_pub_instate,n_vis01_per_sch_pub_instate,n_sch_priv_instate,n_vis01_priv_instate,n_vis01_per_sch_priv_instate)

pubprivhs_univ_df %>% glimpse()

pubprivhs_univ_df %>% 
  filter(univ_id == 'all') %>% 
  mutate(
    hs_pct_blnbr = hs_pct_black + hs_pct_hispanic,
  ) %>% 
  rename(niche =hs_overall_niche_letter_grade) %>% 
  filter(hs_eps %in% c(all_codes$los_angeles$eps,all_codes$orange_county$eps,all_codes$san_diego$eps)) %>%  # note: high schools are unique! %>% group_by(hs_ncessch) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group)
  arrange(univ_id,hs_eps_codename,hs_control,hs_school_type) %>% 
  select(hs_eps_codename,hs_sch_name,hs_control,hs_school_type,hs_magnet01,hs_g11,hs_num_prof_math,niche,hs_zip_inc_house_med,hs_pct_blnbr,visit01_all,visit01_inst_pubu,visit01_inst,visit01_outst) %>% 
  print(n=980)




###############
############### NEXT STEP IS TO MAKE SUPER SWEET MAPS THAT HELP YOU VISUALIZE ALL THIS
###############

allyr_anal_eps_sf %>% glimpse()
allyr_anal_tract_sf %>% glimpse()
pubprivhs_df %>% glimpse()
events_df  %>% glimpse()

pubprivhs_df %>% st_crs()
eps20 %>% glimpse()

create_eps_map <- function(
    eps_codes,
    mapping_year  = 2020,
    univ_ids      = NULL,
    eps_sf_data   = allyr_anal_eps_sf,
    tract_sf_data = allyr_anal_tract_sf,
    hs_data       = pubprivhs_df,
    events_data   = events_df,
    eps20_data    = NULL            # optional: your 2020 EPS frame (eps, eps_name, geometry)
) {
  # ── 0. Safety checks ─────────────────────────────────────────────
  stopifnot(length(eps_codes) > 0)
  
  # Normalize EPS codes once (handles "CA10" vs "CA 10")
  fmt_eps <- function(x) sub("^([A-Z]{2}) ?(\\d+)$", "\\1 \\2", x)
  eps_codes <- fmt_eps(eps_codes)
  
  # ── 0a. Normalize tract median-income column name ────────────────
  # Expect inc_house_med; if not, patch from common alternatives.
  if (!"inc_house_med" %in% names(tract_sf_data)) {
    if ("med_inc_house" %in% names(tract_sf_data)) {
      tract_sf_data <- dplyr::rename(tract_sf_data, inc_house_med = med_inc_house)
    } else if ("med_inc_house_med_all" %in% names(tract_sf_data)) {
      tract_sf_data <- dplyr::rename(tract_sf_data, inc_house_med = med_inc_house_med_all)
    } else {
      stop("Could not find a median income column. Expected 'inc_house_med' or 'med_inc_house'.")
    }
  }
  
  # ── 1. EPS polygons (prefer eps20 for 2020) ──────────────────────
  if (!is.null(eps20_data) && mapping_year == 2020) {
    eps_sf <- eps20_data %>%
      dplyr::mutate(eps = fmt_eps(eps)) %>%
      dplyr::filter(eps %in% eps_codes) %>%
      dplyr::select(eps, eps_name) %>%
      sf::st_transform(4326)
  } else {
    eps_sf <- eps_sf_data %>%
      dplyr::mutate(eps = fmt_eps(eps)) %>%
      dplyr::filter(eps %in% eps_codes, year == mapping_year) %>%
      dplyr::select(eps, dplyr::any_of("eps_name")) %>%
      sf::st_transform(4326)
    
    # If eps_name missing, backfill from eps20 when available
    if (!"eps_name" %in% names(eps_sf) || all(is.na(eps_sf$eps_name))) {
      if (!is.null(eps20_data)) {
        eps_sf <- eps_sf %>%
          dplyr::left_join(
            eps20_data %>%
              dplyr::mutate(eps = fmt_eps(eps)) %>%
              dplyr::select(eps, eps_name20 = eps_name),
            by = "eps"
          ) %>%
          dplyr::mutate(eps_name = dplyr::coalesce(eps_name, eps_name20)) %>%
          dplyr::select(-eps_name20)
      }
    }
  }
  
  if (nrow(eps_sf) == 0) stop("No EPS polygons found after filtering. Check eps_codes/year.")
  
  # ── 2. High-school master layer ──────────────────────────────────
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
    dplyr::rename(
      hs_total_12  = hs_g12
      #hs_state_code = state_code,
      #hs_sch_name  = sch_name,
      #hs_control   = control
    ) %>%
    sf::st_as_sf(sf_column_name = "hs_geometry", crs = 4326) %>%
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
  
  # ── 3. Visits inside EPS (university-filter aware) ───────────────
  events_pts <- events_data %>% 
    rename(hs_ncessch = school_id) %>% 
    # grab school latitude and longitude
    left_join(
      y = hs_data %>% select(hs_ncessch,hs_geometry),
      by = c('hs_ncessch')
    ) %>% 
    #dplyr::filter(!is.na(hs_latitude) & !is.na(hs_longitude)) %>%
    { if (!is.null(univ_ids)) dplyr::filter(., univ_id %in% univ_ids) else . } %>%
    sf::st_as_sf(sf_column_name = "hs_geometry", crs = 4326) %>%
    sf::st_join(eps_sf, join = sf::st_within, left = FALSE) %>%
    dplyr::mutate(event_date = lubridate::ymd(event_date)) %>% 
    # grab university info
    left_join(
      y = univ_df %>% select(univ_id,univ_name),
      by = c('univ_id')
    )
  
  # ── 4. Collapse visits → one row / school ────────────────────────
  visits_summary <- events_pts %>%
    #dplyr::rename(hs_ncessch = school_id) %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(hs_ncessch, event_date) %>%
    dplyr::group_by(hs_ncessch) %>%
    dplyr::summarise(
      n_visits    = dplyr::n(),
      visits_html = paste(
        sprintf("%s: %s",
                format(event_date, "%Y-%m-%d"), univ_name),
        collapse = "<br>"
      ),
      .groups = "drop"
    )
  
  # ── 5. Merge school + visit info ─────────────────────────────────
  hs_map <- hs_pts %>%
    dplyr::left_join(visits_summary, by = "hs_ncessch") %>%
    dplyr::mutate(
      visited    = !is.na(n_visits),
      popup_html = ifelse(
        visited,
        paste0(label_html, "<br><hr style='margin:2px 0;'>", visits_html),
        label_html
      )
    ) %>%
    # 🔽 Force hs_control factor levels here
    dplyr::mutate(
      hs_control = factor(hs_control, levels = c("public", "private"))
    )
  
  visited_pts   <- dplyr::filter(hs_map,  visited)
  unvisited_pts <- dplyr::filter(hs_map, !visited)
  
  # ── 6. Census-tract median-income layer ──────────────────────────
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
  
  # ── 7. Helper palettes & halo-radius ─────────────────────────────
  circle_pal <- leaflet::colorFactor(
    palette = c("#00aa00", "#cc0000"),   # first = public, second = private
    domain  = c("public", "private")
  )
  halo_rad <- function(n) sqrt(n) * 6
  
  # ── 8. Leaflet map ───────────────────────────────────────────────
  leaflet::leaflet(eps_sf) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    
    # EPS borders
    leaflet::addPolygons(
      fillColor = "transparent",
      color     = "#444444", weight = 2,
      label     = ~eps,
      group     = "EPS borders"
    ) %>%
    
    # 8a. Income choropleth
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
    
    # 8b–d. High-school markers
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
    
    # 9. Controls & legends
    leaflet::addLayersControl(
      overlayGroups = c("High schools", "Median income", "EPS borders"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addLegend(
      position = "bottomleft",
      pal      = circle_pal,
      values   = hs_map$hs_control,
      title    = "Large = visited · Small = not visited\nHalo size ∝ # visits",
      group    = "High schools"
    )
}

create_eps_map(c(all_codes$los_angeles$eps,all_codes$orange_county$eps,all_codes$san_diego$eps))  # all colleges


# 3. Several universities
create_eps_map(
  eps_codes = all_codes$houston$eps,
  univ_ids  = c("168148", "216597", "164924")                         # Tufts, Villanova, Boston College
)

create_eps_map(all_codes$chicago$eps, univ_ids = 204501)  # oberlin
create_eps_map(all_codes$chicago$eps)  # all colleges



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





