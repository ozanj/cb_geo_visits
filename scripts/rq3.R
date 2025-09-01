################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < rq3.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/22/2025
## [ DESC ] < post-modeling descriptive RQs, RQ3>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1000)
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
#    (an absolute “potential yield” measure, not a rate).
# 2) Equity (disadvantage): school composition (low-SES / high Black- or
#    Hispanic-enrollment).
# 3) Place (gate): the geomarket’s visit richness (high- vs low-visit), i.e.,
#    whether the market is already on colleges’ recruiting circuits.

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
#    disadvantaged schools than for advantaged ones?
#
# B) Place gate:
#    For those same high-pipeline disadvantaged schools, is visit probability
#    much higher if they’re in high-visit geomarkets than in low-visit ones?
#    (If yes, geomarkets act as a gate that can mute efficiency-based targeting.)
#
# C) Who “rescues” whom:
#    Do local/regional colleges still reach high-pipeline disadvantaged schools
#    in low-visit markets, while national colleges don’t?

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
################ START HERE ON 9/2/2025. THIS DATA FRAME SHOULD HAVE ALL DESIRED GEOMARKET LEVEL VARIABLES. OR AT LEAST MOST OF THEM
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
  glimpse()



df_by_univ_eps %>% count(hs_eps_codename) %>% print(n=100)

df_socal %>% glimpse() #756 obs = 18 geomarkets X 42 colleges
  print(n=200)
  

df_socal %>% group_by(hs_ncessch,univ_id) %>% summarize(n_per_group=n()) %>% ungroup() %>% count(n_per_group) # one per group
df_socal %>% count(univ_id)
