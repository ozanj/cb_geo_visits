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

# check free reduced lunch [CRYSTAL -- TRY TO DRAMATICALLY REDUCE THE NUMBER OF MISSING VALUES]
  pubprivhs_df %>% select(contains('lunch')) %>% glimpse()
  pubprivhs_df %>% filter(hs_control == 'public') %>% count(is.na(hs_pct_free_reduced_lunch))
  pubprivhs_df %>% filter(hs_control == 'public') %>% count(hs_pct_free_reduced_lunch>100)
  pubprivhs_df %>% filter(hs_control == 'public',hs_school_type == 'regular school') %>% count(is.na(hs_pct_free_reduced_lunch))

  # 1918 public HS missing hs_pct_free_reduced_lunch
  missing_lunch <- (pubprivhs_df %>% filter(hs_control == 'public') %>% filter(is.na(hs_pct_free_reduced_lunch)))$hs_ncessch
  
  # These missing lunch schools are indeed missing from 1718 lunch data, but 1629 of the 1918 are present in the 1415 lunch data
  # They are just currently not being used bc we are using 1718 other data (e.g., )
  load(file = file.path('.','data','fr_lunch_1415_1718.RData'))
  lunch_membership_1718 %>% filter(ncessch %in% missing_lunch) %>% View()
  lunch_membership_1415 %>% filter(ncessch %in% missing_lunch) %>% View()
  
####### CREATE GEOMARKET-LEVEL POPULARITY MEASURES THAT EXCLUDES SELF-SCHOOL

df_by_univ_eps %>% select(univ_id,hs_eps_codename,starts_with('n_')) %>% rename_with(~ paste0("eps_", .), starts_with("n_")) %>%  glimpse()


# merge df_by_univ_eps to pubprivhs_univ_df
rq3_df <- pubprivhs_univ_df %>% left_join(
  y = df_by_univ_eps %>% select(univ_id,hs_eps_codename,starts_with('n_')) %>% rename_with(~ paste0("eps_", .), starts_with("n_")) %>% mutate(one=1),
  by = c('univ_id','hs_eps_codename')
) %>% 
  # 5 obs per school-univ_id don't merge. these are ones that don't have hs_eps_codename
  #filter(is.na(one)) %>% count(univ_id) %>% print(n=50)
  filter(!is.na(one)) %>% select(-one)


# look at data and make sure it seems ok [DONE]
# rq3_df %>% filter(univ_id == '147767') %>% # northwestern
#   filter((hs_control == 'public' & hs_school_type == "regular school") | (hs_control == 'private' & hs_school_type %in% c('regular school','special program emphasis'))) %>% 
#   # keep schools with sufficient numbers of 12th graders
#   filter((hs_control == 'public' & hs_g12>=100) | (hs_control == 'private' & hs_g12>=50)) %>% 
#   filter(hs_state_code == 'CA') %>% 
#   arrange(hs_eps_codename,hs_control,hs_school_type,hs_ncessch) %>% select(
#     hs_eps_codename,hs_ncessch, hs_sch_name,hs_control,hs_school_type,visit01,num_visits,eps_n_sch_all,eps_n_vis01_all,eps_n_vistot_all
#   ) %>% print(n=500)

# create measures of geomarket popularity that remove self-school
rq3_df %>% glimpse()

rq3_df <- rq3_df %>% 
  # filter on school type and 12 grade enrollment size
  filter((hs_control == 'public' & hs_school_type == "regular school") | (hs_control == 'private' & hs_school_type %in% c('regular school','special program emphasis'))) %>% 
  #filter((hs_control == 'public' & hs_g12>=100) | (hs_control == 'private' & hs_g12>=50)) %>%
  mutate(
    hs_pct_bl_hisp_nat_decile = fct_relevel(hs_pct_bl_hisp_nat_decile, "D5")
  ) %>%   
  # create measures of geomarket popularity that remove self-school
  # variable names start with p for popularity
  mutate(
    # all schools
    peps_n_sch_all = eps_n_sch_all - 1,
    peps_n_vis01_all = eps_n_vis01_all - visit01,
    peps_n_vistot_all = eps_n_vistot_all - num_visits,
    peps_n_vis01_per_sch_all = peps_n_vis01_all/peps_n_sch_all,
    peps_n_vistot_per_sch_all = peps_n_vistot_all/peps_n_sch_all,
    # ── public schools ─────────────────────────────────────────────
    peps_n_sch_pub          = eps_n_sch_pub - if_else(hs_control == "public", 1L, 0L),
    peps_n_vis01_pub        = eps_n_vis01_pub - if_else(hs_control == "public", as.integer(visit01), 0L),
    peps_n_vistot_pub       = eps_n_vistot_pub - if_else(hs_control == "public", as.integer(num_visits), 0L),
    peps_n_vis01_per_sch_pub  = if_else(peps_n_sch_pub > 0, peps_n_vis01_pub  / peps_n_sch_pub,  NA_real_),
    peps_n_vistot_per_sch_pub = if_else(peps_n_sch_pub > 0, peps_n_vistot_pub / peps_n_sch_pub, NA_real_)    
  ) %>% filter(hs_control == 'public')
  # %>% 
  # filter(hs_state_code == 'IL',univ_id == '147767') %>% 
  # arrange(hs_eps_codename,hs_control,hs_school_type,hs_ncessch) %>% select(
  #   #hs_eps,hs_ncessch,hs_control,eps_n_sch_all,peps_n_sch_all,visit01,eps_n_vis01_all,peps_n_vis01_all,peps_n_vis01_per_sch_all,num_visits,eps_n_vistot_all,peps_n_vistot_all,peps_n_vistot_per_sch_all
  #   hs_eps,hs_ncessch,hs_control,eps_n_sch_pub,peps_n_sch_pub,visit01,eps_n_vis01_pub,peps_n_vis01_pub,peps_n_vis01_per_sch_pub,num_visits,eps_n_vistot_pub,peps_n_vistot_pub,peps_n_vistot_per_sch_pub
  # ) %>% print(n=500)

####### RUN REGRESSION OF VISITS TO SCHOOL I FROM COLLEGE J, ALL EPS
rq3_df %>% glimpse()

rhs_pub <- c(
  "hs_g12","hs_pct_bl_hisp_nat_decile", # "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian","hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade","hs_magnet01",
  "hs_pct_free_reduced_lunch_decile","hs_pct_prof_math","hs_pct_prof_rla",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist","peps_n_vis01_per_sch_all"
)

rhs_pub

#visit01 ~ rhs_terms | univ_id + hs_state_code + interaction(univ_id, hs_state_code)
rhs_formula_ij <- as.formula(
  paste("visit01 ~", paste(rhs_pub, collapse = " + "), "| interaction(univ_id, hs_state_code)") #  + hs_ncessch # this would add fixed effects for high school
)
# note: adding fixed effects for high school eliminates all variables that are constant within high school. 
  # note that 

rhs_formula_ij

model <- feols(
  fml = rhs_formula_ij,
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'private_libarts'), # all
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'private_national'), # all
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'public_research'), # all
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'public_research',hs_univ_market %in% c('regional','national')), # out-of-state visits
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'public_research',hs_univ_market %in% c('local','in_state')), # in-state visits
  data    = rq3_df %>% filter(univ_id != 'all',), # all
  #data    = rq3_df %>% filter(univ_id != 'all', hs_univ_market %in% c('regional','national')), # out-of-state visits
  #data    = rq3_df %>% filter(univ_id != 'all', hs_univ_market %in% c('local','in_state')), # in-state visits
  cluster = ~ hs_state_code       # <-- cluster by state
)

model %>% summary()

# model wtih fixed effects per high school
  # note: adding fixed effects for high school eliminates all variables that are constant within high school. 
  # however the variable of interest peps_n_vis01_per_sch_all remains in model and is highly significant
# rhs_formula_fe_sch_ij <- as.formula(
#   paste("visit01 ~", paste(rhs_pub, collapse = " + "), "| interaction(univ_id, hs_state_code) + hs_ncessch")
# )
# rhs_formula_fe_sch_ij
# 
# model <- feols(
#   fml = rhs_formula_fe_sch_ij,
#   data    = rq3_df %>% filter(univ_id != 'all',), # all
#   cluster = ~ hs_state_code       # <-- cluster by state
# )
# 
# model %>% summary()


# MODEL WITH RANDOM EFFECTS FOR HIGH SCHOOL.
# CAN'T DO CLUSTER ROBUST STANDARD ERRORS BECAUSE MATRIX EXCEEDS 32 GIGABYTES
# BUT IN NON-ROBUST ERRORS, VARIABLE OF INTEREST peps_n_vis01_per_sch_all IS STILL HIGHLY SIGNIFICANT
# model with random effects for high school
# library(glmmTMB)
# 
# # create compact univ × state FE index if you haven’t already
# rq3_df <- rq3_df %>%
#   mutate(univ_state_fe = interaction(univ_id, hs_state_code, drop = TRUE))
# 
# # fit LPM with random intercepts for schools + FE for univ × state
# model_lpm_re <- glmmTMB(
#   visit01 ~ hs_g12 + hs_pct_bl_hisp_nat_decile + hs_overall_niche_letter_grade +
#     hs_magnet01 + hs_pct_free_reduced_lunch_decile + hs_pct_prof_math +
#     hs_pct_prof_rla + hs_zip_inc_house_mean_decile + hs_zip_pct_edu_baplus_all_decile +
#     hs_zip_pct_pov_yes + I(hs_zip_pct_pov_yes^2) + hs_zip_pct_nhisp_black +
#     hs_zip_pct_nhisp_native + hs_zip_pct_nhisp_asian + hs_zip_pct_nhisp_nhpi +
#     hs_zip_pct_nhisp_multi + hs_zip_pct_hisp_all + hs_univ_dist +
#     peps_n_vis01_per_sch_all + hs_state_code + univ_id +
#     (1 | hs_ncessch),
#     #univ_state_fe,                       # FE for univ × state
#   data   = rq3_df %>% filter(univ_id != "all"),
#   family = gaussian()   # linear probability model
# )
# 
# model_lpm_re %>% summary()
# 
# library(clubSandwich)
# 
# mf <- model.frame(model_lpm_re)  # rows actually used in the fit
# 
# vcov_state <- vcovCR(
#   model_lpm_re,
#   cluster = mf$hs_state_code,   # must match mf rows
#   type    = "CR1S"              # lighter than CR2
# )
# 
# coef_table <- coef_test(model_lpm_re, vcov = vcov_state, test = "naive-t")
# coef_table

##############run the interaction between geomarket popularity and income decile

# --- RHS terms including interaction ---
rhs_pub_interact_ij <- c(
  "hs_g12","hs_pct_bl_hisp_nat_decile", # "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian","hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade","hs_magnet01",
  "hs_pct_free_reduced_lunch_decile","hs_pct_prof_math","hs_pct_prof_rla",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist","peps_n_vis01_per_sch_all",
  "peps_n_vis01_per_sch_all:hs_pct_free_reduced_lunch_decile" 
  #"peps_n_vis01_per_sch_all:hs_pct_bl_hisp_nat_decile" 
  # "peps_n_vis01_per_sch_all:hs_zip_inc_house_mean_decile"
)

rhs_formula_interact_ij <- as.formula(
  paste("visit01 ~", paste(rhs_pub_interact_ij, collapse = " + "), "| interaction(univ_id, hs_state_code)")
)
rhs_formula_interact_ij
#run the interaction between geomarket popularity and racial composition decile

model <- feols(
  fml = rhs_formula_interact_ij,
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'private_libarts'), # all
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'private_national'), # all
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'public_research'), # all
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'public_research',hs_univ_market %in% c('regional','national')), # out-of-state visits
  #data    = rq3_df %>% filter(univ_id != 'all', univ_classification == 'public_research',hs_univ_market %in% c('local','in_state')), # in-state visits
  data    = rq3_df %>% filter(univ_id != 'all',), # all
  #data    = rq3_df %>% filter(univ_id != 'all', hs_univ_market %in% c('regional','national')), # out-of-state visits
  #data    = rq3_df %>% filter(univ_id != 'all', hs_univ_market %in% c('local','in_state')), # in-state visits
  cluster = ~ hs_state_code       # <-- cluster by state
)

model %>% summary()

####### RUN REGRESSION SEPARATELY BY UNIVERSITIES FOR ALL EPS


# build the formula
rhs_formula <- reformulate(termlabels = rhs_pub, response = "visit01")
rhs_formula

# run the model
model <- feols(
  fml = rhs_formula,
  data    = rq3_df %>% filter(univ_id == "147767"), # replace with your dataset
  cluster = ~ hs_state_code       # <-- cluster by state
)

model %>% summary()

univ_df %>% glimpse()

######### RUN MULTIPLE UNIVERSITIES AT A TIME

library(dplyr)
library(purrr)
library(fixest)
library(broom)
library(tidyr)

# ---------------------------
# list of target universities
# ---------------------------
univ_ids <- c(
  "230959","216287","123165","120254","115409","126678","221519","204501",
  "173902","128902","167835","168342","147767","152080","201645","139658",
  "223232","160755","228246","127060","168148","239105","216597","164924",
  "228875","186867","100751","218663","139959","181464","201885","215293",
  "186380","110635","110653","126614","155317","106397","166629","110671",
  "110680","196097"
)

# --- RHS terms (your spec) ---
rhs_pub <- c(
  "hs_g12",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces","hs_pct_free_reduced_lunch",
  "hs_overall_niche_letter_grade","hs_magnet01",
  "hs_pct_prof_math","hs_pct_prof_rla",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist","peps_n_vis01_per_sch_all"
)

rhs_formula <- reformulate(termlabels = rhs_pub, response = "visit01")

# --- vectorized stars + formatter for the focal coefficient ---
stars_vec <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.1]  <- "."
  out[!is.na(p) & p < 0.05] <- "*"
  out[!is.na(p) & p < 0.01] <- "**"
  out[!is.na(p) & p < 0.001] <- "***"
  out
}

fmt_coef_vec <- function(est, se, p) {
  # produce "est (se)***" or NA when any input is NA
  is_na <- is.na(est) | is.na(se) | is.na(p)
  res <- paste0(sprintf("%.3f", est), " (", sprintf("%.3f", se), ")", stars_vec(p))
  res[is_na] <- NA_character_
  res
}

# --- run one university and pull the focal term (no univ_id column returned) ---
run_one <- function(uid) {
  dfu <- rq3_df %>% filter(univ_id == uid) %>% 
  # only if it is an out-of-state high school
  filter(hs_univ_market %in% c('regional','national'))
  
  m <- feols(
    fml     = rhs_formula,
    data    = dfu,
    cluster = ~ hs_state_code    # cluster by state
  )
  
  tid <- tidy(m) %>% filter(term == "peps_n_vis01_per_sch_all")
  
  if (nrow(tid) == 0) {
    tibble(est = NA_real_, se = NA_real_, p = NA_real_)
  } else {
    tibble(est = tid$estimate[1], se = tid$std.error[1], p = tid$p.value[1])
  }
}

# --- run for all universities in univ_df (keeps univ_df order and labels) ---
all_res <- univ_df %>%
  filter(univ_id %in% univ_ids) %>%
  select(univ_id, univ_abbrev) %>%
  mutate(res = map(
    univ_id,
    possibly(run_one, otherwise = tibble(est = NA_real_, se = NA_real_, p = NA_real_))
  )) %>%
  unnest(res)

# --- build the final 2-column table ---
table_out <- all_res %>%
  mutate(`peps_n_vis01_per_sch_all` = fmt_coef_vec(est, se, p)) %>%
  select(univ_abbrev, `peps_n_vis01_per_sch_all`)

# view the table
table_out %>% print(n=50)
# Optional pretty print:
# knitr::kable(table_out, align = c("l","r"))

######## INTERACTION EFFECT BETWEEN peps_n_vis01_per_sch_all AND HS INCOME DECILE

library(dplyr)
library(purrr)
library(fixest)
library(broom)
library(tidyr)
library(stringr)
library(forcats)

# ---------------------------
# list of target universities
# ---------------------------
univ_ids <- c(
  "230959","216287","123165","120254","115409","126678","221519","204501",
  "173902","128902","167835","168342","147767","152080","201645","139658",
  "223232","160755","228246","127060","168148","239105","216597","164924",
  "228875","186867","100751","218663","139959","181464","201885","215293",
  "186380","110635","110653","126614","155317","106397","166629","110671",
  "110680","196097"
)

# --- RHS terms including interaction ---
rhs_pub_interact <- c(
  "hs_g12",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces","hs_pct_free_reduced_lunch",
  "hs_overall_niche_letter_grade","hs_magnet01",
  "hs_pct_prof_math","hs_pct_prof_rla",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist","peps_n_vis01_per_sch_all",
  "peps_n_vis01_per_sch_all:hs_zip_inc_house_mean_decile"
)

rhs_formula_interact <- reformulate(termlabels = rhs_pub_interact, response = "visit01")

# --- formatter ---
stars_vec <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.1]   <- "."
  out[!is.na(p) & p < 0.05]  <- "*"
  out[!is.na(p) & p < 0.01]  <- "**"
  out[!is.na(p) & p < 0.001] <- "***"
  out
}

fmt_coef_vec <- function(est, se, p) {
  is_na <- is.na(est) | is.na(se) | is.na(p)
  res <- paste0(sprintf("%.3f", est), " (", sprintf("%.3f", se), ")", stars_vec(p))
  res[is_na] <- NA_character_
  res
}

# --- run one university (reference = D6) ---
run_one_interact <- function(uid) {
  dfu <- rq3_df %>% 
    filter(univ_id == uid) %>% 
    # only if it is an out-of-state high school
    filter(hs_univ_market %in% c('regional','national')) %>% 
    mutate(hs_zip_inc_house_mean_decile = fct_relevel(hs_zip_inc_house_mean_decile, "D6"))
  
  m <- feols(
    fml     = rhs_formula_interact,
    data    = dfu,
    cluster = ~ hs_state_code
  )
  
  tidy(m) %>% mutate(univ_id = uid)
}

# --- run across universities ---
all_interact_raw <- map_dfr(
  univ_ids,
  ~ tryCatch(run_one_interact(.x), error = function(e) NULL)
)

all_interact <- all_interact_raw %>%
  left_join(select(univ_df, univ_id, univ_abbrev), by = "univ_id")

# ------------------------------
# 1) MAIN EFFECT table (at D6)
# ------------------------------
main_effect_table <- all_interact %>%
  filter(term == "peps_n_vis01_per_sch_all") %>%
  transmute(
    univ_abbrev,
    `peps_n_vis01_per_sch_all_at_D6` = fmt_coef_vec(estimate, std.error, p.value)
  ) %>%
  distinct() %>%
  arrange(univ_abbrev)

# ---------------------------------------
# 2) INTERACTION table (other deciles vs D6)
# ---------------------------------------
interact_table <- all_interact %>%
  filter(str_detect(term, "peps_n_vis01_per_sch_all") &
           str_detect(term, "hs_zip_inc_house_mean_decile")) %>%
  mutate(
    decile = str_extract(term, "D[0-9]+")
  ) %>%
  transmute(
    univ_abbrev,
    decile,
    value = fmt_coef_vec(estimate, std.error, p.value)
  ) %>%
  distinct() %>%
  pivot_wider(names_from = decile, values_from = value) %>%
  arrange(univ_abbrev)

# ---------------------------------------
# MAIN EFFECTS of income decile (vs D6)
# ---------------------------------------
decile_effect_table <- all_interact %>%
  filter(str_starts(term, "hs_zip_inc_house_mean_decile") & 
           !str_detect(term, ":")) %>%        # exclude interactions
  mutate(
    decile = str_remove(term, "hs_zip_inc_house_mean_decile")
  ) %>%
  transmute(
    univ_abbrev,
    decile,
    value = fmt_coef_vec(estimate, std.error, p.value)
  ) %>%
  distinct() %>%
  pivot_wider(names_from = decile, values_from = value) %>%
  arrange(univ_abbrev)


# ----- View results -----

main_effect_table %>% print(n=50)   # effect at D6
decile_effect_table %>% print(n=50)
interact_table %>% print(n=50)       # differences for D1–D5, D7–D10 relative to D6


# Pretty print (optional):
# knitr::kable(main_effect_table, align = c("l","r"))
# knitr::kable(interact_table %>% select(univ_abbrev, D1:D5, D7:D10), align = c("l", rep("r", 9)))



####### CA SPECIFIC REGRESSION MODEL

# load school-level CA SAT data from 2016-17
load(file = file.path('.','data','ca_sat_data','ca_sat16_17.RData'))

ca_sat16_17 %>% glimpse()
pubprivhs_df %>% glimpse()


pubprivhs_univ_ca <- pubprivhs_univ_df %>% left_join(
  y = ca_sat16_17 %>% select(-sname) %>% rename(hs_g12_sat = hs_g12) %>% mutate(one = 1),
  by = c('hs_ncessch')
) %>% filter(hs_state_code == 'CA',hs_control == 'public',hs_school_type == 'regular school',hs_g12 >=100) %>% 
  select(-one) 
  


  #pubprivhs_univ_ca %>% filter(univ_abbrev == 'Emory') %>% select(hs_eps_codename,hs_sch_name,hs_free_reduced_lunch,hs_free_reduced_reg_lunch,hs_tot_students,hs_pct_free_reduced_lunch) %>% print(n=500)

pubprivhs_univ_ca %>% glimpse()
pubprivhs_univ_ca %>% filter(univ_id == 'all') %>% glimpse()

# merge in geomarket-level measures

df_by_univ_eps %>% glimpse()

pubprivhs_univ_ca %>% left_join(
  y = df_by_univ_eps %>% select(univ_id,hs_eps_codename) %>% mutate(one = 1),
  by = c('univ_id','hs_eps_codename')
) %>% select(-c(one))
  count(one)

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
