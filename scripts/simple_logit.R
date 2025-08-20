################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < simple_logit.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/8/2025
## [ DESC ] < logistic regression of probability that school i gets visit, separate model for each college>
################################################################################

### SETTINGS
rm(list = ls())
options(max.print=1000)
#options(width = 160)

library(lme4)
library(performance)
library(tidyverse)
library(pscl)

####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES


getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()

# remove objects from cb_geo (ajs manuscript) mapping
rm(create_rq1_map,format_vars,get_palette)

############

# START BUILDING LOGISTIC REGRESSION MODEL; CAN START WITH CODE FROM eda.R

pubprivhs_univ_df %>% filter(hs_control == 'public') %>% glimpse()
pubprivhs_univ_df %>% select(hs_tot_students)

pubprivhs_univ_df %>% count(hs_school_type)

#'hs_magnet01','hs_school_type','hs_pct_free_reduced_lunch','hs_pct_prof_math','hs_pct_prof_rla',

# THIS VARIABLE IS USUALLY MISSING FOR PUBLIC SCHOOLS
hs_free_reduced_reg_lunch
pubprivhs_univ_df %>% filter(univ_id == 128902, hs_control == 'public') %>% filter( is.na(hs_free_reduced_reg_lunch)) %>% count()
pubprivhs_univ_df %>% filter(univ_id == 128902, hs_control == 'public') %>% filter(!is.na(hs_free_reduced_reg_lunch)) %>% count() # lotta missing!!!!!!


pubprivhs_univ_df %>% filter(univ_id == 128902, hs_control == 'public') %>% filter( is.na(hs_free_reduced_lunch)) %>% count()
pubprivhs_univ_df %>% filter(univ_id == 128902, hs_control == 'public') %>% filter(!is.na(hs_free_reduced_lunch)) %>% count() # lotta missing!!!!!!


pubprivhs_univ_df %>% filter(univ_id == 128902, hs_control == 'public') %>% filter( is.na(hs_pct_free_reduced_lunch)) %>% count()
pubprivhs_univ_df %>% filter(univ_id == 128902, hs_control == 'public') %>% filter(!is.na(hs_pct_free_reduced_lunch)) %>% count() # lotta missing!!!!!!


# create test data frame
pubprivhs_univ_df <- pubprivhs_univ_df %>% 
  # string variables should be changed to factor variables. make this change upstream
  mutate(
    hs_pct_free_reduced_lunch = hs_free_reduced_lunch/hs_tot_students*100,
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
  )

# create indicator of whether any high schools in the state received a visit
  # need to check on creation of this variable
pubprivhs_univ_df <- pubprivhs_univ_df %>%
  group_by(univ_id, hs_state_code) %>%
  mutate(
    univ_state_any_visit = as.integer(any(visit01 == 1, na.rm = TRUE)),  # 0/1
    univ_state_n_visit   = sum(num_visits, na.rm = TRUE),              # count
    state_n_schools      = dplyr::n()                                    # count
  ) %>%
  ungroup()

# some checks that this variable was created correctly

#pubprivhs_univ_df %>% count(univ_id, hs_state_code, univ_state_any_visit) %>% arrange(univ_id, hs_state_code) %>% print(n=100)

# total visits across all states for this university
#pubprivhs_univ_df %>% filter(univ_id == "100751") %>% distinct(hs_state_code, univ_state_n_visit) %>%   # avoid double-counting per-state totals
  #summarise(total_univ_state_n_visit = sum(univ_state_n_visit, na.rm = TRUE))

#pubprivhs_univ_df %>% filter(univ_id == "100751") %>% summarise(total_from_raw = sum(num_visits, na.rm = TRUE))

pubprivhs_univ_df %>% filter(univ_id == 128902) %>% count()
pubprivhs_univ_df %>% filter(univ_id == 128902) %>% count(univ_state_any_visit)
pubprivhs_univ_df %>% filter(univ_id == 128902) %>% count(hs_state_code,univ_state_any_visit) %>% print(n=60)

## NULL MODEL
  #mod_null <- glm(visit01 ~ 1, data = pubprivhs_univ_df %>% filter(univ_id == 128902), family = binomial)
  #summary(mod_null)
  #pR2(mod_null)   # McFadden, Cox & Snell, Nagelkerke

  # To interpret: convert log-odds intercept to probability
  #pvisit_null <- plogis(coef(mod_null)[1])
  #pvisit_null

# interpretation:
  #Here’s how to read what you just got:
  
  # Intercept (log-odds) = −3.833
  # Probability = plogis(-3.833) ≈ 0.02118 → about 2.1% of high schools in your sample got a visit from Connecticut College in 2017.
  # Because there are no predictors, this model just encodes that baseline probability in log-odds form.
  # In other words: “If I know nothing about the high school except that it’s in the set Connecticut College could have visited, the model says there’s about a 2.1% chance it got a visit.”

# CREATE MODEL EQUATIONS

  # ALL SCHOOLS

  # 1) Build a reusable RHS (no response) with reformulate() 
    # RHS = right hand side
rhs_terms_all_sch <- c(
  'hs_control*hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces', # remove + hs_pct_white cuz of colinearitiey
  'hs_overall_niche_letter_grade',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian','hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all', # remove hs_zip_pct_nhisp_white +  + hs_zip_pct_nhisp_other cuz of collinearity
  'hs_univ_dist','hs_state_code'
)
# getting rid of the zip code racial percentages cause a modest but signifcant loss of model explanatory power. so consider keeping them.
  rhs_terms_all_sch
  
  # fixed effects models
  form_base_all_sch <- reformulate(termlabels = rhs_terms_all_sch, response = "visit01")
  form_base_all_sch
  
  # random intercept models
  form_eps_all_sch  <- update(form_base_all_sch, . ~ . + (1 | hs_eps_codename))
  form_eps_all_sch


  # PUBLIC SCHOOLS
  # 1) Build a reusable RHS (no response) with reformulate() RHS = right hand side
  rhs_terms_pub_sch <- c(
    'hs_g11',
    'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
    'hs_pct_nativehawaii','hs_pct_tworaces', # remove + hs_pct_white cuz of colinearitiey
    'hs_overall_niche_letter_grade','hs_magnet01','hs_school_type','hs_pct_free_reduced_lunch','hs_pct_prof_math','hs_pct_prof_rla',
    'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
    'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian','hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all', # remove hs_zip_pct_nhisp_white +  + hs_zip_pct_nhisp_other cuz of collinearity
    'hs_univ_dist','hs_state_code'
  )
  rhs_terms_pub_sch
  
  # fixed effects models
  form_base_pub_sch <- reformulate(termlabels = rhs_terms_pub_sch, response = "visit01")
  form_base_pub_sch
  
  # random intercept models
  form_eps_pub_sch  <- update(form_base_pub_sch, . ~ . + (1 | hs_eps_codename))
  form_eps_pub_sch
  
  # PRIVATE SCHOOLS
  pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'private') %>% count(hs_religion_4)
  
  rhs_terms_priv_sch <- c(
    'hs_g11',
    'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
    'hs_pct_nativehawaii','hs_pct_tworaces', # remove + hs_pct_white cuz of colinearitiey
    'hs_school_type','hs_religion_5','hs_overall_niche_letter_grade',
    'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
    'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian','hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all', # remove hs_zip_pct_nhisp_white +  + hs_zip_pct_nhisp_other cuz of collinearity
    'hs_univ_dist','hs_state_code'
  )
  rhs_terms_priv_sch
  
  # fixed effects models
  form_base_priv_sch <- reformulate(termlabels = rhs_terms_priv_sch, response = "visit01")
  form_base_priv_sch
  
  # random intercept models
  form_eps_priv_sch  <- update(form_base_priv_sch, . ~ . + (1 | hs_eps_codename))
  form_eps_priv_sch
  
  # RUN MODELS [ALL SCHOOLS]
  
  mod_base_all_sch <- glm(form_base_all_sch, data = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename)), family = binomial)
  mod_base_all_sch %>% summary()
  pR2(mod_base_all_sch)   # McFadden, Cox & Snell, Nagelkerke
  
  
  mod_eps_all_sch <- glmer(
    form_eps_all_sch,
    data = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename)),
    family = binomial,
    nAGQ = 0,  # 0 = fast approximation;  1= Laplace
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl   = list(maxfun = 2e4) # if you run into convergence problems bump to maxfun = 2e5
    ),
    verbose = 2
  )
  mod_eps_all_sch %>% summary()
  
  # COMPARE MODEL FIT
  AIC(mod_base_all_sch, mod_eps_all_sch)
  BIC(mod_base_all_sch, mod_eps_all_sch)
  
  # EXAMINE PSEUDO R^2 OF FIXED EFFECTS MODEL AND PSEUDO R^2 OF THE FIXED EFFECTS + RANDOM INTERCEPT MODEL
  
  performance::r2(mod_eps_all_sch)
  # performance::r2(mod_base_all_sch)
  # note: when you run for a fixed effects model it calculates Tjur's R^2 which has different interpretation than Psuedo R^2
  
  # R2 for Mixed Models
  # NOTE: results below for a different university.
  # Marginal R2: 0.475: fixed effects alone explain 47.5% of the variance
  # Conditional R2: 0.639: adding EPS random intercept boosts the total explained variance to 63.9% -- a jump of 16 percentage points
  # the gap between 47.5% and 63.9% is the amount that between-geomarket clustering matters for predicting visits beyond the measured covariates.
  # in plain English: EPS grouping captures a substantial share of variation in visit likelihood that the fixed effects can’t explain, raising total variance explained from ~48% to ~64%.
  
  # calculating ICC
  # by hand
  # Extract variance of the geomarket random intercept
  var_eps <- as.data.frame(VarCorr(mod_eps_all_sch))$vcov[1]
  # Logistic residual variance = pi^2 / 3
  icc <- var_eps / (var_eps + (pi^2 / 3))
  icc
  
  # directly from function
  performance::icc(mod_eps_all_sch) # alternative approach to calculating icc
  
  
  pubprivhs_univ_df %>% glimpse()
  
# RUN MODELS [PUBLIC SCHOOLS]  

  # fixed effects
  mod_base_pub_sch <- glm(form_base_pub_sch, data = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'public'), family = binomial)
  mod_base_pub_sch %>% summary()
  pR2(mod_base_pub_sch)   # McFadden, Cox & Snell, Nagelkerke
  
  # random intercept
  mod_eps_pub_sch <- glmer(
    form_eps_pub_sch,
    data = pubprivhs_univ_df %>% filter(univ_id == 160755, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'public'),
    family = binomial,
    nAGQ = 0,  # 0 = fast approximation;  1= Laplace
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl   = list(maxfun = 2e4) # if you run into convergence problems bump to maxfun = 2e5
    ),
    verbose = 2
  )
  #mod_eps_pub_sch %>% summary() 
  
  # post estimation
  # COMPARE MODEL FIT
  #AIC(mod_base_pub_sch, mod_eps_pub_sch)
  #BIC(mod_base_pub_sch, mod_eps_pub_sch)
  
  # EXAMINE PSEUDO R^2 OF FIXED EFFECTS MODEL AND PSEUDO R^2 OF THE FIXED EFFECTS + RANDOM INTERCEPT MODEL
  performance::r2(mod_eps_pub_sch)
  
  # ICC directly from function
  performance::icc(mod_eps_pub_sch) # alternative approach to calculating icc
  
  # RUN MODELS [PRIVATE SCHOOLS]  
  
  # fixed effects
  mod_base_priv_sch <- glm(form_base_priv_sch, data = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'private'), family = binomial)
  mod_base_priv_sch %>% summary()
  pR2(mod_base_priv_sch)   # McFadden, Cox & Snell, Nagelkerke
  
  # random intercept
  mod_eps_priv_sch <- glmer(
    form_eps_priv_sch,
    data = pubprivhs_univ_df %>% filter(univ_id == 160755, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'private'),
    family = binomial,
    nAGQ = 0,  # 0 = fast approximation;  1= Laplace
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl   = list(maxfun = 2e4) # if you run into convergence problems bump to maxfun = 2e5
    ),
    verbose = 2
  )
  mod_eps_priv_sch %>% summary() 
  
  # post estimation
  # COMPARE MODEL FIT
  #AIC(mod_base_priv_sch, mod_eps_priv_sch)
  #BIC(mod_base_priv_sch, mod_eps_priv_sch)
  
  # EXAMINE PSEUDO R^2 OF FIXED EFFECTS MODEL AND PSEUDO R^2 OF THE FIXED EFFECTS + RANDOM INTERCEPT MODEL
  performance::r2(mod_eps_priv_sch)
  
  # ICC directly from function
  performance::icc(mod_eps_priv_sch) # alternative approach to calculating icc

##################################################
################################################## WRITE FUNCTIONS TO RUN ALL MODELS
##################################################  

  

# --- RHS term sets ----------------------------------------------------------
rhs_terms_all_sch <- c(
  'hs_control*hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_overall_niche_letter_grade',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist','hs_state_code'
)
  
library(fixest)
  
  

rhs_terms_pub_sch <- c(
  'hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_overall_niche_letter_grade','hs_magnet01','hs_school_type',
  'hs_pct_free_reduced_lunch','hs_pct_prof_math','hs_pct_prof_rla',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist','hs_state_code'
)

rhs_terms_priv_sch <- c(
  'hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_school_type','hs_religion_5','hs_overall_niche_letter_grade',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist','hs_state_code'
)

rhs_terms_all_sch <- c(
  'hs_state_code'
)

rhs_terms_pub_sch <- c(
  'hs_state_code'
)

rhs_terms_priv_sch <- c(
 'hs_state_code'
)
# --- Helpers ---------------------------------------------------------------
make_forms <- function(rhs_terms, response = "visit01") {
  form_base <- reformulate(termlabels = rhs_terms, response = response)
  form_eps  <- update(form_base, . ~ . + (1 | hs_eps_codename))
  list(base = form_base, eps = form_eps)
}
# this function creates a list of two elements: (1) base fixed-effects model call; (2) random-intercept model call
make_forms(rhs_terms_all_sch)
make_forms(rhs_terms_pub_sch)
make_forms(rhs_terms_priv_sch)

fit_one_spec <- function(df, forms, family = binomial) {
  # Will error out (desired) if something is wrong
  mod_base <- glm(forms$base, data = df, family = family)
  mod_eps  <- glmer(
    forms$eps,
    data = df,
    family = family,
    nAGQ = 0,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e4))
  )
  list(base = mod_base, eps = mod_eps)
}
# this function runs the two regression models: first = fixed effects; second = random intercept
fit_one_spec(
  df = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename)),
  forms = make_forms(rhs_terms_all_sch)
)
fit_one_spec(
  df = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'public'),
  forms = make_forms(rhs_terms_pub_sch)
)
fit_one_spec(
  df = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename), hs_control == 'private'),
  forms = make_forms(rhs_terms_priv_sch)
)

# --- Self-contained pseudo-R² for GLM (no pscl::pR2) -----------------------
pseudo_r2_from_glm <- function(m) {
  # For binomial GLM: use logLik and null deviance (no refit needed)
  ll_full <- as.numeric(logLik(m))
  ll_null <- -m$null.deviance / 2
  n <- stats::nobs(m)

  mcfadden   <- 1 - (ll_full / ll_null)
  coxsnell   <- 1 - exp((2 / n) * (ll_null - ll_full))
  nagelkerke <- coxsnell / (1 - exp((2 / n) * ll_null))

  c(McFadden = mcfadden, CoxSnell = coxsnell, Nagelkerke = nagelkerke)
}
# example (build a quick model then compute pseudo-R²)
mods_demo_all <- fit_one_spec(
  df = pubprivhs_univ_df %>% filter(univ_id == 152080, univ_state_any_visit == 1, !is.na(hs_eps_codename)),
  forms = make_forms(rhs_terms_all_sch)
)
pseudo_r2_from_glm(mods_demo_all$base)

extract_metrics <- function(mod_base, mod_eps) {
  # --- GLM stats via pseudo_r2_from_glm() ---
  pr2 <- pseudo_r2_from_glm(mod_base)
  glm_stats <- data.frame(
    AIC_base = AIC(mod_base),
    BIC_base = BIC(mod_base),
    McFadden = unname(pr2["McFadden"]),
    CoxSnell = unname(pr2["CoxSnell"]),
    Nagelkerke = unname(pr2["Nagelkerke"])
  )

  # --- GLMM stats ---
  r2s <- performance::r2(mod_eps)
  # manual ICC (logistic): var_between / (var_between + pi^2/3)
  var_eps <- as.data.frame(VarCorr(mod_eps))$vcov[1]
  ICC_manual <- var_eps / (var_eps + (pi^2 / 3))
  glmm_stats <- data.frame(
    AIC_eps = AIC(mod_eps),
    BIC_eps = BIC(mod_eps),
    R2_marg = r2s$R2_marginal,
    R2_cond = r2s$R2_conditional,
    ICC = ICC_manual
  )

  cbind(glm_stats, glmm_stats, stringsAsFactors = FALSE)
}
# this function grabs desired statistics after models are run
extract_metrics(mods_demo_all$base, mods_demo_all$eps)

run_for_univ <- function(univ_id, data, forms_all, forms_pub, forms_priv, stop_if_empty = TRUE) {
  base_filter <- data %>%
    filter(univ_id == !!univ_id,
           univ_state_any_visit == 1,
           !is.na(hs_eps_codename))

  # ALL
  df_all <- base_filter
  if (stop_if_empty && nrow(df_all) == 0) stop(sprintf("No rows for univ_id %s (ALL).", univ_id))
  if (length(unique(df_all$visit01)) < 2) stop(sprintf("visit01 has one class for univ_id %s (ALL).", univ_id))
  mods_all <- fit_one_spec(df_all, forms_all)
  met_all  <- extract_metrics(mods_all$base, mods_all$eps)
  met_all$spec <- "all"; met_all$n <- nrow(df_all)

  # PUBLIC
  df_pub <- base_filter %>% filter(hs_control == "public")
  if (stop_if_empty && nrow(df_pub) == 0) stop(sprintf("No PUBLIC rows for univ_id %s.", univ_id))
  if (length(unique(df_pub$visit01)) < 2) stop(sprintf("visit01 has one class for univ_id %s (PUBLIC).", univ_id))
  mods_pub <- fit_one_spec(df_pub, forms_pub)
  met_pub  <- extract_metrics(mods_pub$base, mods_pub$eps)
  met_pub$spec <- "public"; met_pub$n <- nrow(df_pub)

  # PRIVATE
  df_priv <- base_filter %>% filter(hs_control == "private")
  if (stop_if_empty && nrow(df_priv) == 0) stop(sprintf("No PRIVATE rows for univ_id %s.", univ_id))
  if (length(unique(df_priv$visit01)) < 2) stop(sprintf("visit01 has one class for univ_id %s (PRIVATE).", univ_id))
  mods_priv <- fit_one_spec(df_priv, forms_priv)
  met_priv  <- extract_metrics(mods_priv$base, mods_priv$eps)
  met_priv$spec <- "private"; met_priv$n <- nrow(df_priv)

  # combine metrics
  metrics <- rbind(
    transform(met_all,   univ_id = univ_id),
    transform(met_pub,   univ_id = univ_id),
    transform(met_priv,  univ_id = univ_id)
  )
  metrics <- metrics %>% select(univ_id, spec, n, everything())

  models <- list(
    all    = mods_all,
    public = mods_pub,
    private= mods_priv
  )

  list(metrics = metrics, models = models)
}
# this function runs models and grabs statistics for one university
run_for_univ(
  univ_id = '230959',
  data = pubprivhs_univ_df,
  forms_all = make_forms(rhs_terms_all_sch),
  forms_pub = make_forms(rhs_terms_pub_sch),
  forms_priv = make_forms(rhs_terms_priv_sch),
  stop_if_empty = TRUE
)

run_all_universities <- function(univ_ids, data = pubprivhs_univ_df, stop_if_empty = TRUE) {
  forms_all  <- make_forms(rhs_terms_all_sch)
  forms_pub  <- make_forms(rhs_terms_pub_sch)
  forms_priv <- make_forms(rhs_terms_priv_sch)

  metrics_list <- list()
  model_objects <- list()

  for (i in seq_along(univ_ids)) {
    uid <- univ_ids[i]
    message(sprintf("[%d/%d] Fitting models for univ_id = %s", i, length(univ_ids), uid))
    res <- run_for_univ(uid, data, forms_all, forms_pub, forms_priv, stop_if_empty = stop_if_empty)
    metrics_list[[length(metrics_list) + 1]] <- res$metrics
    model_objects[[as.character(uid)]] <- res$models
  }

  #metrics <- do.call(rbind, metrics_list)
  metrics <- dplyr::bind_rows(metrics_list)
  rownames(metrics) <- NULL

  list(
    metrics = metrics,
    model_objects = model_objects
  )
}
# this function loops over many universities and returns a tidy metrics table + model objects (by univ_id key)

# --- Example IDs from your table -------------------------------------------
univ_ids <- c(
  230959, 216287, 123165, 120254, 115409, 126678, 221519, 204501, 173902,
  128902, 167835, 168342, 147767, 152080, 201645, 139658, 223232, 160755,
  228246, 127060, 168148, 239105, 216597, 164924, 228875, 186867, 100751,
  218663, 139959, 181464, 201885, 215293, 186380, 110635, 110653, 126614,
  155317, 106397, 166629, 110671, 110680, 196097
)

# --- Run -------------------------------------------------------------------
# Will stop on the first university/spec that fails, is empty, or has single-class outcome.
res <- run_all_universities(univ_ids, data = pubprivhs_univ_df, stop_if_empty = TRUE)

# Results
metrics_tbl <- res$metrics
tibble::as_tibble(metrics_tbl) |> print(n = Inf, width = Inf)

# Example: inspect a model object (Notre Dame 152080, all-schools GLMM)
# res$model_objects[["152080"]]$all$eps %>% summary()

# --- Make spec-specific tables using univ_abbrev and univ_classification ----
make_spec_tables <- function(metrics_df, univ_df) {
  merged <- metrics_df %>%
    mutate(univ_id = as.character(univ_id)) %>%
    left_join(
      univ_df %>% dplyr::select(univ_id, univ_abbrev, univ_classification),
      by = "univ_id"
    ) %>%
    # add delta_R2 = R2_cond - R2_marg
    mutate(delta_R2 = R2_cond - R2_marg) %>%
    dplyr::select(
      univ_abbrev, univ_classification, spec, n,
      AIC_base, BIC_base, McFadden, CoxSnell, Nagelkerke,
      AIC_eps, BIC_eps, R2_marg, R2_cond, delta_R2, ICC
    )
  
  metrics_all <- merged %>%
    dplyr::filter(spec == "all") %>%
    dplyr::arrange(univ_classification, univ_abbrev)
  
  metrics_public <- merged %>%
    dplyr::filter(spec == "public") %>%
    dplyr::arrange(univ_classification, univ_abbrev)
  
  metrics_private <- merged %>%
    dplyr::filter(spec == "private") %>%
    dplyr::arrange(univ_classification, univ_abbrev)
  
  list(
    all = tibble::as_tibble(metrics_all),
    public = tibble::as_tibble(metrics_public),
    private = tibble::as_tibble(metrics_private)
  )
}
# this function converts your metrics to abbrev-based tables, adds delta_R2, and splits by spec
spec_tables <- make_spec_tables(res$metrics, univ_df)

# Example prints
spec_tables$all     |> print(n = Inf, width = Inf)
spec_tables$public  |> print(n = Inf, width = Inf)
spec_tables$private |> print(n = Inf, width = Inf)

# Optional: save
# write.csv(metrics_tbl, "univ_visit_models_metrics.csv", row.names = FALSE)

univ_df %>% glimpse()
  
##################################################
##################################################
##################################################  
  
# University ID reference table (univ_info)
# -----------------------------------------
# univ_name                                  univ_id
# Middlebury College                         230959
# Swarthmore College                         216287
# Scripps College                            123165
# Occidental College                         120254
# Harvey Mudd College                        115409 geomarkets explain very little
# Colorado College                           126678 geomarkets explain very little
# Sewanee-The University of the South        221519 geomarkets explain modest
# Oberlin College                            204501 geomarkets explain modest
# Macalester College                         173902 geomarkets explain very little
# Connecticut College                        128902 geomarkets explain modest
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


### NEXT THING TO WORK ON, 8/14/2025: WANT TO COMPARE MODEL FIT IN MODEL THAT HAS STATE BUT NOT GEOMARKET TO A MODEL THAT HAS EPS CODE (BUT NOT STATE)
# AFTER THAT: FIGURE OUT WHICH STATISTICS PROVIDE BEST EXPLANATION OF MODEL FIT AND HOW TO COMPARE BETWEEN THE MODELS WITH AND WITHOUT EPS CODE



# why this quicker version is inferior:
#short answer: it isn’t—except you’ve reduced the optimizer’s work budget.
#In glmer(form_re, data=df1, family=binomial) the defaults (in current lme4) use optimizer="bobyqa" with a larger maxfun (typically 100,000).
#In your first call you also use bobyqa but cap maxfun at 20,000. That can make it faster but more likely to stop early (non-convergence, poorer optimum).
#If both converge cleanly, estimates should be (nearly) identical. If the 20k version throws convergence warnings or a worse log-likelihood, bump it up:
  
#Here’s a tight sequence that usually fixes it:
  #1) Try a more generous / robust optimization setup


# (Optional) set reference level if you care about the FE baseline:
# df1$hs_eps_codename <- relevel(df1$hs_eps_codename, ref = "YourRefEPS")



# START ADDING XVARS
mod1 <- glm(
  visit01 ~ hs_control*hs_g11 + hs_pct_asian + hs_pct_black + hs_pct_hispanic + hs_pct_amerindian + hs_pct_nativehawaii + hs_pct_tworaces +  # remove + hs_pct_white cuz of colinearitiey
    hs_overall_niche_letter_grade*hs_control + 
    hs_zip_inc_house_mean + hs_zip_pct_edu_baplus_all + hs_zip_pct_pov_yes + # hs_zip_tot_all +
    hs_zip_pct_nhisp_black + hs_zip_pct_nhisp_native + hs_zip_pct_nhisp_asian + hs_zip_pct_nhisp_nhpi + hs_zip_pct_nhisp_multi + hs_zip_pct_hisp_all + # remove hs_zip_pct_nhisp_white +  + hs_zip_pct_nhisp_other cuz of collinearity
    hs_univ_dist + hs_state_code,
  data = pubprivhs_univ_df %>% filter(univ_id == 147767, univ_state_any_visit ==1),
  family = binomial
)

mod1 %>% summary()
pR2(mod1)   # McFadden, Cox & Snell, Nagelkerke


mod1c <- glmer(
  visit01 ~ hs_control*hs_g11 + hs_pct_asian + hs_pct_black + hs_pct_hispanic + 
    hs_pct_amerindian + hs_pct_nativehawaii + hs_pct_tworaces + 
    hs_overall_niche_letter_grade*hs_control + 
    hs_zip_inc_house_mean + hs_zip_pct_edu_baplus_all + hs_zip_pct_pov_yes + 
    hs_zip_pct_nhisp_black + hs_zip_pct_nhisp_native + hs_zip_pct_nhisp_asian + 
    hs_zip_pct_nhisp_nhpi + hs_zip_pct_nhisp_multi + hs_zip_pct_hisp_all + 
    hs_univ_dist + (1 | hs_eps_codename),
  data = pubprivhs_univ_df %>% filter(univ_id == 147767, univ_state_any_visit == 1),
  family = binomial
)


mod1c <- glmer(
  visit01 ~ hs_control*hs_g11 + hs_pct_asian + hs_pct_black + hs_pct_hispanic +
    hs_pct_amerindian + hs_pct_nativehawaii + hs_pct_tworaces +
    hs_overall_niche_letter_grade*hs_control +
    hs_zip_inc_house_mean + hs_zip_pct_edu_baplus_all + hs_zip_pct_pov_yes +
    hs_zip_pct_nhisp_black + hs_zip_pct_nhisp_native + hs_zip_pct_nhisp_asian +
    hs_zip_pct_nhisp_nhpi + hs_zip_pct_nhisp_multi + hs_zip_pct_hisp_all +
    hs_univ_dist + (1 | hs_eps_codename),
  data = pubprivhs_univ_df %>% filter(univ_id == 147767, univ_state_any_visit == 1),
  family = binomial,
  nAGQ = 0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

mod1c %>% summary()
# --- What the EPS random intercept is doing (ready-to-paste sentences) ---
# ICC interpretation for your model:
# "Roughly one-third of the residual variation in baseline visit propensity occurs at the EPS level.
# In other words, schools within the same EPS have more similar visit probabilities than schools in
# different EPSs. Omitting the EPS random intercept leaves that clustering unexplained."

# Paper-ready summary line:
# "Adding a random intercept for hs_eps_codename captures substantial between-EPS heterogeneity
# (random-intercept variance = 1.54, SD = 1.24; ICC ≈ 0.32), indicating that ~32% of the latent
# variance in visit propensity lies at the EPS level."

# --- Compute ICC from your fitted glmer model (mod1c) ---
var_eps <- as.data.frame(VarCorr(mod1c))$vcov[1]
icc_eps <- var_eps / (var_eps + (pi^2/3))
icc_eps

# --- Marginal vs Conditional R^2 (how much the random effect adds) ---
# Marginal R^2 = fixed effects only; Conditional R^2 = fixed + random effects.
# The gap between them is the extra explanatory power from hs_eps_codename.
library(performance)
r2_mod1c <- performance::r2(mod1c)
r2_mod1c

# --- EPS random effect: what it adds (paste in paper) ---
# "Adding a random intercept for hs_eps_codename captures substantial between-EPS heterogeneity
# (variance = 1.54, SD = 1.24; ICC ≈ 0.32). Roughly one-third of the residual variation in baseline
# visit propensity lies at the EPS level—schools within the same EPS have more similar visit probabilities."

# "Explained variance rose from Marginal R^2 = 0.763 (fixed effects only) to Conditional R^2 = 0.838
# (fixed + random effects), so the EPS random intercept contributes ~0.075 additional R^2."

# --- Compute and print the key stats from your fitted model mod1c ---
var_eps <- as.data.frame(VarCorr(mod1c))$vcov[1]
icc_eps <- var_eps / (var_eps + (pi^2/3))
r2_mod1c <- performance::r2(mod1c)

delta_r2 <- as.numeric(r2_mod1c$R2_conditional - r2_mod1c$R2_marginal)

cat(sprintf(
  "EPS random intercept: variance = %.3f, SD = %.3f, ICC = %.3f\nMarginal R^2 = %.3f, Conditional R^2 = %.3f, ΔR^2 = %.3f\n",
  var_eps, sqrt(var_eps), icc_eps, r2_mod1c$R2_marginal, r2_mod1c$R2_conditional, delta_r2
))

#Here’s a clean sentence you can paste directly:
  
  #Adding a random intercept for hs_eps_codename captured substantial between-EPS heterogeneity
    # (variance = 1.54, SD = 1.24; ICC ≈ 0.32). This increased explained variance from marginal R² = 0.763
    # (fixed effects only) to conditional R² = 0.838, a gain of 0.076 R², indicating that about one-third 
    # of the residual variation in baseline visit propensity occurs at the EPS level.

# In your context, a ΔR² of 0.076 for a model that already had a marginal R² of 0.763 is substantively big.
#
# Here’s why:
#
# In most social science applications, even a 0.02–0.03 jump in R² from adding one grouping variable
# is often considered meaningful — especially when the base model is already explaining a lot of variance.
#
# Your model was already explaining ~76% of the variance from fixed effects, and adding hs_eps_codename
# pushed it up to ~84%. That means the random intercept is accounting for nearly 10% of the remaining
# unexplained variance.
#
# Combined with your ICC of ~0.32, it says EPS-level clustering is not just statistically significant,
# it’s practically relevant — the grouping explains a large share of the differences that the fixed effects can’t.

# --- Fit stats you can cite directly from mod1c (already in summary, but programmatic) ---
aic_mod1c <- AIC(mod1c)
ll_mod1c  <- as.numeric(logLik(mod1c))
dev_mod1c <- -2 * ll_mod1c
c(AIC = aic_mod1c, logLik = ll_mod1c, minus2LL = dev_mod1c)

# If later you fit a fixed-only comparator, you can report ΔAIC and Δ(−2LL) like so:
# delta_AIC    <- AIC(mod_fixed) - AIC(mod1c)
# delta_minus2 <- (-2 * as.numeric(logLik(mod_fixed))) - (-2 * as.numeric(logLik(mod1c)))
# paste0("ΔAIC = ", round(delta_AIC,1), "; Δ−2LL = ", round(delta_minus2,1))



# Fixed-effects-only model (no random intercept)
mod1c_fixed <- glmer(
  visit01 ~ hs_control*hs_g11 + hs_pct_asian + hs_pct_black + hs_pct_hispanic +
    hs_pct_amerindian + hs_pct_nativehawaii + hs_pct_tworaces +
    hs_overall_niche_letter_grade*hs_control +
    hs_zip_inc_house_mean + hs_zip_pct_edu_baplus_all + hs_zip_pct_pov_yes +
    hs_zip_pct_nhisp_black + hs_zip_pct_nhisp_native + hs_zip_pct_nhisp_asian +
    hs_zip_pct_nhisp_nhpi + hs_zip_pct_nhisp_multi + hs_zip_pct_hisp_all +
    hs_univ_dist,
  data = pubprivhs_univ_df %>% filter(univ_id == 147767, univ_state_any_visit == 1),
  family = binomial,
  nAGQ = 0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Compare with the random-intercept model
anova(mod1c_fixed, mod1c, test = "Chisq")


df %>% filter( is.na(hs_zip_tot_all)) %>% count()
df %>% filter( !is.na(hs_zip_tot_all)) %>% count()

df %>% select(hs_eps_codename,hs_eps, hs_eps_name) %>% glimpse()

df %>% count(hs_eps_codename) %>% print(n=400)


# McFadden tests of Pseudo R^2 and related
pR2(mod1)   # McFadden, Cox & Snell, Nagelkerke
  # Interpretation of pR2(mod1) output for the simple logistic regression:
  #
  # 1. llh = −1080.85
  #    The log-likelihood of your fitted model.
  #
  # 2. llhNull = −2482.93
  #    The log-likelihood of the null model (only an intercept, no predictors).
  #    Less negative = better fit.
  #
  # 3. G2 = 2804.17
  #    The likelihood ratio chi-square statistic:
  #    G^2 = −2 * (llhNull − llh)
  #    Very large here → huge improvement over the null.
  #
  # 4. McFadden = 0.565
  #    Formula: 1 − (llh / llhNull)
  #    Rough logistic analog of R², based on log-likelihoods.
  #    Interpretation: Your predictors reduce the unexplained “error” by ~56%
  #    compared to the null model.
  #    Rule of thumb: 0.2–0.4 is already “excellent” in logistic regression —
  #    0.56 is huge. It means the model is extremely good at separating visited vs.
  #    non-visited.
  #
  # 5. r2ML = 0.110 (Cox & Snell)
  #    Likelihood-based, scaled for sample size.
  #    Doesn’t reach 1.0 for binary outcomes, so often feels “small.”
  #
  # 6. r2CU = 0.590 (Nagelkerke)
  #    Adjusts Cox & Snell so it can reach 1.0.
  #    Interpretable more like OLS R² → about 59% of the variation in the
  #    log-likelihood is “explained” by your predictors.


pubprivhs_univ_df %>% group_by(univ_id,hs_state_code) %>% 
  summarize(
    n_schools = n(),
    n_visit = sum(visit01 == 1),
    n_not_visit = sum(visit01 ==0)
  ) %>% print(n=400)

# same cleaned RHS, no state FE, add EPS random intercept


# NICHE GRADE
df %>% filter( is.na(hs_overall_niche_letter_grade)) %>% count()
df %>% filter( !is.na(hs_overall_niche_letter_grade)) %>% count()
df %>% count(hs_overall_niche_letter_grade)
df %>% count(hs_control)
df %>% count(hs_control,hs_overall_niche_letter_grade) %>% print(n=30)


# checking zip-code level educational attainment variables
df %>% filter( is.na(hs_zip_pct_edu_baplus_all)) %>% count()
df %>% filter( !is.na(hs_zip_pct_edu_baplus_all)) %>% count()



# checking zip-code level income variables
df %>% filter( is.na(hs_zip_inc_house_med)) %>% count()
df %>% filter( !is.na(hs_zip_inc_house_med)) %>% count()

df %>% filter( is.na(hs_zip_inc_house_mean)) %>% count()
df %>% filter( !is.na(hs_zip_inc_house_mean)) %>% count()

df %>% filter( is.na(hs_zip_inc_house_mean_calc)) %>% count()
df %>% filter( !is.na(hs_zip_inc_house_mean_calc)) %>% count()







# Interpretation:

  # 1. Interpret the intercept:
  # The intercept is the log-odds of visit01 = 1 when all predictors are at their reference categories, that is when state = CT and school control = public.
    coef(mod1)["(Intercept)"] # this is what we get from the summary() table
    # Convert log-odds to odds:
    exp(coef(mod1)["(Intercept)"])
    #(Intercept) 
    #0.1395702     
    # This means: when hs_control = "public" (reference category) AND hs_state_code = "CT" (reference category),
    # the odds of visit01 = 1 (i.e., a visit) are about 0.14 to 1.
    # In other words, for every 1 high school that *is* visited, there are about 7.17 (1 / 0.1396) high schools that are *not* visited.    
  
  # Convert log-odds to probability:
    plogis(coef(mod1)["(Intercept)"])
    # plogis(coef(mod1)["(Intercept)"]) = 0.1224762
    # This means: when hs_control = "public" and hs_state_code = "CT", the predicted probability of a visit is about 12.25%.
    
  
  # 2. Interpret the coefficient on hs_controlprivate:
  # The coefficient is the difference in log-odds between private and public schools, holding state constant.
    coef(mod1)["hs_controlprivate"]    
    # coef(mod1)["hs_controlprivate"] = 1.898513
    # This means: compared to public high schools (reference group), private high schools in the same state have an increase of about 1.90 in the log-odds of receiving a visit.
    
  # Convert the log-odds difference to an odds ratio (private vs public):
    exp(coef(mod1)["hs_controlprivate"])
    # exp(coef(mod1)["hs_controlprivate"]) = 6.675959
    # This means: compared to public high schools (reference group), private high schools have odds of receiving a visit that are about 6.68 times higher, holding state constant.
    
  # seems like calculating marginal effect for coef(mod1)["hs_controlprivate"] is more complicated
    # why this is not correct: 
      # plogis(coef(mod1)["hs_controlprivate"])
      # plogis(coef(mod1)["hs_controlprivate"]) just takes the log-odds difference for being private vs. public, pretends that’s the whole linear predictor, and converts it to a probability.
      # It ignores the intercept and other terms in your model, so it’s not the marginal effect — it’s the probability that would result if the only thing in the model was that coefficient, starting from zero log-odds.
    # correct approach conceptually:
    # Marginal effect (in a logit model) = change in predicted probability when a predictor changes, holding other variables constant.
      # That requires two probability predictions:
      # One with the predictor at the baseline (e.g., public)
      # One with the predictor switched (e.g., private)
      # Then you subtract: P(private) – P(public)    
    
    # If you want the true marginal effect for hs_controlprivate in your model, the correct approach is:
    
    new_data <- data.frame(
      hs_control = c("public", "private"),
      hs_state_code = "CT" # or any fixed reference state
    )
    new_data
    
    pred_probs <- predict(mod1, new_data, type = "response")
    pred_probs
    marginal_effect <- pred_probs[2] - pred_probs[1]
    
    pred_probs
    marginal_effect
    
    #That marginal_effect is the actual change in probability of a visit when moving from public → private, holding state constant.
    # pred_probs = c(0.1224762, 0.4823387)
    # Interpretation: Holding state constant (here fixed at CT), the model predicts that public high schools
    # have a 12.25% probability of receiving a visit, while private high schools have a 48.23% probability.
    
    # marginal_effect = 0.3598625
    # Interpretation: Moving from a public to a private high school increases the predicted probability
    # of receiving a visit by about 35.99 percentage points, holding state constant.
    
    
    # 3. Get predicted probabilities for each category:
    # Sometimes easier to explain than log-odds or odds ratios.
    new_data <- data.frame(
      hs_control = c("public", "private"),
      hs_state_code = "CT" # or whichever reference state
    )
    predict(mod1, new_data, type = "response") # returns predicted visit probability for each group
    
    
#NEXT STEPS, MONDAY 8/11. KEEP ON ADDING X VARIABLES TO YOUR LOGIT MODEL;
    # START ASKING GPT5 HOW TO FIND AND INTERPRET THE PERCENT OF VARIATION EXPLAINED BY THE MODEL [AND BY SPECIFIC VARIABLES]