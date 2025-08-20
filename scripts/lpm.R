################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < lpm.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/20/2025
## [ DESC ] < linear probability model that school i gets visit, fixed effects for geomarket>
################################################################################

### SETTINGS
rm(list = ls())
options(max.print=1000)
#options(width = 160)

#library(lme4)
#library(pscl)
#library(performance)
library(tidyverse)
library(fixest)


####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES


getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()

# remove objects from cb_geo (ajs manuscript) mapping
rm(create_rq1_map,format_vars,get_palette)

############

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

pubprivhs_univ_df %>% glimpse()

##################################################
################################################## MODELING VISITS TO SCHOOL I BY COLLEGE J
################################################## 

# NEXT UP, CONSIDER ADDING THESE VARIABLES

$ hs_univ_ineps                     <dbl> 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0…
$ hs_univ_instate                   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
$ hs_univ_inregion                  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
$ hs_univ_market                    <ord> local, local, local, local, in_state, in_state, in_state, local, in_state, local, in_state, in_state, in_state, in_state, in_state, local, in_sta…

df_all <- pubprivhs_univ_df %>%
  dplyr::filter(!is.na(hs_eps_codename)) %>%
  dplyr::mutate(one = rnorm(dplyr::n()))   # only to give (1) & (2) a slope


# ---------- TERM SETS ----------
# Common covariates (NO state here). Note: hs_g11 enters via hs_control*hs_g11.
rhs_common_ij <- c(
  "hs_control*hs_g11",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean","hs_zip_pct_edu_baplus_all","hs_zip_pct_pov_yes",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)
rhs_common_ij

# Covariates + state (baseline)
rhs_with_state_ij <- c(rhs_common_ij, "hs_state_code")
rhs_with_state_ij

# ---------- FORMULAS ----------

# (1) State dummies only + random 'one' slope (so there's a coeff to show)
form_state_only_ij <- visit01 ~ one + hs_state_code | univ_id
form_state_only_ij

# (2) EPS fixed effects only + random 'one' slope
form_eps_only_ij   <- as.formula("visit01 ~ one | univ_id + hs_eps_codename")
form_eps_only_ij

# (3) Covariates only (no state, no EPS FE)
form_cov_only_ij <- as.formula(
  sprintf("visit01 ~ %s | univ_id", paste(rhs_common_ij, collapse = " + "))
)
form_cov_only_ij

# (4) Covariates + state dummies
form_base_all_sch_ij <- as.formula(
  paste0("visit01 ~ ", paste(rhs_with_state_ij, collapse = " + "), " | univ_id")
)
form_base_all_sch_ij


# (5) Covariates + EPS fixed effects
form_fe_all_sch_ij <- as.formula(
  paste0("visit01 ~ ", paste(rhs_common_ij, collapse = " + "), " | univ_id + hs_eps_codename")
)
form_fe_all_sch_ij

# ---------- FIT MODELS (clustered by state) ----------

# (1) State dummies only + random 'one' slope (so there's a coeff to show)

m_state_only_ij <- feols(
  form_state_only_ij,
  data    = df_all,
  cluster = ~ hs_state_code + univ_id
)
m_state_only_ij %>% summary()

m_eps_only_ij <- feols(
  form_eps_only_ij,
  data    = df_all,
  cluster = ~ hs_state_code + univ_id
)
m_eps_only_ij %>% summary()

m_cov_only_ij <- feols(
  form_cov_only_ij,
  data    = df_all,                       # pooled school × university data
  cluster = ~ hs_state_code + univ_id     # two-way clustered SEs (state & university)
)
m_cov_only_ij %>% summary()

m_base_all_sch_ij <- feols(
  form_base_all_sch_ij,
  data    = df_all,
  cluster = ~ hs_state_code + univ_id
)
m_base_all_sch_ij %>% summary()

m_fe_all_sch_ij <- feols(
  form_fe_all_sch_ij,
  data    = df_all,
  cluster = ~ hs_state_code + univ_id
)

m_fe_all_sch_ij %>% summary()

# ---------- SIDE-BY-SIDE TABLE (i × j pooled) ----------
mods_ij <- list(
  "(1) State only + Univ FE"          = m_state_only_ij,
  "(2) EPS FE + Univ FE"              = m_eps_only_ij,
  "(3) Covariates only + Univ FE"     = m_cov_only_ij,
  "(4) Covariates + State + Univ FE"  = m_base_all_sch_ij,
  "(5) Covariates + EPS FE + Univ FE" = m_fe_all_sch_ij
)

# Good-of-fit rows to keep (Adjusted R² & Within R² work with fixest)
gof_map_ij <- data.frame(
  raw   = c("nobs","adj.r.squared","r2.within","rmse"),
  clean = c("N","Adj. R²","Within R²","RMSE"),
  fmt   = c(0,3,3,3),
  stringsAsFactors = FALSE
)

tab_df_ij <- modelsummary(
  mods_ij,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*"=.05, "**"=.01, "***"=.001),
  gof_map   = gof_map_ij,
  output    = "data.frame"
)

# Identify the model columns (all except metadata)
model_cols_ij <- setdiff(names(tab_df_ij), c("term","part","statistic"))

# Omit state rows and the helper 'one' row
tab_df_ij <- tab_df_ij |>
  dplyr::filter(!grepl("^hs_state_code", term)) |>
  dplyr::filter(term != "one")

# SE rows: any model column begins with "("
se_rows_ij <- apply(tab_df_ij[model_cols_ij], 1, function(x) {
  any(grepl("^\\(", ifelse(is.na(x), "", x)))
})

# Blank the variable name on SE rows
tab_df_ij$term[se_rows_ij] <- ""

# Print cleanly (no gridlines, no extra cols)
tab_df_ij |>
  dplyr::select(-c(part, statistic)) |>
  print(row.names = FALSE)


##################################################
################################################## MODELING ONE UNIVERSITY AT A TIME
################################################## 

# ---------- PACKAGES ----------
library(dplyr)
library(fixest)
library(broom)        # registers tidy.fixest
library(parameters)   # optional
library(modelsummary)

# ---------- DATA FILTER ----------
df_sub <- pubprivhs_univ_df %>%
  dplyr::filter(univ_id == 228246, !is.na(hs_eps_codename)) %>%
  dplyr::mutate(one = rnorm(dplyr::n()))   # only to give (1) & (2) a slope

# ---------- TERM SETS ----------
# Common covariates (NO state here). Note: hs_g11 enters via hs_control*hs_g11.
rhs_common <- c(
  "hs_control*hs_g11",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean","hs_zip_pct_edu_baplus_all","hs_zip_pct_pov_yes",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# Covariates + state (baseline)
rhs_with_state <- c(rhs_common, "hs_state_code")

# ---------- FORMULAS ----------
# (1) State dummies only + random 'one' slope (so there's a coeff to show)
form_state_only <- visit01 ~ one + hs_state_code

# (2) EPS fixed effects only + random 'one' slope
form_eps_only   <- as.formula("visit01 ~ one | hs_eps_codename")

# (3) Covariates only (no state, no EPS FE)
form_cov_only   <- as.formula(paste0("visit01 ~ ", paste(rhs_common, collapse = " + ")))

# (4) Covariates + state dummies
form_base_all_sch <- reformulate(rhs_with_state, response = "visit01")

# (5) Covariates + EPS fixed effects
form_fe_all_sch <- as.formula(
  paste0("visit01 ~ ", paste(rhs_common, collapse = " + "), " | hs_eps_codename")
)

# ---------- FIT MODELS (clustered by state) ----------
m_state_only <- feols(form_state_only,   data = df_sub, cluster = ~ hs_state_code) # (1)
m_eps_only   <- feols(form_eps_only,     data = df_sub, cluster = ~ hs_state_code) # (2)
m_cov        <- feols(form_cov_only,     data = df_sub, cluster = ~ hs_state_code) # (3)
m_base       <- feols(form_base_all_sch, data = df_sub, cluster = ~ hs_state_code) # (4)
m_fe         <- feols(form_fe_all_sch,   data = df_sub, cluster = ~ hs_state_code) # (5)

# ---------- SIDE-BY-SIDE TABLE ----------
mods <- list(
  "(1) State only"              = m_state_only,
  "(2) EPS FE only"             = m_eps_only,
  "(3) Covariates only"         = m_cov,
  "(4) Covariates + State"      = m_base,
  "(5) Covariates + EPS FE"     = m_fe
)

# Good-of-fit rows to keep
gof_map <- data.frame(
  raw   = c("nobs","adj.r.squared","r2.within","rmse"),
  clean = c("N","Adj. R²","Within R²","RMSE"),
  fmt   = c(0,3,3,3),
  stringsAsFactors = FALSE
)

tab_df <- modelsummary(
  mods,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*"=.05, "**"=.01, "***"=.001),
  gof_map   = gof_map,
  output    = "data.frame"
)

# Identify the model columns (all except metadata)
model_cols <- setdiff(names(tab_df), c("term","part","statistic"))

# Omit state rows and the helper 'one' row
tab_df <- tab_df %>%
  dplyr::filter(!grepl("^hs_state_code", term)) %>%
  dplyr::filter(term != "one")

# SE rows: any model column begins with "("
se_rows <- apply(tab_df[model_cols], 1, function(x) any(grepl("^\\(", x %||% "")))

# Blank the variable name on SE rows
tab_df$term[se_rows] <- ""

# Print cleanly (no gridlines, no extra cols)
tab_df %>%
  dplyr::select(-c(part, statistic)) %>%
  print(row.names = FALSE)


##################################################
################################################## WRITE FUNCTIONS TO RUN ALL MODELS, SEPARATELY FOR VITIS TO: ALL SCHOOLS; PUBLIC SCHOOLS; PRIVATE SCHOOLS
##################################################  

# ============================
# Packages
# ============================
library(dplyr)
library(fixest)
library(purrr)
library(tibble)

# ============================
# Inputs
# ============================
univ_ids <- c(
  230959, 216287, 123165, 120254, 115409, 126678, 221519, 204501, 173902,
  128902, 167835, 168342, 147767, 152080, 201645, 139658, 223232, 160755,
  228246, 127060, 168148, 239105, 216597, 164924, 228875, 186867, 100751,
  218663, 139959, 181464, 201885, 215293, 186380, 110635, 110653, 126614,
  155317, 106397, 166629, 110671, 110680, 196097
)

# ============================
# Data prep
# ============================
set.seed(42)  # only used for ALL-schools "one" regressor
df_work <- pubprivhs_univ_df %>%
  mutate(one = rnorm(n()))   # used to guarantee a slope in ALL-schools Models (1)-(2)

# ============================
# Covariate blocks
# ============================
# ALL schools (your original block)
rhs_common <- c(
  "hs_control*hs_g11",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean","hs_zip_pct_edu_baplus_all","hs_zip_pct_pov_yes",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)
rhs_with_state <- c(rhs_common, "hs_state_code")

# PUBLIC HS models (your requested block)
rhs_terms_pub_sch <- c(
  'hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_overall_niche_letter_grade','hs_magnet01','hs_school_type',
  'hs_pct_free_reduced_lunch','hs_pct_prof_math','hs_pct_prof_rla',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist'
)

# PRIVATE HS models (your requested block)
rhs_terms_priv_sch <- c(
  'hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_school_type','hs_religion_5','hs_overall_niche_letter_grade',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist'
)

# ============================
# Formulas
# ============================
# --- ALL schools (5 specs) ---
form_state_only   <- visit01 ~ one + hs_state_code
form_eps_only     <- as.formula("visit01 ~ one | hs_eps_codename")
form_cov_only     <- reformulate(rhs_common, response = "visit01")
form_cov_state    <- reformulate(rhs_with_state, response = "visit01")
form_cov_epsfe    <- as.formula(
  paste0("visit01 ~ ", paste(rhs_common, collapse = " + "), " | hs_eps_codename")
)

# --- PUBLIC HS (5 specs) ---
form_pub_state_only <- visit01 ~ hs_g11 + hs_state_code
form_pub_eps_only   <- as.formula("visit01 ~ hs_g11 | hs_eps_codename")
form_pub_cov_only   <- reformulate(rhs_terms_pub_sch, response = "visit01")
form_pub_cov_state  <- reformulate(c(rhs_terms_pub_sch, "hs_state_code"), response = "visit01")
form_pub_cov_epsfe  <- as.formula(
  paste0("visit01 ~ ", paste(rhs_terms_pub_sch, collapse = " + "), " | hs_eps_codename")
)

# --- PRIVATE HS (5 specs) ---
form_priv_state_only <- visit01 ~ hs_g11 + hs_state_code
form_priv_eps_only   <- as.formula("visit01 ~ hs_g11 | hs_eps_codename")
form_priv_cov_only   <- reformulate(rhs_terms_priv_sch, response = "visit01")
form_priv_cov_state  <- reformulate(c(rhs_terms_priv_sch, "hs_state_code"), response = "visit01")
form_priv_cov_epsfe  <- as.formula(
  paste0("visit01 ~ ", paste(rhs_terms_priv_sch, collapse = " + "), " | hs_eps_codename")
)

# ============================
# Metrics helpers
# ============================
get_adj_r2 <- function(m) {
  out <- tryCatch(fixest::r2(m, type = "ar2"), error = function(e) NA_real_)
  if (!is.na(out)) return(out)
  out <- tryCatch(fixest::fitstat(m, ~ r2.a)[[1]], error = function(e) NA_real_)
  if (!is.na(out)) return(out)
  R2 <- tryCatch(fixest::fitstat(m, ~ r2)[[1]], error = function(e) NA_real_)
  n  <- tryCatch(nobs(m), error = function(e) NA_real_)
  k  <- tryCatch(length(coef(m)), error = function(e) NA_real_)
  if (is.na(R2) || is.na(n) || is.na(k) || (n - k - 1) <= 0) return(NA_real_)
  1 - (1 - R2) * (n - 1) / (n - k - 1)
}

get_within_r2 <- function(m) {
  out <- tryCatch(fixest::r2(m, type = "within"), error = function(e) NA_real_)
  if (!is.na(out)) return(out)
  for (k in list(~ r2_within, ~ wr2, ~ r2.within)) {
    out <- tryCatch(fixest::fitstat(m, k)[[1]], error = function(e) NA_real_)
    if (!is.na(out)) return(out)
  }
  NA_real_
}

# Safe wrappers (some univs may have 0 rows in a sector)
safe_feols <- function(form, data) {
  tryCatch(feols(form, data = data, cluster = ~ hs_state_code), error = function(e) NULL)
}
adj2_safe    <- function(m) if (is.null(m)) NA_real_ else get_adj_r2(m)
within2_safe <- function(m) if (is.null(m)) NA_real_ else get_within_r2(m)

# ============================
# Runners
# ============================
# --- ALL schools for one university ---
run_all_models_for_univ <- function(uid) {
  d <- df_work %>% filter(univ_id == uid, !is.na(hs_eps_codename))
  m1 <- safe_feols(form_state_only,  d)
  m2 <- safe_feols(form_eps_only,    d)
  m3 <- safe_feols(form_cov_only,    d)
  m4 <- safe_feols(form_cov_state,   d)
  m5 <- safe_feols(form_cov_epsfe,   d)
  tibble(
    univ_id               = uid,
    adjR2_state_only      = adj2_safe(m1),
    adjR2_eps_fe_only     = adj2_safe(m2),
    adjR2_cov_only        = adj2_safe(m3),
    adjR2_cov_state       = adj2_safe(m4),
    adjR2_cov_eps_fe      = adj2_safe(m5),
    withinR2_cov_eps_fe   = within2_safe(m5)
  )
}

# --- PUBLIC or PRIVATE for one university ---
run_all_models_for_univ_sector <- function(uid, sector = c("public","private")) {
  sector <- match.arg(sector)
  d <- df_work %>%
    filter(univ_id == uid, !is.na(hs_eps_codename), hs_control == sector)
  
  if (sector == "public") {
    m1 <- safe_feols(form_pub_state_only, d)
    m2 <- safe_feols(form_pub_eps_only,   d)
    m3 <- safe_feols(form_pub_cov_only,   d)
    m4 <- safe_feols(form_pub_cov_state,  d)
    m5 <- safe_feols(form_pub_cov_epsfe,  d)
    tibble(
      univ_id                   = uid,
      adjR2_pub_state_only      = adj2_safe(m1),
      adjR2_pub_eps_fe_only     = adj2_safe(m2),
      adjR2_pub_cov_only        = adj2_safe(m3),
      adjR2_pub_cov_state       = adj2_safe(m4),
      adjR2_pub_cov_eps_fe      = adj2_safe(m5),
      withinR2_pub_cov_eps_fe   = within2_safe(m5)
    )
  } else {
    m1 <- safe_feols(form_priv_state_only, d)
    m2 <- safe_feols(form_priv_eps_only,   d)
    m3 <- safe_feols(form_priv_cov_only,   d)
    m4 <- safe_feols(form_priv_cov_state,  d)
    m5 <- safe_feols(form_priv_cov_epsfe,  d)
    tibble(
      univ_id                   = uid,
      adjR2_priv_state_only     = adj2_safe(m1),
      adjR2_priv_eps_fe_only    = adj2_safe(m2),
      adjR2_priv_cov_only       = adj2_safe(m3),
      adjR2_priv_cov_state      = adj2_safe(m4),
      adjR2_priv_cov_eps_fe     = adj2_safe(m5),
      withinR2_priv_cov_eps_fe  = within2_safe(m5)
    )
  }
}

# ============================
# Execute (ALL / PUBLIC / PRIVATE)
# ============================
adjr2_all_by_univ  <- map_dfr(univ_ids, run_all_models_for_univ)
adjr2_pub_by_univ  <- map_dfr(univ_ids, run_all_models_for_univ_sector, sector = "public")
adjr2_priv_by_univ <- map_dfr(univ_ids, run_all_models_for_univ_sector, sector = "private")

# ============================
# Visit counts
# ============================
visit_counts_total <- events_df %>%
  group_by(univ_id) %>%
  summarize(
    n_vis        = n(),
    n_pubhs_vis  = sum(event_type == "pub_hs",  na.rm = TRUE),
    n_privhs_vis = sum(event_type == "priv_hs", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(univ_id = as.character(univ_id))

visit_counts_pub <- events_df %>%
  filter(event_type == "pub_hs") %>%
  count(univ_id, name = "n_pubhs_vis") %>%
  mutate(univ_id = as.character(univ_id))

visit_counts_priv <- events_df %>%
  filter(event_type == "priv_hs") %>%
  count(univ_id, name = "n_privhs_vis") %>%
  mutate(univ_id = as.character(univ_id))

# ============================
# Merge with metadata
# ============================
meta_small <- univ_df %>%
  select(univ_id, univ_abbrev, univ_classification, univ_usnwr_rank) %>%
  mutate(univ_id = as.character(univ_id))


# --- ALL schools table ---
adjr2_all_with_meta <- adjr2_all_by_univ %>%
  mutate(univ_id = as.character(univ_id)) %>%
  left_join(meta_small, by = "univ_id") %>%
  left_join(visit_counts_total, by = "univ_id") %>%
  select(
    univ_abbrev, univ_classification, univ_usnwr_rank,
    n_vis, n_pubhs_vis, n_privhs_vis,
    adjR2_state_only, adjR2_eps_fe_only, adjR2_cov_only,
    adjR2_cov_state,  adjR2_cov_eps_fe,  withinR2_cov_eps_fe
  ) %>%
  arrange(univ_classification, univ_usnwr_rank)

# --- PUBLIC HS table ---
adjr2_pub_with_meta <- adjr2_pub_by_univ %>%
  mutate(univ_id = as.character(univ_id)) %>%
  left_join(meta_small, by = "univ_id") %>%
  left_join(visit_counts_pub, by = "univ_id") %>%
  select(
    univ_abbrev, univ_classification, univ_usnwr_rank, n_pubhs_vis,
    adjR2_pub_state_only, adjR2_pub_eps_fe_only, adjR2_pub_cov_only,
    adjR2_pub_cov_state,  adjR2_pub_cov_eps_fe,  withinR2_pub_cov_eps_fe
  ) %>%
  arrange(univ_classification, univ_usnwr_rank)

# --- PRIVATE HS table ---
adjr2_priv_with_meta <- adjr2_priv_by_univ %>%
  mutate(univ_id = as.character(univ_id)) %>%
  left_join(meta_small, by = "univ_id") %>%
  left_join(visit_counts_priv, by = "univ_id") %>%
  select(
    univ_abbrev, univ_classification, univ_usnwr_rank, n_privhs_vis,
    adjR2_priv_state_only, adjR2_priv_eps_fe_only, adjR2_priv_cov_only,
    adjR2_priv_cov_state,  adjR2_priv_cov_eps_fe,  withinR2_priv_cov_eps_fe
  ) %>%
  arrange(univ_classification, univ_usnwr_rank)

# ============================
# Print (round only the R² columns)
# ============================
round_r2_cols <- function(df) {
  df %>%
    mutate(
      across(
        where(is.numeric) & (starts_with("adjR2") | starts_with("withinR2")),
        ~ ifelse(is.na(.x), NA_real_, round(.x, 3))
      )
    )
}

cat("\n=== ALL HS (combined public+private) ===\n")
adjr2_all_with_meta %>% round_r2_cols() %>% print(row.names = FALSE, n = Inf)

cat("\n=== PUBLIC HS only ===\n")
adjr2_pub_with_meta %>% round_r2_cols() %>% print(row.names = FALSE, n = Inf)

cat("\n=== PRIVATE HS only ===\n")
adjr2_priv_with_meta %>% round_r2_cols() %>% print(row.names = FALSE, n = Inf)


##################################################
################################################## 
##################################################  

##################################################
################################################## WRITE FUNCTIONS TO RUN ALL MODELS FOR VISITS TO: ALL SCHOOLS
##################################################  

rhs_terms_pub_sch <- c(
  'hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_overall_niche_letter_grade','hs_magnet01','hs_school_type',
  'hs_pct_free_reduced_lunch','hs_pct_prof_math','hs_pct_prof_rla',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist'
)

rhs_terms_priv_sch <- c(
  'hs_g11',
  'hs_pct_asian','hs_pct_black','hs_pct_hispanic','hs_pct_amerindian',
  'hs_pct_nativehawaii','hs_pct_tworaces',
  'hs_school_type','hs_religion_5','hs_overall_niche_letter_grade',
  'hs_zip_inc_house_mean','hs_zip_pct_edu_baplus_all','hs_zip_pct_pov_yes',
  'hs_zip_pct_nhisp_black','hs_zip_pct_nhisp_native','hs_zip_pct_nhisp_asian',
  'hs_zip_pct_nhisp_nhpi','hs_zip_pct_nhisp_multi','hs_zip_pct_hisp_all',
  'hs_univ_dist'
)

# ---------- INPUT: which universities ----------
univ_ids <- c(
  230959, 216287, 123165, 120254, 115409, 126678, 221519, 204501, 173902,
  128902, 167835, 168342, 147767, 152080, 201645, 139658, 223232, 160755,
  228246, 127060, 168148, 239105, 216597, 164924, 228875, 186867, 100751,
  218663, 139959, 181464, 201885, 215293, 186380, 110635, 110653, 126614,
  155317, 106397, 166629, 110671, 110680, 196097
)

# ---------- DATA PREP ----------
# (If you already did the factor/relevel work upstream, you can skip that here.)
# Add a random continuous regressor `one` ONCE for reproducibility
set.seed(42)
df_work <- pubprivhs_univ_df %>%
  mutate(one = rnorm(n()))

# ---------- SPEC: covariate blocks ----------
rhs_common <- c(
  "hs_control*hs_g11",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean","hs_zip_pct_edu_baplus_all","hs_zip_pct_pov_yes",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)
rhs_with_state <- c(rhs_common, "hs_state_code")

# ---------- FORMULAS (5 models) ----------
# (1) State dummies only + 'one' (to have at least one slope)
form_state_only   <- visit01 ~ one + hs_state_code

# (2) EPS fixed effects only + 'one'
form_eps_only     <- as.formula("visit01 ~ one | hs_eps_codename")

# (3) Covariates only (no state, no EPS FE)
form_cov_only     <- reformulate(rhs_common, response = "visit01")

# (4) Covariates + state dummies
form_cov_state    <- reformulate(rhs_with_state, response = "visit01")

# (5) Covariates + EPS fixed effects
form_cov_epsfe    <- as.formula(
  paste0("visit01 ~ ", paste(rhs_common, collapse = " + "), " | hs_eps_codename")
)

# ---------- HELPER: adjusted overall R² (works for FE & non-FE) ----------
get_adj_r2 <- function(m) {
  # 1) Preferred: fixest API
  out <- tryCatch(fixest::r2(m, type = "ar2"), error = function(e) NA_real_)
  if (!is.na(out)) return(out)
  
  # 2) Fallback: fitstat alias
  out <- tryCatch(fixest::fitstat(m, ~ r2.a)[[1]], error = function(e) NA_real_)
  if (!is.na(out)) return(out)
  
  # 3) Last resort: manual from unadjusted R², N, and # of slopes (excludes FE)
  R2 <- tryCatch(fixest::fitstat(m, ~ r2)[[1]], error = function(e) NA_real_)
  n  <- tryCatch(nobs(m), error = function(e) NA_real_)
  k  <- tryCatch(length(coef(m)), error = function(e) NA_real_)  # slopes only
  if (is.na(R2) || is.na(n) || is.na(k) || (n - k - 1) <= 0) return(NA_real_)
  1 - (1 - R2) * (n - 1) / (n - k - 1)
}

# ----- helper for Within R² (robust across fixest versions) -----
get_within_r2 <- function(m) {
  # try the official API
  out <- tryCatch(fixest::r2(m, type = "within"), error = function(e) NA_real_)
  if (!is.na(out)) return(out)
  
  # fallbacks for older versions
  keys <- list(~ r2_within, ~ wr2, ~ r2.within)
  for (k in keys) {
    out <- tryCatch(fixest::fitstat(m, k)[[1]], error = function(e) NA_real_)
    if (!is.na(out)) return(out)
  }
  NA_real_
}

# ----- run all models for one university, now with Within R² for model (5) -----
run_all_models_for_univ <- function(uid) {
  d <- df_work %>%
    dplyr::filter(univ_id == uid, !is.na(hs_eps_codename))
  
  m1 <- feols(form_state_only, data = d, cluster = ~ hs_state_code)
  m2 <- feols(form_eps_only,   data = d, cluster = ~ hs_state_code)
  m3 <- feols(form_cov_only,   data = d, cluster = ~ hs_state_code)
  m4 <- feols(form_cov_state,  data = d, cluster = ~ hs_state_code)
  m5 <- feols(form_cov_epsfe,  data = d, cluster = ~ hs_state_code)
  
  tibble::tibble(
    univ_id               = uid,
    adjR2_state_only      = get_adj_r2(m1),
    adjR2_eps_fe_only     = get_adj_r2(m2),
    adjR2_cov_only        = get_adj_r2(m3),
    adjR2_cov_state       = get_adj_r2(m4),
    adjR2_cov_eps_fe      = get_adj_r2(m5),
    withinR2_cov_eps_fe   = get_within_r2(m5)   # <-- 6th column
  )
}

# ----- execute & pretty print -----
adjr2_by_univ <- purrr::map_dfr(univ_ids, run_all_models_for_univ)

adjr2_by_univ %>%
  dplyr::mutate(
    dplyr::across(
      -univ_id,
      ~ ifelse(is.na(.x), NA_real_, round(.x, 3))
    )
  ) %>%
  dplyr::arrange(univ_id) %>%
  print(n = Inf)

library(dplyr)

# --- visit counts by university ---------------------------------------
visit_counts <- events_df %>%
  group_by(univ_id) %>%
  summarize(
    n_vis       = n(),
    n_pubhs_vis = sum(event_type == "pub_hs",  na.rm = TRUE),
    n_privhs_vis= sum(event_type == "priv_hs", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(univ_id = as.character(univ_id))

# --- merge model results + metadata + visit counts ---------------------
adjr2_with_meta <- adjr2_by_univ %>%
  mutate(univ_id = as.character(univ_id)) %>%
  left_join(
    univ_df %>% select(univ_id, univ_abbrev, univ_classification, univ_usnwr_rank),
    by = "univ_id"
  ) %>%
  left_join(visit_counts, by = "univ_id") %>%
  select(
    univ_abbrev,
    univ_classification,
    univ_usnwr_rank,
    n_vis, n_pubhs_vis, n_privhs_vis,              # << inserted right after rank
    adjR2_state_only,
    adjR2_eps_fe_only,
    adjR2_cov_only,
    adjR2_cov_state,
    adjR2_cov_eps_fe,
    withinR2_cov_eps_fe
  ) %>%
  arrange(univ_classification, univ_usnwr_rank)

# --- print nicely (round only the R² columns) --------------------------
adjr2_with_meta %>%
  mutate(
    across(starts_with(c("adjR2","withinR2")),
           ~ ifelse(is.na(.x), NA_real_, round(.x, 3)))
  ) %>%
  print(row.names = FALSE, n = Inf)