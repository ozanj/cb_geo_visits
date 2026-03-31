################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < lpm.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/20/2025
## [ DESC ] < linear probability model that school i gets visit, fixed effects for geomarket>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print = 1000)
#options(width = 160)

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES
getwd()
source(file = file.path("scripts", "create_cb_geo_hs_visits.R"))
getwd()

# remove objects from cb_geo (ajs manuscript) mapping
rm(create_rq1_map, format_vars, get_palette)

############

# create data frames for with indicators for whether high school is 2, 1, 0.5 miles from a Geomarket border

hs_geomarket_distance_df <- readRDS(file.path("data", "hs_geomarket_distance_df.RDS")) %>% 
  as_tibble()

hs_geomarket_distance_df %>% glimpse()

hs_geomarket_distance_two_mi  <- hs_geomarket_distance_df %>%
  filter(distance_mi <= 2) %>%
  select(hs_ncessch) %>%
  distinct(hs_ncessch) %>%
  mutate(border_two_mi = 1)

hs_geomarket_distance_one_mi  <- hs_geomarket_distance_df %>%
  filter(distance_mi <= 1) %>%
  select(hs_ncessch) %>%
  distinct(hs_ncessch) %>%
  mutate(border_one_mi = 1)

hs_geomarket_distance_half_mi <- hs_geomarket_distance_df %>%
  filter(distance_mi <= .5) %>%
  select(hs_ncessch) %>%
  distinct(hs_ncessch) %>%
  mutate(border_half_mi = 1)

hs_geomarket_distance_two_mi %>% glimpse()

# merge distance indicators to pubprivhs_univ_df
# replace border_x_mi indicators = 0 for obs that don't merge

pubprivhs_univ_df <- pubprivhs_univ_df %>%
  left_join(hs_geomarket_distance_two_mi,  by = "hs_ncessch") %>%
  left_join(hs_geomarket_distance_one_mi,  by = "hs_ncessch") %>%
  left_join(hs_geomarket_distance_half_mi, by = "hs_ncessch") %>%
  mutate(
    border_two_mi  = if_else(is.na(border_two_mi),  0, border_two_mi),
    border_one_mi  = if_else(is.na(border_one_mi),  0, border_one_mi),
    border_half_mi = if_else(is.na(border_half_mi), 0, border_half_mi)
  )

pubprivhs_univ_df %>% filter(univ_id == "all") %>% count(border_two_mi)
pubprivhs_univ_df %>% filter(univ_id == "all") %>% count(border_one_mi)
pubprivhs_univ_df %>% filter(univ_id == "all") %>% count(border_half_mi)

rm(hs_geomarket_distance_two_mi, hs_geomarket_distance_one_mi, hs_geomarket_distance_half_mi)

##################################################
################################################## MODELING VISITS TO SCHOOL I FROM COLLEGE J,
## SEPARATE MODELS FOR VISITS TO: ALL SCHOOLS; PUBLIC SCHOOLS; PRIVATE SCHOOLS
##################################################

# ============================
# Data (ALL HS sample baseline)
# ============================

set.seed(42)

df_all <- pubprivhs_univ_df %>%
  filter(!is.na(hs_eps_codename)) %>%
  filter(univ_id != "all") %>%
  mutate(one = rnorm(n()))   # helper slope so FE-only specs print cleanly

# Subsamples
df_pub  <- df_all %>% filter(hs_control == "public")
df_priv <- df_all %>% filter(hs_control == "private")

# ============================
# Covariate blocks
# ============================

# ALL HS (public + private together)
rhs_common_ij <- c(
  "hs_control*hs_g12",
  "hs_pct_asian", "hs_pct_black", "hs_pct_hispanic", "hs_pct_amerindian",
  "hs_pct_nativehawaii", "hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean_decile", "hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes", "I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black", "hs_zip_pct_nhisp_native", "hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi", "hs_zip_pct_nhisp_multi", "hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# PUBLIC HS
rhs_pub_ij <- c(
  "hs_g12",
  "hs_pct_asian", "hs_pct_black", "hs_pct_hispanic", "hs_pct_amerindian",
  "hs_pct_nativehawaii", "hs_pct_tworaces",
  "hs_overall_niche_letter_grade", "hs_magnet01", "hs_school_type",
  "hs_pct_free_reduced_lunch", "hs_pct_prof_math", "hs_pct_prof_rla",
  "hs_zip_inc_house_mean_decile", "hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes", "I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black", "hs_zip_pct_nhisp_native", "hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi", "hs_zip_pct_nhisp_multi", "hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# PRIVATE HS
rhs_priv_ij <- c(
  "hs_g12",
  "hs_pct_asian", "hs_pct_black", "hs_pct_hispanic", "hs_pct_amerindian",
  "hs_pct_nativehawaii", "hs_pct_tworaces",
  "hs_school_type", "hs_religion_5", "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean_decile", "hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes", "I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black", "hs_zip_pct_nhisp_native", "hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi", "hs_zip_pct_nhisp_multi", "hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# ============================
# Helpers
# ============================

mk_forms <- function(rhs_vec) {
  rhs_str <- paste(rhs_vec, collapse = " + ")
  list(
    # (1) Univ FE only
    univ             = as.formula("visit01 ~ one | univ_id"),
    # (2) Univ × State FE only
    univ_state       = as.formula("visit01 ~ one | univ_id^hs_state_code"),
    # (3) Univ × County FE only
    univ_county      = as.formula("visit01 ~ one | univ_id^hs_county_geoid"),
    # (4) Univ × EPS FE only
    univ_eps         = as.formula("visit01 ~ one | univ_id^hs_eps_codename"),
    # (5) Covariates + Univ FE
    cov_univ         = as.formula(paste0("visit01 ~ ", rhs_str, " | univ_id")),
    # (6) Covariates + Univ × State FE
    cov_univ_state   = as.formula(paste0("visit01 ~ ", rhs_str, " | univ_id^hs_state_code")),
    # (7) Covariates + Univ × County FE
    cov_univ_county  = as.formula(paste0("visit01 ~ ", rhs_str, " | univ_id^hs_county_geoid")),
    # (8) Covariates + Univ × EPS FE
    cov_univ_eps     = as.formula(paste0("visit01 ~ ", rhs_str, " | univ_id^hs_eps_codename"))
  )
}

# Standalone helper: count distinct hs_ncessch in the rows actually used by a
# fitted feols model. Handles NA removal (complete.cases) and fixest's iterative
# singleton removal. Used by both fit_models and fit_models_border_compare.
n_hs_in_model <- function(mod, data) {
  vars_used    <- all.vars(formula(mod))
  vars_present <- vars_used[vars_used %in% names(data)]
  d <- data[complete.cases(data[, vars_present, drop = FALSE]), ]
  
  # If complete cases exceed nobs, fixest also dropped singletons — remove them
  if (nrow(d) > nobs(mod)) {
    fe_rhs <- tryCatch({
      rhs <- formula(mod)[[3]]
      if (length(rhs) == 3L && as.character(rhs[[1L]]) == "|") rhs[[3L]] else NULL
    }, error = function(e) NULL)
    
    if (!is.null(fe_rhs)) {
      fe_vars <- all.vars(fe_rhs)
      fe_vars <- fe_vars[fe_vars %in% names(d)]
      fe_key  <- do.call(paste, c(lapply(fe_vars, function(v) d[[v]]), sep = "\x01"))
      repeat {
        cnts <- table(fe_key)
        bad  <- names(cnts[cnts == 1L])
        if (length(bad) == 0L) break
        keep   <- !(fe_key %in% bad)
        d      <- d[keep, ]
        fe_key <- fe_key[keep]
      }
    }
  }
  
  dplyr::n_distinct(d$hs_ncessch)
}

fit_models <- function(data, rhs_vec) {
  f <- mk_forms(rhs_vec)
  
  fit_and_tag <- function(formula) {
    mod <- feols(formula, data = data, cluster = ~ hs_state_code + univ_id)
    attr(mod, "n_hs") <- n_hs_in_model(mod, data)
    mod
  }
  
  list(
    "(1) Univ FE"                  = fit_and_tag(f$univ),
    "(2) Univ × State FE"          = fit_and_tag(f$univ_state),
    "(3) Univ × County FE"         = fit_and_tag(f$univ_county),
    "(4) Univ × EPS FE"            = fit_and_tag(f$univ_eps),
    "(5) Covar + Univ FE"          = fit_and_tag(f$cov_univ),
    "(6) Covar + Univ × State FE"  = fit_and_tag(f$cov_univ_state),
    "(7) Covar + Univ × County FE" = fit_and_tag(f$cov_univ_county),
    "(8) Covar + Univ × EPS FE"    = fit_and_tag(f$cov_univ_eps)
  )
}

# Reads n_hs from the attribute tagged at fit time — no data closure needed.
extra_gof_fixest <- function(model) {
  data.frame(
    "N schools"            = attr(model, "n_hs"),
    "Parameters (incl. FE)" = unname(model$nparams),
    "Residual df"           = unname(nobs(model) - model$nparams),
    check.names             = FALSE
  )
}

# footnote for paper/regression table
reg_table_note <- paste(
  "Parameters include absorbed fixed effects;",
  "counts reflect the number of identified coefficients after normalization."
)

mk_table <- function(models_list) {
  
  gof_map <- data.frame(
    raw = c(
      "N schools",
      "nobs",
      "Parameters (incl. FE)",
      "Residual df",
      "r.squared",
      "adj.r.squared",
      "r2.within",
      "rmse"
    ),
    clean = c(
      "N schools",
      "N (schools X colleges)",
      "Parameters (incl. FE)",
      "Residual df",
      "R²",
      "Adj. R²",
      "Within R²",
      "RMSE"
    ),
    fmt = c(0, 0, 0, 0, 3, 3, 3, 3),
    stringsAsFactors = FALSE
  )
  
  tab <- modelsummary(
    models_list,
    estimate      = "{estimate}{stars}",
    statistic     = "({std.error})",
    stars         = c("*" = .05, "**" = .01, "***" = .001),
    gof_function  = extra_gof_fixest,
    gof_map       = gof_map,
    output        = "data.frame"
  )
  
  # Identify model columns
  model_cols <- setdiff(names(tab), c("term", "part", "statistic"))
  
  # Remove helper 'one'
  tab <- tab %>% dplyr::filter(term != "one")
  
  # Identify SE rows
  se_rows <- apply(tab[model_cols], 1, function(x) {
    any(grepl("^\\(", ifelse(is.na(x), "", x)))
  })
  
  # Create blocks (coef + SE row)
  block_id <- cumsum(!se_rows)
  tab$block_id <- block_id
  
  # Figure out order of blocks
  first_terms <- tab %>%
    dplyr::filter(!se_rows) %>%
    dplyr::group_by(block_id) %>%
    dplyr::summarise(term_first = dplyr::first(term), .groups = "drop") %>%
    dplyr::mutate(
      block_order = dplyr::case_when(
        term_first == "hs_controlprivate"             ~ 1L,
        term_first == "hs_g12"                        ~ 2L,
        term_first == "hs_controlprivate:hs_g12"      ~ 3L,
        TRUE                                          ~ 100L + dplyr::row_number()
      )
    )
  
  # Join block order and reorder
  tab <- tab %>%
    dplyr::left_join(first_terms, by = "block_id") %>%
    dplyr::arrange(block_order, block_id)
  
  # Remove helper ordering column before output
  tab <- tab %>% dplyr::select(-term_first)
  
  # Blank SE term labels
  se_rows_reordered <- apply(tab[model_cols], 1, function(x) {
    any(grepl("^\\(", ifelse(is.na(x), "", x)))
  })
  tab$term[se_rows_reordered] <- ""
  
  # Final cleanup
  tab %>%
    dplyr::select(-c(part, statistic, block_id, block_order))
}

# ============================
# Fit & print: ALL / PUBLIC / PRIVATE
# ============================

mods_all  <- fit_models(df_all,  rhs_common_ij)
mods_pub  <- fit_models(df_pub,  rhs_pub_ij)
mods_priv <- fit_models(df_priv, rhs_priv_ij)

cat("\n================  ALL HIGH SCHOOLS  ================\n")
tab_all <- mk_table(mods_all)
print(tab_all, row.names = FALSE)
saveRDS(tab_all, file = "results/reg_tab_all_ij.RDS")

cat("\n================  PUBLIC HIGH SCHOOLS  ================\n")
tab_pub <- mk_table(mods_pub)
print(tab_pub, row.names = FALSE)
saveRDS(tab_pub, file = "results/tab_pub_ij.RDS")

cat("\n================  PRIVATE HIGH SCHOOLS  ================\n")
tab_priv <- mk_table(mods_priv)
print(tab_priv, row.names = FALSE)
saveRDS(tab_priv, file = "results/tab_priv_ij.RDS")

##################################################
################################################## BORDER COMPARISON MODELS
## COMPARE UNIV × STATE FE VS UNIV × EPS FE
## ACROSS UNRESTRICTED AND BORDER-RESTRICTED SAMPLES
##################################################

# REGRESSIONS TO RUN
# ALL REGRESSIONS COMPARE: UNIV × STATE FE vs UNIV × EPS FE
# COLUMNS: NO RESTRICTION / ≤ 2 MILES / ≤ 1 MILE / ≤ 0.5 MILES × STATE FE / EPS FE
# PRINTED AS TWO TABLES PER SAMPLE GROUP:
# TABLE A: NO COVARIATES (8 MODELS)
# TABLE B: WITH COVARIATES (8 MODELS)
# DONE SEPARATELY FOR: ALL / PUBLIC / PRIVATE HIGH SCHOOLS

# ============================
# Border comparison model fitter
# ============================

fit_models_border_compare <- function(data, rhs_vec) {
  
  rhs_str <- paste(rhs_vec, collapse = " + ")
  
  f_no_cov_state <- as.formula("visit01 ~ one | univ_id^hs_state_code")
  f_no_cov_eps   <- as.formula("visit01 ~ one | univ_id^hs_eps_codename")
  f_cov_state    <- as.formula(paste0("visit01 ~ ", rhs_str, " | univ_id^hs_state_code"))
  f_cov_eps      <- as.formula(paste0("visit01 ~ ", rhs_str, " | univ_id^hs_eps_codename"))
  
  # Sample restrictions
  d_no_border <- data
  d_two_mi    <- data %>% filter(border_two_mi  == 1)
  d_one_mi    <- data %>% filter(border_one_mi  == 1)
  d_half_mi   <- data %>% filter(border_half_mi == 1)
  
  fit_and_tag <- function(formula, d) {
    mod <- feols(formula, data = d, cluster = ~ hs_state_code + univ_id)
    attr(mod, "n_hs") <- n_hs_in_model(mod, d)
    mod
  }
  
  list(
    # ---- No-covariate models (1–8) ----
    "(1) State"          = fit_and_tag(f_no_cov_state, d_no_border),
    "(2) EPS"            = fit_and_tag(f_no_cov_eps,   d_no_border),
    "(3) ≤2mi, State"    = fit_and_tag(f_no_cov_state, d_two_mi),
    "(4) ≤2mi, EPS"      = fit_and_tag(f_no_cov_eps,   d_two_mi),
    "(5) ≤1mi, State"    = fit_and_tag(f_no_cov_state, d_one_mi),
    "(6) ≤1mi, EPS"      = fit_and_tag(f_no_cov_eps,   d_one_mi),
    "(7) ≤0.5mi, State"  = fit_and_tag(f_no_cov_state, d_half_mi),
    "(8) ≤0.5mi, EPS"    = fit_and_tag(f_no_cov_eps,   d_half_mi),
    
    # ---- Covariate models (9–16, renumbered 1–8 at print time) ----
    "(9) Cov, State"          = fit_and_tag(f_cov_state, d_no_border),
    "(10) Cov, EPS"           = fit_and_tag(f_cov_eps,   d_no_border),
    "(11) Cov, ≤2mi, State"   = fit_and_tag(f_cov_state, d_two_mi),
    "(12) Cov, ≤2mi, EPS"     = fit_and_tag(f_cov_eps,   d_two_mi),
    "(13) Cov, ≤1mi, State"   = fit_and_tag(f_cov_state, d_one_mi),
    "(14) Cov, ≤1mi, EPS"     = fit_and_tag(f_cov_eps,   d_one_mi),
    "(15) Cov, ≤0.5mi, State" = fit_and_tag(f_cov_state, d_half_mi),
    "(16) Cov, ≤0.5mi, EPS"   = fit_and_tag(f_cov_eps,   d_half_mi)
  )
}

# Helper: split 16-model list into no-cov (1–8) and cov (9–16) sublists.
# Covariate models are renumbered (1)–(8) so column headers print cleanly.
split_border_compare <- function(mods) {
  no_cov <- mods[1:8]
  cov    <- mods[9:16]
  
  # Strip original (9)–(16) prefix and replace with (1)–(8)
  names(cov) <- paste0("(", seq_along(cov), ") ",
                       sub("^\\([0-9]+\\) ", "", names(cov)))
  list(no_cov = no_cov, cov = cov)
}

# ============================
# Fit all 16 models per sample group
# ============================

mods_all_bc  <- fit_models_border_compare(df_all,  rhs_common_ij)
mods_pub_bc  <- fit_models_border_compare(df_pub,  rhs_pub_ij)
mods_priv_bc <- fit_models_border_compare(df_priv, rhs_priv_ij)

# ============================
# Print & save: ALL HIGH SCHOOLS
# ============================

cat("\n================  ALL HS — BORDER COMPARISON, NO COVARIATES  ================\n")
tab_all_bc_nocov <- mk_table(split_border_compare(mods_all_bc)$no_cov)
print(tab_all_bc_nocov, row.names = FALSE)
saveRDS(tab_all_bc_nocov, file = "results/tab_all_border_compare_nocov_ij.RDS")

cat("\n================  ALL HS — BORDER COMPARISON, WITH COVARIATES  ================\n")
tab_all_bc_cov <- mk_table(split_border_compare(mods_all_bc)$cov)
print(tab_all_bc_cov, row.names = FALSE)
saveRDS(tab_all_bc_cov, file = "results/tab_all_border_compare_cov_ij.RDS")

# ============================
# Print & save: PUBLIC HIGH SCHOOLS
# ============================

cat("\n================  PUBLIC HS — BORDER COMPARISON, NO COVARIATES  ================\n")
tab_pub_bc_nocov <- mk_table(split_border_compare(mods_pub_bc)$no_cov)
print(tab_pub_bc_nocov, row.names = FALSE)
saveRDS(tab_pub_bc_nocov, file = "results/tab_pub_border_compare_nocov_ij.RDS")

cat("\n================  PUBLIC HS — BORDER COMPARISON, WITH COVARIATES  ================\n")
tab_pub_bc_cov <- mk_table(split_border_compare(mods_pub_bc)$cov)
print(tab_pub_bc_cov, row.names = FALSE)
saveRDS(tab_pub_bc_cov, file = "results/tab_pub_border_compare_cov_ij.RDS")

# ============================
# Print & save: PRIVATE HIGH SCHOOLS
# ============================

cat("\n================  PRIVATE HS — BORDER COMPARISON, NO COVARIATES  ================\n")
tab_priv_bc_nocov <- mk_table(split_border_compare(mods_priv_bc)$no_cov)
print(tab_priv_bc_nocov, row.names = FALSE)
saveRDS(tab_priv_bc_nocov, file = "results/tab_priv_border_compare_nocov_ij.RDS")

cat("\n================  PRIVATE HS — BORDER COMPARISON, WITH COVARIATES  ================\n")
tab_priv_bc_cov <- mk_table(split_border_compare(mods_priv_bc)$cov)
print(tab_priv_bc_cov, row.names = FALSE)
saveRDS(tab_priv_bc_cov, file = "results/tab_priv_border_compare_cov_ij.RDS")

##################################################
################################################## HOW SIMILAR ARE SCHOOLS BEFORE/AFTER RESTRICTING BORDER; CRYSTAL -- HELP HERE. 
##################################################

hs_df <- pubprivhs_univ_df %>% filter(univ_id=='all',hs_control=='public',!is.na(hs_pct_free_reduced_lunch))

hs_df %>% # start with data frame object ipeds_hc_temp
  group_by(hs_ncessch) %>% # group by unitid
  summarise(n_per_group=n()) %>% # create measure of number of obs per group
  ungroup %>% # ungroup (otherwise frequency table [next step] created) separately for each group (i.e., separate frequency table for each value of unitid)
  count(n_per_group)


# helper:
# for a given sample, compute geomarket means of hs_pct_free_reduced_lunch,
# then summarize dispersion across geomarkets
summarize_geomarket_frpl <- function(df) {
  
  gm_means <- df %>%
    filter(!is.na(hs_eps_codename), !is.na(hs_pct_free_reduced_lunch)) %>%
    group_by(hs_eps_codename) %>%
    summarise(
      geomarket_mean_frpl = mean(hs_pct_free_reduced_lunch, na.rm = TRUE),
      .groups = "drop"
    )
  
  tibble(
    n_schools = n_distinct(
      df$hs_ncessch[!is.na(df$hs_pct_free_reduced_lunch) & !is.na(df$hs_eps_codename)]
    ),
    mean_geomarket_mean_frpl = mean(gm_means$geomarket_mean_frpl, na.rm = TRUE),
    sd_geomarket_mean_frpl   = sd(gm_means$geomarket_mean_frpl, na.rm = TRUE)
  )
}

# build summaries for each restriction
frpl_none <- hs_df %>%
  summarize_geomarket_frpl() %>%
  mutate(sample = "No border restriction")

frpl_2mi <- hs_df %>%
  filter(border_two_mi == 1) %>%
  summarize_geomarket_frpl() %>%
  mutate(sample = "≤ 2 miles")

frpl_1mi <- hs_df %>%
  filter(border_one_mi == 1) %>%
  summarize_geomarket_frpl() %>%
  mutate(sample = "≤ 1 mile")

frpl_halfmi <- hs_df %>%
  filter(border_half_mi == 1) %>%
  summarize_geomarket_frpl() %>%
  mutate(sample = "≤ 0.5 miles")

# combine and compute percent reduction in SD relative to unrestricted
frpl_similarity_table <- bind_rows(
  frpl_none,
  frpl_2mi,
  frpl_1mi,
  frpl_halfmi
) %>%
  mutate(
    pct_reduction_sd = 100 * (first(sd_geomarket_mean_frpl) - sd_geomarket_mean_frpl) / first(sd_geomarket_mean_frpl)
  ) %>%
  select(
    sample,
    n_schools,
    mean_geomarket_mean_frpl,
    sd_geomarket_mean_frpl,
    pct_reduction_sd
  )

frpl_similarity_table

# Same basic story as the income variable: this version is also going the wrong
# way for your intended claim.
#
# As the sample gets tighter around borders, between-Geomarket dispersion
# increases:
#
# unrestricted: SD of Geomarket mean FRPL = 16.3
# <= 2 miles: 21.0
# <= 1 mile: 22.3
# <= 0.5 miles: 24.7
#
# So this table would not support the argument that border restriction makes
# schools across Geomarkets more similar on observables.
#
# High-level, that strengthens my view that the global dispersion across all
# Geomarkets is the wrong descriptive metric for your purpose. It is probably
# picking up the fact that border-near subsets are selective and may accentuate
# certain kinds of Geomarket contrast rather than averaging them away.
#
# So I would not keep pushing this approach. I'd pivot fully to the
# adjacent-pair approach for both income and FRPL.
#
# For FRPL, the next thing I'd do is:
# - identify each adjacent Geomarket pair
# - for each pair, compute the absolute difference in mean FRPL
# - do that for unrestricted, <= 2 miles, <= 1 mile, <= 0.5 miles
# - summarize with:
#   - N adjacent pairs
#   - Mean absolute difference in mean FRPL
#   - maybe Median absolute difference in mean FRPL
#
# That is much more aligned with the local-border logic.
#
# If you want, the next step should be to write the FRPL adjacent-pair code
# parallel to the income code.

##################################################
################################################## WRITE FUNCTIONS TO RUN ALL MODELS, SEPARATELY FOR VITIS TO: ALL SCHOOLS; PUBLIC SCHOOLS; PRIVATE SCHOOLS
##################################################

##################################################
# RUN 7 MODELS PER UNIVERSITY:
# ALL SCHOOLS, PUBLIC-ONLY, PRIVATE-ONLY
# (State / EPS / County entered as FE)
##################################################

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
set.seed(42)  # only used for Models (1) & (2) to have a slope
df_work <- pubprivhs_univ_df %>%
  mutate(one = rnorm(n()))

# ============================
# Covariate blocks
# ============================
# ALL schools
rhs_common <- c(
  "hs_control*hs_g12",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# PUBLIC HS
rhs_terms_pub_sch <- c(
  "hs_g12",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade","hs_magnet01","hs_school_type",
  "hs_pct_free_reduced_lunch","hs_pct_prof_math","hs_pct_prof_rla",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# PRIVATE HS
rhs_terms_priv_sch <- c(
  "hs_g12",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_school_type","hs_religion_5","hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile",
  "hs_zip_pct_pov_yes","I(hs_zip_pct_pov_yes^2)",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)

# ============================
# Formulas (helper + build)
# ============================
mk_form <- function(rhs, fe = NULL) {
  rhs_str <- if (length(rhs) == 1 && grepl("~", rhs)) {
    sub(".*~", "", rhs)
  } else {
    paste(rhs, collapse = " + ")
  }
  fe_part <- if (is.null(fe)) "" else paste0(" | ", fe)
  as.formula(paste0("visit01 ~ ", rhs_str, fe_part))
}

# ----- ALL schools (7 specs) -----
form_state_only    <- mk_form("one",      "hs_state_code")    # 1) State-only FE
form_county_only   <- mk_form("one",      "hs_county_geoid")  # 2) County-only FE
form_eps_only      <- mk_form("one",      "hs_eps_codename")  # 3) EPS-only FE
form_cov_only      <- mk_form(rhs_common, NULL)               # 4) Covariates only
form_cov_state     <- mk_form(rhs_common, "hs_state_code")    # 5) Covariates + State FE
form_cov_county    <- mk_form(rhs_common, "hs_county_geoid")  # 6) Covariates + County FE
form_cov_epsfe     <- mk_form(rhs_common, "hs_eps_codename")  # 7) Covariates + EPS FE

# ----- PUBLIC HS (7 specs) -----
form_pub_state_only  <- mk_form("one",             "hs_state_code")
form_pub_county_only <- mk_form("one",             "hs_county_geoid")
form_pub_eps_only    <- mk_form("one",             "hs_eps_codename")
form_pub_cov_only    <- mk_form(rhs_terms_pub_sch, NULL)
form_pub_cov_state   <- mk_form(rhs_terms_pub_sch, "hs_state_code")
form_pub_cov_county  <- mk_form(rhs_terms_pub_sch, "hs_county_geoid")
form_pub_cov_epsfe   <- mk_form(rhs_terms_pub_sch, "hs_eps_codename")

# ----- PRIVATE HS (7 specs) -----
form_priv_state_only  <- mk_form("one",              "hs_state_code")
form_priv_county_only <- mk_form("one",              "hs_county_geoid")
form_priv_eps_only    <- mk_form("one",              "hs_eps_codename")
form_priv_cov_only    <- mk_form(rhs_terms_priv_sch, NULL)
form_priv_cov_state   <- mk_form(rhs_terms_priv_sch, "hs_state_code")
form_priv_cov_county  <- mk_form(rhs_terms_priv_sch, "hs_county_geoid")
form_priv_cov_epsfe   <- mk_form(rhs_terms_priv_sch, "hs_eps_codename")

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

# Safe wrapper (cluster by state; per-university regressions)
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
  m1 <- safe_feols(form_state_only,   d)  # State-only FE
  m2 <- safe_feols(form_county_only,  d)  # County-only FE
  m3 <- safe_feols(form_eps_only,     d)  # EPS-only FE
  m4 <- safe_feols(form_cov_only,     d)  # Covariates only
  m5 <- safe_feols(form_cov_state,    d)  # Covariates + State FE
  m6 <- safe_feols(form_cov_county,   d)  # Covariates + County FE
  m7 <- safe_feols(form_cov_epsfe,    d)  # Covariates + EPS FE
  tibble(
    univ_id                  = uid,
    adjR2_state_only         = adj2_safe(m1),
    adjR2_county_fe_only     = adj2_safe(m2),
    adjR2_eps_fe_only        = adj2_safe(m3),
    adjR2_cov_only           = adj2_safe(m4),
    adjR2_cov_state          = adj2_safe(m5),
    adjR2_cov_county_fe      = adj2_safe(m6),
    adjR2_cov_eps_fe         = adj2_safe(m7),
    withinR2_cov_county_fe   = within2_safe(m6),
    withinR2_cov_eps_fe      = within2_safe(m7)
  )
}

# --- PUBLIC or PRIVATE for one university ---
run_all_models_for_univ_sector <- function(uid, sector = c("public","private")) {
  sector <- match.arg(sector)
  d <- df_work %>%
    filter(univ_id == uid, !is.na(hs_eps_codename), hs_control == sector)
  
  if (sector == "public") {
    m1 <- safe_feols(form_pub_state_only,  d)
    m2 <- safe_feols(form_pub_county_only, d)
    m3 <- safe_feols(form_pub_eps_only,    d)
    m4 <- safe_feols(form_pub_cov_only,    d)
    m5 <- safe_feols(form_pub_cov_state,   d)
    m6 <- safe_feols(form_pub_cov_county,  d)
    m7 <- safe_feols(form_pub_cov_epsfe,   d)
    tibble(
      univ_id                        = uid,
      adjR2_pub_state_only           = adj2_safe(m1),
      adjR2_pub_county_fe_only       = adj2_safe(m2),
      adjR2_pub_eps_fe_only          = adj2_safe(m3),
      adjR2_pub_cov_only             = adj2_safe(m4),
      adjR2_pub_cov_state            = adj2_safe(m5),
      adjR2_pub_cov_county_fe        = adj2_safe(m6),
      adjR2_pub_cov_eps_fe           = adj2_safe(m7),
      withinR2_pub_cov_county_fe     = within2_safe(m6),
      withinR2_pub_cov_eps_fe        = within2_safe(m7)
    )
  } else {
    m1 <- safe_feols(form_priv_state_only,  d)
    m2 <- safe_feols(form_priv_county_only, d)
    m3 <- safe_feols(form_priv_eps_only,    d)
    m4 <- safe_feols(form_priv_cov_only,    d)
    m5 <- safe_feols(form_priv_cov_state,   d)
    m6 <- safe_feols(form_priv_cov_county,  d)
    m7 <- safe_feols(form_priv_cov_epsfe,   d)
    tibble(
      univ_id                         = uid,
      adjR2_priv_state_only           = adj2_safe(m1),
      adjR2_priv_county_fe_only       = adj2_safe(m2),
      adjR2_priv_eps_fe_only          = adj2_safe(m3),
      adjR2_priv_cov_only             = adj2_safe(m4),
      adjR2_priv_cov_state            = adj2_safe(m5),
      adjR2_priv_cov_county_fe        = adj2_safe(m6),
      adjR2_priv_cov_eps_fe           = adj2_safe(m7),
      withinR2_priv_cov_county_fe     = within2_safe(m6),
      withinR2_priv_cov_eps_fe        = within2_safe(m7)
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
# Visit counts (optional summary)
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
    adjR2_state_only, adjR2_county_fe_only, adjR2_eps_fe_only, adjR2_cov_only,
    adjR2_cov_state,  adjR2_cov_county_fe,  adjR2_cov_eps_fe,
    withinR2_cov_county_fe, withinR2_cov_eps_fe
  ) %>%
  arrange(univ_classification, univ_usnwr_rank)

# --- PUBLIC HS table ---
adjr2_pub_with_meta <- adjr2_pub_by_univ %>%
  mutate(univ_id = as.character(univ_id)) %>%
  left_join(meta_small, by = "univ_id") %>%
  left_join(visit_counts_pub, by = "univ_id") %>%
  select(
    univ_abbrev, univ_classification, univ_usnwr_rank, n_pubhs_vis,
    adjR2_pub_state_only, adjR2_pub_county_fe_only, adjR2_pub_eps_fe_only, adjR2_pub_cov_only,
    adjR2_pub_cov_state,  adjR2_pub_cov_county_fe,  adjR2_pub_cov_eps_fe,
    withinR2_pub_cov_county_fe, withinR2_pub_cov_eps_fe
  ) %>%
  arrange(univ_classification, univ_usnwr_rank)

# --- PRIVATE HS table ---
adjr2_priv_with_meta <- adjr2_priv_by_univ %>%
  mutate(univ_id = as.character(univ_id)) %>%
  left_join(meta_small, by = "univ_id") %>%
  left_join(visit_counts_priv, by = "univ_id") %>%
  select(
    univ_abbrev, univ_classification, univ_usnwr_rank, n_privhs_vis,
    adjR2_priv_state_only, adjR2_priv_county_fe_only, adjR2_priv_eps_fe_only, adjR2_priv_cov_only,
    adjR2_priv_cov_state,  adjR2_priv_cov_county_fe,  adjR2_priv_cov_eps_fe,
    withinR2_priv_cov_county_fe, withinR2_priv_cov_eps_fe
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
adjr2_all_with_meta %>% round_r2_cols() %>% select(-c(univ_classification)) %>%  print(row.names = FALSE, n = Inf)
saveRDS(adjr2_all_with_meta, file = 'results/adjr2_all_with_meta.RDS')

cat("\n=== PUBLIC HS only ===\n")
adjr2_pub_with_meta %>% round_r2_cols() %>% print(row.names = FALSE, n = Inf)
saveRDS(adjr2_pub_with_meta, file = 'results/adjr2_pub_with_meta.RDS')

cat("\n=== PRIVATE HS only ===\n")
adjr2_priv_with_meta %>% round_r2_cols() %>% print(row.names = FALSE, n = Inf)
saveRDS(adjr2_priv_with_meta, file = 'results/adjr2_priv_with_meta.RDS')


##################################################
################################################## MODELING ONE UNIVERSITY AT A TIME; CLAUDE START HERE 3/17/2026!
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
# Common covariates (NO state here). Note: hs_g12 enters via hs_control*hs_g12.
rhs_common <- c(
  "hs_control*hs_g12",
  "hs_pct_asian","hs_pct_black","hs_pct_hispanic","hs_pct_amerindian",
  "hs_pct_nativehawaii","hs_pct_tworaces",
  "hs_overall_niche_letter_grade",
  "hs_zip_inc_house_mean_decile","hs_zip_pct_edu_baplus_all_decile","hs_zip_pct_pov_yes_decile",
  "hs_zip_pct_nhisp_black","hs_zip_pct_nhisp_native","hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi","hs_zip_pct_nhisp_multi","hs_zip_pct_hisp_all",
  "hs_univ_dist"
)


# ---------- FORMULAS ----------
# Build a single RHS string
rhs_str <- paste(rhs_common, collapse = " + ")
rhs_str

# Small helper to add optional fixed effects after the "|"
mk_form <- function(rhs, fe = NULL) {
  fe_part <- if (is.null(fe)) "" else paste0(" | ", fe)
  as.formula(paste0("visit01 ~ ", rhs, fe_part))
}

# (1) State fixed effects only (+ 'one' slope to show a coeff)
form_state_only   <- mk_form("one", "hs_state_code")

# (2) County fixed effects only (+ 'one' slope)
form_county_only  <- mk_form("one", "hs_county_geoid")

# (3) EPS fixed effects only (+ 'one' slope)
form_eps_only     <- mk_form("one", "hs_eps_codename")

# (4) Covariates only (no fixed effects)
form_cov_only     <- mk_form(rhs_str, NULL)

# (5) Covariates + state fixed effects
form_cov_state    <- mk_form(rhs_str, "hs_state_code")

# (6) Covariates + County fixed effects
form_cov_county   <- mk_form(rhs_str, "hs_county_geoid")

# (7) Covariates + EPS fixed effects
form_cov_eps      <- mk_form(rhs_str, "hs_eps_codename")

# Quick peek (optional)
form_state_only
form_county_only
form_eps_only
form_cov_only
form_cov_state
form_cov_county
form_cov_eps


# ---------- FIT MODELS (clustered by state) ----------
m_state_only   <- feols(form_state_only,  data = df_sub, cluster = ~ hs_state_code)  # (1)
m_county_only  <- feols(form_county_only, data = df_sub, cluster = ~ hs_state_code)  # (2)
m_eps_only     <- feols(form_eps_only,    data = df_sub, cluster = ~ hs_state_code)  # (3)
m_cov_only     <- feols(form_cov_only,    data = df_sub, cluster = ~ hs_state_code)  # (4)
m_cov_state    <- feols(form_cov_state,   data = df_sub, cluster = ~ hs_state_code)  # (5)
m_cov_county   <- feols(form_cov_county,  data = df_sub, cluster = ~ hs_state_code)  # (6)
m_cov_eps      <- feols(form_cov_eps,     data = df_sub, cluster = ~ hs_state_code)  # (7)

# ---------- SIDE-BY-SIDE TABLE ----------
mods <- list(
  "(1) State FE only"           = m_state_only,
  "(2) County FE only"          = m_county_only,
  "(3) EPS FE only"             = m_eps_only,
  "(4) Covariates only"         = m_cov_only,
  "(5) Covariates + State FE"   = m_cov_state,
  "(6) Covariates + County FE"  = m_cov_county,
  "(7) Covariates + EPS FE"     = m_cov_eps
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

# Omit helper 'one' row (and any explicit state rows if present)
tab_df <- tab_df %>%
  dplyr::filter(!grepl("^hs_state_code", term)) %>%
  dplyr::filter(term != "one")

# SE rows: any model column begins with "("
se_rows <- apply(tab_df[model_cols], 1, function(x) {
  any(grepl("^\\(", ifelse(is.na(x), "", x)))
})

# Blank the variable name on SE rows
tab_df$term[se_rows] <- ""

# Print cleanly (no gridlines, no extra cols)
tab_df %>%
  dplyr::select(-c(part, statistic)) %>%
  print(row.names = FALSE)
