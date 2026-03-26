################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits >
## [ FILE ] < rq3_lpm_v3_rewrite.R >
## [ AUTH ] < Ozan Jaquette + ChatGPT draft >
## [ INIT ] < 2026-03-25 >
## [ DESC ] < RQ3 pooled public-school interaction models, automated across
##            popularity measures and interaction variables; saves one master
##            RDS with complete fitted-model results and labeled metadata >
################################################################################

### SETTINGS -------------------------------------------------------------------

# rm(list = ls())
options(max.print = 1500)

library(tidyverse)
library(forcats)
library(fixest)

### LOAD UPSTREAM OBJECTS -------------------------------------------------------

getwd()
source(file = file.path("scripts", "create_cb_geo_hs_visits.R"))
getwd()

# remove mapping helper objects if present
rm(list = intersect(c("create_rq1_map", "format_vars", "get_palette"), ls()))

getwd()
source(file = file.path("scripts", "create_univ_geo_df.R"))
getwd()

# quick look at main input data frames
pubprivhs_univ_df %>% glimpse()
df_by_univ_eps %>% glimpse()



### USER-SET PARAMETERS ---------------------------------------------------------
# Change these first, then run the script from the top.

# Outcome: fixed here to LPM any-visit model.
outcome_var <- "visit01"

# Optional sample restriction by market segment at the HS × university level.
# Set to NULL for all, or e.g. c("regional", "national")
market_subset <- NULL

# Which EPS sector to use when constructing popularity variables.
# Supported here: "all" or "pub"
pop_school_type <- "pub"

# Popularity numerator. Supported here: "vis01" or "vistot"
pop_visit_type <- "vistot"

# Fixed effects and clustering
fe_rhs <- "univ_id^hs_state_code"
cluster_var <- "hs_state_code"

# Save output?
save_outputs <- TRUE
outfile_rds <- "results/rq3_pub_interaction_models_master.RDS"

# Optional run label for bookkeeping
run_label <- "rq3 public-school interaction models"


### CONTROL ARCHITECTURE --------------------------------------------------------
# Keep one stable core, then toggle add-on blocks on/off at the top.
# The interaction variable will be added automatically to the baseline and
# interaction model RHS, and removed from the assembled control vector if it
# already appears there.

core_controls <- c(
  "hs_g12",
  "hs_overall_niche_letter_grade",
  "hs_magnet01",
  "hs_pct_prof_math",
  "hs_pct_prof_rla",
  "hs_pct_free_reduced_lunch_decile",
  "hs_zip_inc_house_mean_decile",
  "hs_pct_bl_hisp_nat_decile",
  "hs_zip_pct_edu_baplus_all_decile",
  "hs_univ_dist"
)

zip_race_controls <- c(
  "hs_zip_pct_nhisp_black",
  "hs_zip_pct_nhisp_native",
  "hs_zip_pct_nhisp_asian",
  "hs_zip_pct_nhisp_nhpi",
  "hs_zip_pct_nhisp_multi",
  "hs_zip_pct_hisp_all"
)

zip_poverty_controls <- c(
  "hs_zip_pct_pov_yes",
  "I(hs_zip_pct_pov_yes^2)"
)

# Toggle add-on blocks here.
# School race controls are intentionally excluded from this script because
# hs_pct_bl_hisp_nat_decile is part of the core control architecture.
use_zip_race_controls    <- TRUE
use_zip_poverty_controls <- TRUE


### SPEC GRID -------------------------------------------------------------------
# Always run these 6 interaction specs by default:
#   2 popularity measures × 3 interaction variables
# Reference groups:
#   - FRL decile: D1
#   - ZIP income decile: D1
#   - Black/Brown/Native decile: D6

spec_grid <- tribble(
  ~spec_id,                 ~pop_rate_type, ~interaction_var,                    ~interaction_ref_level,
  "per_sch_frl",           "per_sch",     "hs_pct_free_reduced_lunch_decile", "D1",
  "per_g12k_frl",          "per_g12k",    "hs_pct_free_reduced_lunch_decile", "D1",
  "per_sch_zipinc",        "per_sch",     "hs_zip_inc_house_mean_decile",     "D1",
  "per_g12k_zipinc",       "per_g12k",    "hs_zip_inc_house_mean_decile",     "D1",
  "per_sch_blhispnat",     "per_sch",     "hs_pct_bl_hisp_nat_decile",        "D6",
  "per_g12k_blhispnat",    "per_g12k",    "hs_pct_bl_hisp_nat_decile",        "D6"
)



### HELPERS --------------------------------------------------------------------

mk_form <- function(rhs, fe = fe_rhs, y = outcome_var) {
  rhs_str <- paste(rhs, collapse = " + ")
  as.formula(paste0(y, " ~ ", rhs_str, " | ", fe))
}

safe_div <- function(num, den) {
  if_else(den > 0, num / den, NA_real_)
}

safe_div_1000 <- function(num, den) {
  if_else(den > 0, 1000 * num / den, NA_real_)
}

add_if_missing <- function(rhs, term) {
  if (term %in% rhs) rhs else c(rhs, term)
}

mk_peps_var <- function(visit_type, rate_type, school_type = "all") {
  paste0("peps_n_", visit_type, "_", rate_type, "_", school_type)
}

assemble_controls <- function(interaction_var) {
  rhs <- core_controls
  
  if (use_zip_race_controls) {
    rhs <- c(rhs, zip_race_controls)
  }
  if (use_zip_poverty_controls) {
    rhs <- c(rhs, zip_poverty_controls)
  }
  
  rhs <- unique(rhs)
  rhs <- setdiff(rhs, interaction_var)
  rhs
}

factorize_and_relevel <- function(df, var, ref_level) {
  stopifnot(var %in% names(df))
  
  if (is.character(df[[var]])) {
    df <- df %>%
      mutate(
        !!var := as.factor(.data[[var]])
      )
  }
  
  if (is.factor(df[[var]])) {
    if (!ref_level %in% levels(df[[var]])) {
      stop(paste0("Reference level '", ref_level, "' not found in ", var, "."))
    }
    
    df <- df %>%
      mutate(
        !!var := forcats::fct_relevel(.data[[var]], ref_level)
      )
  }
  
  df
}

build_popularity_var <- function(df, pop_visit_type, pop_rate_type, pop_school_type) {
  stopifnot(pop_school_type %in% c("all", "pub"))
  stopifnot(pop_visit_type %in% c("vis01", "vistot"))
  stopifnot(pop_rate_type %in% c("per_sch", "per_g12k"))
  
  eps_num_var <- paste0("eps_n_", pop_visit_type, "_", pop_school_type)
  
  if (pop_rate_type == "per_sch") {
    eps_den_var <- paste0("eps_n_sch_", pop_school_type)
  } else {
    eps_den_var <- paste0("eps_n_g12_", pop_school_type)
  }
  
  peps_var <- mk_peps_var(
    visit_type  = pop_visit_type,
    rate_type   = pop_rate_type,
    school_type = pop_school_type
  )
  
  stopifnot(eps_num_var %in% names(df))
  stopifnot(eps_den_var %in% names(df))
  
  df <- df %>%
    mutate(
      peps_num = case_when(
        pop_visit_type == "vis01"  ~ .data[[eps_num_var]] - as.integer(visit01),
        pop_visit_type == "vistot" ~ .data[[eps_num_var]] - as.integer(num_visits),
        TRUE ~ NA_real_
      ),
      peps_den = case_when(
        pop_rate_type == "per_sch"  ~ .data[[eps_den_var]] - 1,
        pop_rate_type == "per_g12k" ~ .data[[eps_den_var]] - hs_g12,
        TRUE ~ NA_real_
      ),
      !!peps_var := case_when(
        pop_rate_type == "per_sch"  ~ safe_div(peps_num, peps_den),
        pop_rate_type == "per_g12k" ~ safe_div_1000(peps_num, peps_den),
        TRUE ~ NA_real_
      )
    )
  
  list(
    data = df,
    peps_var = peps_var,
    eps_num_var = eps_num_var,
    eps_den_var = eps_den_var
  )
}

get_estimation_sample_meta <- function(data, formula, cluster_var, school_id_var = "hs_ncessch") {
  
  vars_needed <- unique(c(
    all.vars(formula),
    cluster_var,
    school_id_var
  ))
  
  df_tmp <- data %>%
    dplyr::select(dplyr::all_of(vars_needed))
  
  keep <- complete.cases(df_tmp)
  
  tibble(
    n_schools = dplyr::n_distinct(df_tmp[[school_id_var]][keep]),
    n_pairs   = sum(keep)
  )
}

get_fit_meta <- function(model) {
  tibble(
    n_parameters = model$nparams,
    resid_df     = fixest::degrees_freedom(model, "resid"),
    r2           = unname(fixest::fitstat(model, "r2")[[1]]),
    adj_r2       = unname(fixest::fitstat(model, "ar2")[[1]]),
    within_r2    = unname(fixest::fitstat(model, "wr2")[[1]]),
    rmse         = unname(fixest::fitstat(model, "rmse")[[1]])
  )
}

make_result_bundle <- function(
    baseline_model,
    interaction_model,
    baseline_form,
    interaction_form,
    spec_row,
    peps_var,
    rhs_controls,
    estimation_meta_baseline,
    estimation_meta_interaction,
    fit_meta_baseline,
    fit_meta_interaction
) {
  list(
    baseline_model = baseline_model,
    interaction_model = interaction_model,
    baseline_formula = baseline_form,
    interaction_formula = interaction_form,
    spec_meta = list(
      spec_id = spec_row$spec_id,
      run_label = run_label,
      outcome_var = outcome_var,
      pop_visit_type = pop_visit_type,
      pop_rate_type = spec_row$pop_rate_type,
      pop_school_type = pop_school_type,
      popularity_var = peps_var,
      interaction_var = spec_row$interaction_var,
      interaction_ref_level = spec_row$interaction_ref_level,
      market_subset = market_subset,
      fe_rhs = fe_rhs,
      cluster_var = cluster_var,
      controls_core = core_controls,
      controls_zip_race = if (use_zip_race_controls) zip_race_controls else character(0),
      controls_zip_poverty = if (use_zip_poverty_controls) zip_poverty_controls else character(0),
      rhs_controls_used = rhs_controls,
      toggles = list(
        use_zip_race_controls = use_zip_race_controls,
        use_zip_poverty_controls = use_zip_poverty_controls
      )
    ),
    sample_meta = list(
      baseline = estimation_meta_baseline,
      interaction = estimation_meta_interaction
    ),
    fit_meta = list(
      baseline = fit_meta_baseline,
      interaction = fit_meta_interaction
    )
  )
}


### STEP 1. MERGE EPS-LEVEL OBJECT ONTO PAIR-LEVEL FILE ------------------------

# Keep EPS variables needed for popularity construction.
# Pull n_* variables and prefix them with eps_.
eps_vars <- df_by_univ_eps %>%
  select(univ_id, hs_eps_codename, starts_with("n_")) %>%
  rename_with(~ paste0("eps_", .), starts_with("n_")) %>%
  mutate(merge_ok = 1L)

rq3_df_raw <- pubprivhs_univ_df %>%
  left_join(
    y  = eps_vars,
    by = c("univ_id", "hs_eps_codename")
  ) %>%
  filter(!is.na(merge_ok)) %>%
  select(-merge_ok)

rq3_df_raw %>% glimpse()


### STEP 2. BUILD MAIN PUBLIC-SCHOOL ANALYTIC SAMPLE ---------------------------

rq3_pub_df_base <- rq3_df_raw %>%
  filter(univ_id != "all") %>%
  filter(!is.na(hs_eps_codename)) %>%
  filter(hs_control == "public") %>%
  filter(hs_school_type == "regular school")
# filter(hs_g12 >= 100) # this exclusion would not be defensible

if (!is.null(market_subset)) {
  rq3_pub_df_base <- rq3_pub_df_base %>%
    filter(hs_univ_market %in% market_subset)
}

rq3_pub_df_base %>% glimpse()


### STEP 3. RUN ALL MODEL SPECS ------------------------------------------------

results_rq3 <- vector(mode = "list", length = nrow(spec_grid))
names(results_rq3) <- spec_grid$spec_id

for (i in seq_len(nrow(spec_grid))) {
  
  spec_row <- spec_grid[i, ]
  
  message("------------------------------------------------------------")
  message("Running spec: ", spec_row$spec_id)
  message("  popularity denominator: ", spec_row$pop_rate_type)
  message("  interaction variable:   ", spec_row$interaction_var)
  message("  reference level:        ", spec_row$interaction_ref_level)
  
  rq3_pub_df <- rq3_pub_df_base
  
  # Prepare interaction variable
  rq3_pub_df <- factorize_and_relevel(
    df        = rq3_pub_df,
    var       = spec_row$interaction_var,
    ref_level = spec_row$interaction_ref_level
  )
  
  # Build spec-specific popularity variable
  pop_out <- build_popularity_var(
    df              = rq3_pub_df,
    pop_visit_type  = pop_visit_type,
    pop_rate_type   = spec_row$pop_rate_type,
    pop_school_type = pop_school_type
  )
  
  rq3_pub_df <- pop_out$data
  peps_var   <- pop_out$peps_var
  
  # Assemble RHS controls and ensure no duplicate interaction variable
  rhs_controls <- assemble_controls(spec_row$interaction_var)
  
  # Baseline RHS: controls + interaction variable main effect + popularity main effect
  rhs_baseline <- rhs_controls %>%
    add_if_missing(spec_row$interaction_var) %>%
    add_if_missing(peps_var)
  
  # Interaction RHS: baseline RHS + popularity × interaction variable
  interaction_term <- paste0(peps_var, ":", spec_row$interaction_var)
  rhs_interact <- c(rhs_baseline, interaction_term)
  
  # Formulae
  form_baseline <- mk_form(rhs = rhs_baseline, fe = fe_rhs, y = outcome_var)
  form_interact <- mk_form(rhs = rhs_interact, fe = fe_rhs, y = outcome_var)
  
  # Fit models
  model_baseline <- feols(
    fml     = form_baseline,
    data    = rq3_pub_df,
    cluster = as.formula(paste0("~ ", cluster_var))
  )
  
  model_interact <- feols(
    fml     = form_interact,
    data    = rq3_pub_df,
    cluster = as.formula(paste0("~ ", cluster_var))
  )
  
  # Estimation-sample metadata
  estimation_meta_baseline <- get_estimation_sample_meta(
    data        = rq3_pub_df,
    formula     = form_baseline,
    cluster_var = cluster_var
  )
  
  estimation_meta_interaction <- get_estimation_sample_meta(
    data        = rq3_pub_df,
    formula     = form_interact,
    cluster_var = cluster_var
  )  
  # Fit metadata
  fit_meta_baseline <- get_fit_meta(model_baseline)
  fit_meta_interaction <- get_fit_meta(model_interact)
  
  # Save full results bundle for this spec
  results_rq3[[spec_row$spec_id]] <- make_result_bundle(
    baseline_model              = model_baseline,
    interaction_model           = model_interact,
    baseline_form               = form_baseline,
    interaction_form            = form_interact,
    spec_row                    = spec_row,
    peps_var                    = peps_var,
    rhs_controls                = rhs_controls,
    estimation_meta_baseline    = estimation_meta_baseline,
    estimation_meta_interaction = estimation_meta_interaction,
    fit_meta_baseline           = fit_meta_baseline,
    fit_meta_interaction        = fit_meta_interaction
  )
}


### STEP 4. ADD RUN-LEVEL METADATA ---------------------------------------------

results_rq3 <- list(
  run_meta = list(
    run_label = run_label,
    outcome_var = outcome_var,
    pop_visit_type = pop_visit_type,
    pop_school_type = pop_school_type,
    fe_rhs = fe_rhs,
    cluster_var = cluster_var,
    market_subset = market_subset,
    outfile_rds = outfile_rds,
    timestamp = Sys.time(),
    control_blocks = list(
      core_controls = core_controls,
      zip_race_controls = zip_race_controls,
      zip_poverty_controls = zip_poverty_controls
    ),
    toggles = list(
      use_zip_race_controls = use_zip_race_controls,
      use_zip_poverty_controls = use_zip_poverty_controls
    ),
    spec_grid = spec_grid
  ),
  models = results_rq3
)


### STEP 5. SAVE MASTER RDS ----------------------------------------------------

if (save_outputs) {
  saveRDS(results_rq3, outfile_rds)
  message("Saved master RDS to: ", outfile_rds)
}


### STEP 6. QUICK DIAGNOSTIC LOOKS ---------------------------------------------

names(results_rq3$models)
results_rq3$run_meta$spec_grid

# Example pulls:
results_rq3$models$per_sch_frl$baseline_model %>% summary()
results_rq3$models$per_sch_frl$interaction_model %>% summary()

results_rq3$models$per_sch_frl$spec_meta
results_rq3$models$per_sch_frl$sample_meta
results_rq3$models$per_sch_frl$fit_meta
results_rq3$models$per_sch_frl$interaction_formula

results_rq3$models$per_sch_frl$baseline_model %>% summary()
results_rq3$models$per_sch_frl$interaction_model %>% summary()



# Example summary check:
# summary(results_rq3$models$per_sch_frl$interaction_model)
