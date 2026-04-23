################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits >
## [ FILE ] < rq3_lpm_v5_multirun.R >
## [ AUTH ] < Ozan Jaquette + ChatGPT draft >
## [ INIT ] < 2026-04-01 >
## [ DESC ] < RQ3 interaction models with a run grid over sample definitions.
##            Saves one standard results_rq3 object per output file so
##            downstream tables/plots/slides can still use:
##            results_rq3$models[[spec_id]] >
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

rm(list = intersect(c("create_rq1_map", "format_vars", "get_palette"), ls()))

getwd()
source(file = file.path("scripts", "create_univ_geo_df.R"))
getwd()

pubprivhs_univ_df %>% glimpse()
df_by_univ_eps %>% glimpse()



### USER-SET PARAMETERS ---------------------------------------------------------

# Outcome: fixed here to LPM any-visit model.
outcome_var <- "visit01"

# -------------------------------------------------
# RUN GRID
# -------------------------------------------------
# Each row defines one analytic sample and one output file.
# This lets the script save:
#   (1) the legacy all-university RQ3 results
#   (2) public-university-only RQ3 results
#   (3) optional public-university in-state / out-of-state results
#
# hs_market_subset is a list-column because some rows use NULL and
# others use a character vector.

run_grid <- tibble::tibble(
  run_id = c(
    "legacy_allhs",
    "pubuniv_allhs",
    "pubuniv_instate",
    "pubuniv_oos"
  ),
  univ_control_subset = c(
    "all",
    "public",
    "public",
    "public"
  ),
  hs_market_subset = list(
    NULL,
    NULL,
    c("local", "in_state"),
    c("regional", "national")
  ),
  sample_label = c(
    "all universities + public HS + all HS",
    "public universities + public HS + all HS",
    "public universities + public HS + in-state HS",
    "public universities + public HS + out-of-state HS"
  ),
  outfile_rds = c(
    "results/rq3_pub_interaction_models_master.RDS",
    "results/rq3_pubuniv_allhs_interaction_models.RDS",
    "results/rq3_pubuniv_instate_interaction_models.RDS",
    "results/rq3_pubuniv_oos_interaction_models.RDS"
  )
)

# -------------------------------------------------
# POPULARITY CONSTRUCTION
# -------------------------------------------------

# Which EPS sector to use when constructing popularity variables.
# Supported here: "all" or "pub"
pop_school_type <- "pub"

# Popularity numerator. Supported here: "vis01" or "vistot"
pop_visit_type <- "vistot"

# -------------------------------------------------
# MODEL SETTINGS
# -------------------------------------------------

fe_rhs <- "univ_id^hs_state_code"
cluster_var <- c("hs_state_code", "univ_id")

# Save output?
save_outputs <- TRUE

# Optional run label for bookkeeping
run_label <- "rq3 interaction models (multi-run sample version)"


### CONTROL ARCHITECTURE --------------------------------------------------------

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

use_zip_race_controls    <- FALSE
use_zip_poverty_controls <- FALSE


### SPEC GRID -------------------------------------------------------------------

spec_grid <- tribble(
  ~spec_id,              ~pop_rate_type, ~interaction_var,                    ~interaction_ref_level,
  "per_sch_frl",         "per_sch",      "hs_pct_free_reduced_lunch_decile", "D1",
  "per_g12k_frl",        "per_g12k",     "hs_pct_free_reduced_lunch_decile", "D1",
  "per_sch_zipinc",      "per_sch",      "hs_zip_inc_house_mean_decile",     "D1",
  "per_g12k_zipinc",     "per_g12k",     "hs_zip_inc_house_mean_decile",     "D1",
  "per_sch_blhispnat",   "per_sch",      "hs_pct_bl_hisp_nat_decile",        "D5",
  "per_g12k_blhispnat",  "per_g12k",     "hs_pct_bl_hisp_nat_decile",        "D5"
)



### HELPERS --------------------------------------------------------------------

mk_form <- function(rhs, fe = fe_rhs, y = outcome_var) {
  rhs_str <- paste(rhs, collapse = " + ")
  as.formula(paste0(y, " ~ ", rhs_str, " | ", fe))
}

mk_cluster_formula <- function(cluster_vars) {
  as.formula(paste0("~ ", paste(cluster_vars, collapse = " + ")))
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
    fit_meta_interaction,
    sample_label,
    univ_control_subset,
    hs_market_subset
) {
  list(
    baseline_model = baseline_model,
    interaction_model = interaction_model,
    baseline_formula = baseline_form,
    interaction_formula = interaction_form,
    spec_meta = list(
      spec_id = spec_row$spec_id,
      run_label = run_label,
      sample_label = sample_label,
      outcome_var = outcome_var,
      pop_visit_type = pop_visit_type,
      pop_rate_type = spec_row$pop_rate_type,
      pop_school_type = pop_school_type,
      popularity_var = peps_var,
      interaction_var = spec_row$interaction_var,
      interaction_ref_level = spec_row$interaction_ref_level,
      univ_control_subset = univ_control_subset,
      hs_market_subset = hs_market_subset,
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

apply_university_subset <- function(df, univ_control_subset) {
  stopifnot(univ_control_subset %in% c("all", "public", "private"))
  
  out <- df %>%
    filter(univ_id != "all")
  
  if (univ_control_subset == "public") {
    out <- out %>% filter(univ_classification == "public_research")
  }
  
  if (univ_control_subset == "private") {
    out <- out %>% filter(univ_classification %in% c("private_libarts", "private_national"))
  }
  
  out
}

build_rq3_sample <- function(df_raw,
                             univ_control_subset = "all",
                             hs_market_subset = NULL) {
  
  df <- df_raw %>%
    filter(!is.na(hs_eps_codename)) %>%
    filter(hs_control == "public") %>%
    filter(hs_school_type == "regular school")
  
  df <- apply_university_subset(
    df = df,
    univ_control_subset = univ_control_subset
  )
  
  if (!is.null(hs_market_subset)) {
    df <- df %>%
      filter(hs_univ_market %in% hs_market_subset)
  }
  
  df
}

make_sample_note <- function(univ_control_subset, hs_market_subset) {
  market_text <- if (is.null(hs_market_subset)) {
    "all HS"
  } else {
    paste(hs_market_subset, collapse = ", ")
  }
  
  paste0(
    "universities = ", univ_control_subset,
    "; HS market subset = ", market_text
  )
}

run_rq3_sample <- function(df_raw,
                           univ_control_subset,
                           hs_market_subset,
                           sample_label,
                           outfile_rds) {
  
  ### STEP 2. BUILD ANALYTIC SAMPLE --------------------------------------------
  
  rq3_analysis_df <- build_rq3_sample(
    df_raw              = df_raw,
    univ_control_subset = univ_control_subset,
    hs_market_subset    = hs_market_subset
  )
  
  rq3_analysis_df %>% glimpse()
  message("Sample n before modeling: ", nrow(rq3_analysis_df))
  message("------------------------------------------------------------")
  message("Analytic sample built")
  message("  ", make_sample_note(univ_control_subset, hs_market_subset))
  message("  sample_label = ", sample_label)
  message("  outfile_rds  = ", outfile_rds)
  message("------------------------------------------------------------")
  
  ### STEP 3. RUN ALL MODEL SPECS ----------------------------------------------
  
  results_rq3_models <- vector(mode = "list", length = nrow(spec_grid))
  names(results_rq3_models) <- spec_grid$spec_id
  
  for (i in seq_len(nrow(spec_grid))) {
    
    spec_row <- spec_grid[i, ]
    
    message("------------------------------------------------------------")
    message("Running spec: ", spec_row$spec_id)
    message("  popularity denominator: ", spec_row$pop_rate_type)
    message("  interaction variable:   ", spec_row$interaction_var)
    message("  reference level:        ", spec_row$interaction_ref_level)
    
    rq3_df <- rq3_analysis_df
    
    rq3_df <- factorize_and_relevel(
      df        = rq3_df,
      var       = spec_row$interaction_var,
      ref_level = spec_row$interaction_ref_level
    )
    
    pop_out <- build_popularity_var(
      df              = rq3_df,
      pop_visit_type  = pop_visit_type,
      pop_rate_type   = spec_row$pop_rate_type,
      pop_school_type = pop_school_type
    )
    
    rq3_df   <- pop_out$data
    peps_var <- pop_out$peps_var
    
    rhs_controls <- assemble_controls(spec_row$interaction_var)
    
    rhs_baseline <- rhs_controls %>%
      add_if_missing(spec_row$interaction_var) %>%
      add_if_missing(peps_var)
    
    interaction_term <- paste0(peps_var, ":", spec_row$interaction_var)
    rhs_interact <- c(rhs_baseline, interaction_term)
    
    form_baseline <- mk_form(rhs = rhs_baseline, fe = fe_rhs, y = outcome_var)
    form_interact <- mk_form(rhs = rhs_interact, fe = fe_rhs, y = outcome_var)
    
    cluster_fml <- mk_cluster_formula(cluster_var)
    
    model_baseline <- feols(
      fml     = form_baseline,
      data    = rq3_df,
      cluster = cluster_fml
    )
    
    model_interact <- feols(
      fml     = form_interact,
      data    = rq3_df,
      cluster = cluster_fml
    )
    
    estimation_meta_baseline <- get_estimation_sample_meta(
      data        = rq3_df,
      formula     = form_baseline,
      cluster_var = cluster_var
    )
    
    estimation_meta_interaction <- get_estimation_sample_meta(
      data        = rq3_df,
      formula     = form_interact,
      cluster_var = cluster_var
    )
    
    fit_meta_baseline <- get_fit_meta(model_baseline)
    fit_meta_interaction <- get_fit_meta(model_interact)
    
    results_rq3_models[[spec_row$spec_id]] <- make_result_bundle(
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
      fit_meta_interaction        = fit_meta_interaction,
      sample_label                = sample_label,
      univ_control_subset         = univ_control_subset,
      hs_market_subset            = hs_market_subset
    )
  }
  
  ### STEP 4. ADD RUN-LEVEL METADATA -------------------------------------------
  
  decile_cutoffs <- list(
    hs_pct_free_reduced_lunch   = quantile(rq3_analysis_df$hs_pct_free_reduced_lunch,
                                           probs = seq(0, 1, 0.1), na.rm = TRUE),
    hs_zip_inc_house_mean       = quantile(rq3_analysis_df$hs_zip_inc_house_mean,
                                           probs = seq(0, 1, 0.1), na.rm = TRUE),
    hs_pct_bl_hisp_nat          = quantile(rq3_analysis_df$hs_pct_bl_hisp_nat,
                                           probs = seq(0, 1, 0.1), na.rm = TRUE)
  )

  results_rq3 <- list(
    run_meta = list(
      run_label = run_label,
      sample_label = sample_label,
      outcome_var = outcome_var,
      pop_visit_type = pop_visit_type,
      pop_school_type = pop_school_type,
      univ_control_subset = univ_control_subset,
      hs_market_subset = hs_market_subset,
      fe_rhs = fe_rhs,
      cluster_var = cluster_var,
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
      spec_grid = spec_grid,
      sample_note = make_sample_note(univ_control_subset, hs_market_subset),
      decile_cutoffs = decile_cutoffs
    ),
    models = results_rq3_models
  )
  
  ### STEP 5. SAVE MASTER RDS --------------------------------------------------
  
  if (save_outputs) {
    saveRDS(results_rq3, outfile_rds)
    message("Saved master RDS to: ", outfile_rds)
  }
  
  results_rq3
}



### STEP 1. MERGE EPS-LEVEL OBJECT ONTO PAIR-LEVEL FILE ------------------------

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


### RUN ALL REQUESTED SAMPLE DEFINITIONS ---------------------------------------

all_run_results <- vector("list", length = nrow(run_grid))
names(all_run_results) <- run_grid$run_id

for (r in seq_len(nrow(run_grid))) {
  
  run_id_this <- run_grid$run_id[r]
  univ_subset_this <- run_grid$univ_control_subset[r]
  hs_market_subset_this <- run_grid$hs_market_subset[[r]]
  sample_label_this <- run_grid$sample_label[r]
  outfile_rds_this <- run_grid$outfile_rds[r]
  
  message("============================================================")
  message("RUNNING SAMPLE: ", run_id_this)
  message("============================================================")
  
  results_rq3 <- run_rq3_sample(
    df_raw              = rq3_df_raw,
    univ_control_subset = univ_subset_this,
    hs_market_subset    = hs_market_subset_this,
    sample_label        = sample_label_this,
    outfile_rds         = outfile_rds_this
  )
  
  all_run_results[[run_id_this]] <- results_rq3
}

### STEP 6. QUICK DIAGNOSTIC LOOKS ---------------------------------------------
# Set this to whichever run you want to inspect interactively.

results_rq3 <- all_run_results[["legacy_allhs"]]

results_rq3$run_meta$sample_label
results_rq3$run_meta$univ_control_subset
results_rq3$run_meta$hs_market_subset

names(results_rq3$models)
names(results_rq3$models$per_sch_frl)

results_rq3$run_meta$spec_grid

results_rq3$models$per_sch_frl$baseline_model %>% summary()
results_rq3$models$per_sch_frl$interaction_model %>% summary()

results_rq3$models$per_sch_frl$spec_meta
results_rq3$models$per_sch_frl$sample_meta
results_rq3$models$per_sch_frl$fit_meta
results_rq3$models$per_sch_frl$interaction_formula

results_rq3$models$per_sch_frl$baseline_model %>% summary()
results_rq3$models$per_sch_frl$interaction_model %>% summary()

#################
################# CREATE SMALL MULTIPLES OF MARGINAL EFFECTS
#################

# =========================================================
# Helpers
# =========================================================

parse_ref_decile <- function(ref_level) {
  as.integer(gsub("^D", "", ref_level))
}

find_interaction_name <- function(coef_names, focal_var, moderator_var, decile) {
  cand1 <- paste0(moderator_var, "D", decile, ":", focal_var)
  cand2 <- paste0(focal_var, ":", moderator_var, "D", decile)
  
  if (cand1 %in% coef_names) return(cand1)
  if (cand2 %in% coef_names) return(cand2)
  
  stop("Could not find interaction coefficient for decile ", decile)
}

# =========================================================
# Core extractor: one spec -> plot-ready dataframe
# =========================================================

get_marginal_effect_df <- function(model_bundle, conf_level = 0.95) {
  
  mod  <- model_bundle$interaction_model
  meta <- model_bundle$spec_meta
  
  b <- coef(mod)
  V <- vcov(mod)
  
  focal_var     <- meta$popularity_var
  moderator_var <- meta$interaction_var
  ref_decile    <- parse_ref_decile(meta$interaction_ref_level)
  
  zcrit <- qnorm(1 - (1 - conf_level) / 2)
  
  out <- data.frame(
    decile   = 1:10,
    estimate = NA_real_,
    se       = NA_real_
  )
  
  # reference decile
  out$estimate[out$decile == ref_decile] <- b[focal_var]
  out$se[out$decile == ref_decile] <- sqrt(V[focal_var, focal_var])
  
  # all other deciles
  for (d in setdiff(1:10, ref_decile)) {
    
    int_name <- find_interaction_name(
      coef_names    = names(b),
      focal_var     = focal_var,
      moderator_var = moderator_var,
      decile        = d
    )
    
    out$estimate[out$decile == d] <- b[focal_var] + b[int_name]
    
    out$se[out$decile == d] <- sqrt(
      V[focal_var, focal_var] +
        V[int_name, int_name] +
        2 * V[focal_var, int_name]
    )
  }
  
  out$conf.low  <- out$estimate - zcrit * out$se
  out$conf.high <- out$estimate + zcrit * out$se
  
  out$spec_id         <- meta$spec_id
  out$popularity_var  <- meta$popularity_var
  out$interaction_var <- meta$interaction_var
  out$interaction_ref <- meta$interaction_ref_level
  out$pop_rate_type   <- meta$pop_rate_type
  
  out
}

# =========================================================
# Labels for one spec
# =========================================================

get_spec_labels <- function(model_bundle) {
  
  meta <- model_bundle$spec_meta
  
  title_text <- dplyr::case_when(
    meta$pop_rate_type == "per_sch"  ~ "Popularity: Visits per school",
    meta$pop_rate_type == "per_g12k" ~ "Popularity: Visits per 1,000 12th graders",
    TRUE ~ meta$pop_rate_type
  )
  
  moderator_text <- dplyr::case_when(
    meta$interaction_var == "hs_pct_free_reduced_lunch_decile" ~
      "Moderator: School % free/reduced lunch decile",
    meta$interaction_var == "hs_zip_inc_house_mean_decile" ~
      "Moderator: ZIP mean household income decile",
    meta$interaction_var == "hs_pct_bl_hisp_nat_decile" ~
      "Moderator: School % Black/Hispanic/Native decile",
    TRUE ~ "Moderator: Decile"
  )
  
  list(
    title    = title_text,
    subtitle = moderator_text,
    x_lab    = sub("^Moderator: ", "", moderator_text),
    y_lab    = "Effect on Pr(visit)"
  )
}

# =========================================================
# Stack multiple specs
# =========================================================

build_rq3_plot_df <- function(results_rq3, spec_ids, conf_level = 0.95) {
  purrr::map_dfr(
    spec_ids,
    ~ get_marginal_effect_df(
      model_bundle = results_rq3$models[[.x]],
      conf_level   = conf_level
    )
  )
}

# =========================================================
# Y-axis limits for a set of panels
# =========================================================

get_y_limits <- function(plot_df, pad_fraction = 0.06) {
  
  ymin <- min(plot_df$conf.low,  na.rm = TRUE)
  ymax <- max(plot_df$conf.high, na.rm = TRUE)
  
  yrange <- ymax - ymin
  
  if (yrange == 0) {
    yrange <- max(abs(ymin), abs(ymax), 0.01)
  }
  
  pad <- yrange * pad_fraction
  
  c(ymin - pad, ymax + pad)
}

# =========================================================
# Single-plot function
# =========================================================

plot_marginal_effects_single <- function(model_bundle,
                                         conf_level = 0.95,
                                         y_limits = NULL) {
  
  plot_df <- get_marginal_effect_df(
    model_bundle = model_bundle,
    conf_level   = conf_level
  )
  
  labs_list <- get_spec_labels(model_bundle)
  
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = decile, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      width = 0.18,
      linewidth = 0.45
    ) +
    ggplot2::scale_x_continuous(breaks = 1:10) +
    ggplot2::labs(
      title    = labs_list$title,
      subtitle = labs_list$subtitle,
      x        = labs_list$x_lab,
      y        = labs_list$y_lab
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 12, face = "plain",
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 12, face = "plain",
        margin = ggplot2::margin(t = 4, b = 6)
      ),
      axis.title.x = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 8, r = 12, b = 8, l = 12)
    )
  
  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits)
  }
  
  p
}

# =========================================================
# Generic row-wise grid plot
# Each row shares a y-axis scale; rows can differ
# =========================================================

make_rq3_rowwise_grid_plot <- function(results_rq3,
                                       spec_ids,
                                       ncol = 2,
                                       conf_level = 0.95) {
  
  if (length(spec_ids) %% ncol != 0) {
    stop("length(spec_ids) must be divisible by ncol.")
  }
  
  nrow_grid <- length(spec_ids) / ncol
  plot_list <- vector("list", length(spec_ids))
  
  for (r in seq_len(nrow_grid)) {
    
    idx <- ((r - 1) * ncol + 1):(r * ncol)
    row_specs <- spec_ids[idx]
    
    row_df <- build_rq3_plot_df(
      results_rq3 = results_rq3,
      spec_ids    = row_specs,
      conf_level  = conf_level
    )
    
    row_y_limits <- get_y_limits(row_df)
    
    for (i in seq_along(idx)) {
      plot_list[[idx[i]]] <- plot_marginal_effects_single(
        model_bundle = results_rq3$models[[row_specs[i]]],
        conf_level   = conf_level,
        y_limits     = row_y_limits
      )
    }
  }
  
  patchwork::wrap_plots(
    plotlist = plot_list,
    ncol = ncol,
    byrow = TRUE
  ) +
    patchwork::plot_layout(guides = "collect")
}

# =========================================================
# 2 x 2 combined plot
# Top row: per school
# Bottom row: per 1,000 12th graders
# =========================================================

make_rq3_2x2_plot <- function(results_rq3,
                              spec_ids,
                              conf_level = 0.95) {
  
  if (length(spec_ids) != 4) {
    stop("spec_ids must have length 4 for a 2x2 plot.")
  }
  
  make_rq3_rowwise_grid_plot(
    results_rq3 = results_rq3,
    spec_ids    = spec_ids,
    ncol        = 2,
    conf_level  = conf_level
  )
}

# =========================================================
# 2 x 3 combined plot
# Top row: per school
# Bottom row: per 1,000 12th graders
# Columns: FRL, ZIP mean household income, % Black/Hispanic/Native
# =========================================================

make_rq3_2x3_plot <- function(results_rq3,
                              spec_ids,
                              conf_level = 0.95) {
  
  if (length(spec_ids) != 6) {
    stop("spec_ids must have length 6 for a 2x3 plot.")
  }
  
  make_rq3_rowwise_grid_plot(
    results_rq3 = results_rq3,
    spec_ids    = spec_ids,
    ncol        = 3,
    conf_level  = conf_level
  )
}

# =========================================================
# Save helper
# Saves both PDF and PNG for .qmd use
# =========================================================

save_rq3_plot <- function(plot_obj,
                          file_stem,
                          width = 12,
                          height = 8.5,
                          dpi = 300) {
  
  pdf_path <- paste0("results/", file_stem, ".pdf")
  png_path <- paste0("results/", file_stem, ".png")
  
  ggplot2::ggsave(
    filename = pdf_path,
    plot     = plot_obj,
    width    = width,
    height   = height,
    bg       = "white"
  )
  
  ggplot2::ggsave(
    filename = png_path,
    plot     = plot_obj,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  
  invisible(list(
    pdf = pdf_path,
    png = png_path
  ))
}

# =========================================================
# Build plots
# =========================================================

# 2 x 2
# Top row: per school
# Bottom row: per 1,000 12th graders
spec_ids_2x2 <- c(
  "per_sch_frl",        # top left
  "per_sch_blhispnat",  # top right
  "per_g12k_frl",       # bottom left
  "per_g12k_blhispnat"  # bottom right
)

p_rq3_2x2 <- make_rq3_2x2_plot(
  results_rq3 = results_rq3,
  spec_ids    = spec_ids_2x2,
  conf_level  = 0.95
)

# 2 x 3
# Top row = per school
# Bottom row = per 1,000 12th graders
# Col 1 = FRL
# Col 2 = ZIP mean household income
# Col 3 = % Black/Hispanic/Native
spec_ids_2x3 <- c(
  "per_sch_frl",        "per_sch_zipinc",      "per_sch_blhispnat",
  "per_g12k_frl",       "per_g12k_zipinc",     "per_g12k_blhispnat"
)

p_rq3_2x3 <- make_rq3_2x3_plot(
  results_rq3 = results_rq3,
  spec_ids    = spec_ids_2x3,
  conf_level  = 0.95
)

# Print to viewer if desired
p_rq3_2x2
p_rq3_2x3

# =========================================================
# Save plots for .qmd input
# Use the PNG files in your .qmd
# =========================================================

paths_rq3_2x2 <- save_rq3_plot(
  plot_obj   = p_rq3_2x2,
  file_stem  = "rq3_marginal_effects_2x2",
  width      = 12,
  height     = 8.5
)

paths_rq3_2x3 <- save_rq3_plot(
  plot_obj   = p_rq3_2x3,
  file_stem  = "rq3_marginal_effects_2x3",
  width      = 15,
  height     = 8.5
)

paths_rq3_2x2
paths_rq3_2x3

# =========================================================
# Example .qmd chunk usage
# =========================================================
#
# ```{r fig-rq3-me-2x2, echo=FALSE, out.width="100%"}
# knitr::include_graphics("results/rq3_marginal_effects_2x2.png")
# ```
#
# ```{r fig-rq3-me-2x3, echo=FALSE, out.width="100%"}
# knitr::include_graphics("results/rq3_marginal_effects_2x3.png")
# ```




#################
################# CREATE 3 x 3 SMALL MULTIPLES FOR PUBLIC UNIVERSITIES
#################

#################
################# CREATE 3 x 3 SMALL MULTIPLES FOR PUBLIC UNIVERSITIES
#################

# =========================================================
# Helpers: labels for public-university sample comparison plots
# =========================================================

get_pubu_sample_label <- function(sample_id) {
  dplyr::case_when(
    sample_id == "allhs"   ~ "All high schools",
    sample_id == "instate" ~ "In-state high schools",
    sample_id == "oos"     ~ "Out-of-state high schools",
    TRUE ~ sample_id
  )
}

get_pubu_moderator_label <- function(interaction_var) {
  dplyr::case_when(
    interaction_var == "hs_pct_free_reduced_lunch_decile" ~
      "School FRL",
    interaction_var == "hs_zip_inc_house_mean_decile" ~
      "ZIP income",
    interaction_var == "hs_pct_bl_hisp_nat_decile" ~
      "School % Black/Hisp/Native",
    TRUE ~ interaction_var
  )
}
get_pubu_spec_id <- function(moderator, pop_rate_type = "per_sch") {
  stopifnot(pop_rate_type %in% c("per_sch", "per_g12k"))
  stopifnot(moderator %in% c("frl", "zipinc", "blhispnat"))
  paste0(pop_rate_type, "_", moderator)
}

# =========================================================
# Read one spec from one file, then immediately release memory
# =========================================================

get_spec_from_file <- function(rds_path, spec_id) {
  obj <- readRDS(rds_path)
  out <- obj$models[[spec_id]]
  rm(obj)
  invisible(gc())
  out
}

# =========================================================
# Build one long dataframe by reading one spec at a time
# =========================================================

build_pubu_3x3_plot_df <- function(pop_rate_type = "per_sch",
                                   conf_level = 0.95) {
  
  stopifnot(pop_rate_type %in% c("per_sch", "per_g12k"))
  
  moderator_keys <- c("frl", "zipinc", "blhispnat")
  
  file_map <- list(
    allhs   = "results/rq3_pubuniv_allhs_interaction_models.RDS",
    instate = "results/rq3_pubuniv_instate_interaction_models.RDS",
    oos     = "results/rq3_pubuniv_oos_interaction_models.RDS"
  )
  
  out_list <- list()
  counter <- 1L
  
  for (sample_id in names(file_map)) {
    
    for (mod_key in moderator_keys) {
      
      spec_id <- get_pubu_spec_id(
        moderator     = mod_key,
        pop_rate_type = pop_rate_type
      )
      
      model_bundle <- get_spec_from_file(
        rds_path = file_map[[sample_id]],
        spec_id  = spec_id
      )
      
      plot_df <- get_marginal_effect_df(
        model_bundle = model_bundle,
        conf_level   = conf_level
      )
      
      out_list[[counter]] <- plot_df %>%
        dplyr::mutate(
          sample_id     = sample_id,
          sample_label  = get_pubu_sample_label(sample_id),
          moderator_key = mod_key,
          moderator_lab = get_pubu_moderator_label(unique(interaction_var)),
          pop_rate_type = pop_rate_type
        )
      
      counter <- counter + 1L
      
      rm(model_bundle, plot_df)
      invisible(gc())
    }
  }
  
  dplyr::bind_rows(out_list) %>%
    dplyr::mutate(
      sample_label = factor(
        sample_label,
        levels = c(
          "All high schools",
          "In-state high schools",
          "Out-of-state high schools"
        )
      ),
      moderator_lab = factor(
        moderator_lab,
        levels = c(
          "School FRL",
          "ZIP income",
          "School % Black/Hisp/Native"
        )
      )
    )
}

# =========================================================
# Single global y-axis limits across all 9 panels
# =========================================================

get_global_y_limits <- function(plot_df, pad_fraction = 0.06) {
  
  ymin <- min(plot_df$conf.low,  na.rm = TRUE)
  ymax <- max(plot_df$conf.high, na.rm = TRUE)
  
  yrange <- ymax - ymin
  
  if (yrange == 0) {
    yrange <- max(abs(ymin), abs(ymax), 0.01)
  }
  
  pad <- yrange * pad_fraction
  
  c(ymin - pad, ymax + pad)
}

# =========================================================
# 3 x 3 public-university comparison plot
# =========================================================

rm(list = intersect(
  c(
    "all_run_results",
    "results_rq3",
    "mod_all",
    "mod_oos",
    "p_rq3_2x2",
    "p_rq3_2x3",
    "p_rq3_pubu_3x3_per_sch",
    "paths_rq3_2x2",
    "paths_rq3_2x3",
    "paths_rq3_pubu_3x3_per_sch"
  ),
  ls()
))
invisible(gc())

make_pubu_3x3_plot <- function(pop_rate_type = "per_sch",
                               conf_level = 0.95,
                               y_limits = NULL) {
  
  plot_df <- build_pubu_3x3_plot_df(
    pop_rate_type = pop_rate_type,
    conf_level    = conf_level
  )
  
  if (is.null(y_limits)) {
    y_limits <- get_global_y_limits(plot_df)
  }
  
  y_lab_text <- dplyr::case_when(
    pop_rate_type == "per_sch"  ~ "Marginal effect on Pr(visit)",
    pop_rate_type == "per_g12k" ~ "Marginal effect on Pr(visit)",
    TRUE ~ "Marginal effect on Pr(visit)"
  )
  
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = decile, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      width = 0.18,
      linewidth = 0.45
    ) +
    ggplot2::scale_x_continuous(breaks = 1:10) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(moderator_lab),
      cols = ggplot2::vars(sample_label),
      switch = "y"
    ) +
    ggplot2::labs(
      x = "Decile",
      y = y_lab_text
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 11, face = "plain"),
      strip.text.y = ggplot2::element_text(size = 11, face = "plain", angle = 0),
      axis.title.x = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.0, "lines"),
      plot.margin = ggplot2::margin(t = 8, r = 12, b = 8, l = 12)
    )
  
  rm(plot_df)
  invisible(gc())
  
  p
}

# =========================================================
# Build plots
# Default: visits per school
# To switch later, change pop_rate_type = "per_g12k"
# =========================================================

p_rq3_pubu_3x3_per_sch <- make_pubu_3x3_plot(
  pop_rate_type = "per_sch",
  conf_level    = 0.95
)

# Optional future version:
# p_rq3_pubu_3x3_per_g12k <- make_pubu_3x3_plot(
#   pop_rate_type = "per_g12k",
#   conf_level    = 0.95
# )

# Print to viewer if desired
p_rq3_pubu_3x3_per_sch

# =========================================================
# Save plot for .qmd input
# =========================================================

paths_rq3_pubu_3x3_per_sch <- save_rq3_plot(
  plot_obj   = p_rq3_pubu_3x3_per_sch,
  file_stem  = "rq3_pubu_marginal_effects_3x3_per_sch",
  width      = 13.5,
  height     = 10.5
)

paths_rq3_pubu_3x3_per_sch

# Optional future save:
# paths_rq3_pubu_3x3_per_g12k <- save_rq3_plot(
#   plot_obj   = p_rq3_pubu_3x3_per_g12k,
#   file_stem  = "rq3_pubu_marginal_effects_3x3_per_g12k",
#   width      = 13.5,
#   height     = 10.5
# )

# Clean up plot object if desired
invisible(gc())