# ============================================================
#  Lorenz/Concentration & Over-Index — parameterized by school_var
#  (visit_var is auto-derived from school_var)
#  Assumes tidyverse + ggplot2 loaded.
# ============================================================

# ---- Internal helper: derive the paired visit var ---------------------------
derive_visit_var <- function(school_var, df_cols = NULL) {
  if (!grepl("^n_sch", school_var)) {
    stop(sprintf("school_var must start with 'n_sch', got: '%s'", school_var))
  }
  visit_var <- sub("^n_sch", "n_vistot", school_var)
  
  if (!is.null(df_cols) && !(visit_var %in% df_cols)) {
    candidates <- grep("^n_vistot", df_cols, value = TRUE)
    stop(sprintf(
      "Could not find paired visit var '%s' for school_var '%s'.\nAvailable visit vars include:\n  %s",
      visit_var, school_var, paste(candidates, collapse = ", ")
    ))
  }
  visit_var
}

# ---- Scope builder: choose the column used for 'schools' --------------------
# visit_var is auto-mapped (n_sch* -> n_vistot*)
build_scope <- function(df,
                        scope = c("all", "group", "one"),
                        school_var = "n_sch_all",
                        group_vals = NULL,
                        univ_ids   = NULL) {
  scope <- match.arg(scope)
  visit_var <- derive_visit_var(school_var, df_cols = names(df))
  
  if (scope == "all") {
    out <- df %>%
      dplyr::filter(.data$univ_id == "all") %>%
      dplyr::transmute(
        hs_eps_codename,
        schools = .data[[school_var]],
        visits  = .data[[visit_var]],
        mean_inc_house, pct_edu_baplus_all, pct_pov_yes,
        pct_nhisp_white, pct_nhisp_asian, pct_nhisp_black, pct_hisp_all
      )
  } else if (scope == "group") {
    stopifnot(length(group_vals) >= 1)
    out <- df %>%
      dplyr::filter(.data$univ_id != "all",
                    .data$univ_classification %in% group_vals) %>%
      dplyr::group_by(hs_eps_codename) %>%
      dplyr::summarise(
        schools = dplyr::first(.data[[school_var]]),
        visits  = sum(.data[[visit_var]], na.rm = TRUE),
        mean_inc_house        = dplyr::first(mean_inc_house),
        pct_edu_baplus_all    = dplyr::first(pct_edu_baplus_all),
        pct_pov_yes           = dplyr::first(pct_pov_yes),
        pct_nhisp_white       = dplyr::first(pct_nhisp_white),
        pct_nhisp_asian       = dplyr::first(pct_nhisp_asian),
        pct_nhisp_black       = dplyr::first(pct_nhisp_black),
        pct_hisp_all          = dplyr::first(pct_hisp_all),
        .groups = "drop"
      )
  } else { # "one"
    stopifnot(length(univ_ids) == 1)
    out <- df %>%
      dplyr::filter(.data$univ_id == univ_ids) %>%
      dplyr::transmute(
        hs_eps_codename,
        schools = .data[[school_var]],
        visits  = .data[[visit_var]],
        mean_inc_house, pct_edu_baplus_all, pct_pov_yes,
        pct_nhisp_white, pct_nhisp_asian, pct_nhisp_black, pct_hisp_all
      )
  }
  
  out <- out %>%
    dplyr::mutate(
      schools = as.numeric(schools),
      visits  = as.numeric(visits)
    ) %>%
    dplyr::filter(!is.na(schools), schools > 0)
  
  attr(out, "school_var") <- school_var
  attr(out, "visit_var")  <- visit_var
  out
}

# ---- Decile over-index by affluence ----------------------------------------
ses_overindex <- function(scope_df, affluence_var = "mean_inc_house", ntiles = 10) {
  stopifnot(affluence_var %in% names(scope_df))
  oi_tbl <- scope_df %>%
    dplyr::mutate(
      aff = .data[[affluence_var]],
      dec = dplyr::ntile(aff, ntiles)
    ) %>%
    dplyr::group_by(dec) %>%
    dplyr::summarise(
      visits  = sum(visits,  na.rm = TRUE),
      schools = sum(schools, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      share_visits  = visits / sum(visits),
      share_schools = schools / sum(schools),
      overindex     = share_visits / share_schools
    )
  
  attr(oi_tbl, "school_var") <- attr(scope_df, "school_var")
  attr(oi_tbl, "visit_var")  <- attr(scope_df, "visit_var")
  oi_tbl
}

# ---- Concentration curve & index (low→high affluence) -----------------------
# Returns list with data, ACI (negative = affluent tilt), ATI (= -ACI), and var names.
ses_concentration <- function(scope_df, affluence_var = "mean_inc_house") {
  d <- scope_df %>%
    dplyr::arrange(.data[[affluence_var]]) %>%
    dplyr::mutate(
      school_w = schools / sum(schools),
      visit_w  = visits  / sum(visits),
      cs = cumsum(school_w),
      cv = cumsum(visit_w)
    )
  cs0 <- c(0, d$cs); cv0 <- c(0, d$cv)
  auc <- sum(diff(cs0) * (head(cv0, -1) + tail(cv0, -1)) / 2)
  ACI <- 2 * auc - 1
  ATI <- -ACI
  
  list(
    data = d, ACI = ACI, ATI = ATI,
    school_var = attr(scope_df, "school_var"),
    visit_var  = attr(scope_df,  "visit_var")
  )
}

# (kept for completeness, not used in titles anymore)
append_vars_to_title <- function(title, school_var, visit_var) {
  sprintf("%s (schools = %s; visits = %s)", title, school_var, visit_var)
}

# ---- Plots ------------------------------------------------------------------
plot_overindex <- function(oi_tbl, title = "Affluence Decile Over-Index") {
  school_var <- attr(oi_tbl, "school_var"); visit_var <- attr(oi_tbl, "visit_var")
  
  ggplot2::ggplot(oi_tbl, ggplot2::aes(x = factor(dec), y = overindex)) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(xend = factor(dec), y = 1, yend = overindex)) +
    ggplot2::labs(
      x = "Affluence decile (low → high)",
      y = sprintf("Visits share ÷ Schools share (visits = %s; schools = %s)", visit_var, school_var),
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# index_label = "ATI" (default; positive = affluent tilt) or "ACI"
# AXES: label every 0.1 as .0, .1, .2, ...; add red vertical line at x = .5
plot_concentration <- function(cc, title = "Visits Concentration by Affluence",
                               index_label = c("ATI","ACI")) {
  index_label <- match.arg(index_label)
  idx <- if (index_label == "ATI") cc$ATI else cc$ACI
  d <- cc$data
  cs0 <- c(0, d$cs); cv0 <- c(0, d$cv)
  
  # breaks and labels at every 0.1, rendered as ".0", ".1", ..., "1.0"
  breaks_01 <- seq(0, 1, by = 0.1)
  fmt_dot <- function(x) {
    s <- sprintf("%.1f", x)
    ifelse(x < 1, sub("^0", "", s), s)
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_vline(xintercept = 0.5, color = "red") +
    ggplot2::geom_line(ggplot2::aes(x = cs0, y = cv0)) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = breaks_01,
      minor_breaks = NULL,
      labels = fmt_dot,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = breaks_01,
      minor_breaks = NULL,
      labels = fmt_dot,
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = sprintf("Cumulative share of schools (%s)", cc$school_var),
      y = sprintf("Cumulative share of visits (%s)", cc$visit_var),
      title = sprintf("%s (%s = %.3f)", title, index_label, idx)
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(5, "pt")
    )
}


##### CHANGES TO MAKE TO FUNCTIONS

# CREATE A SET OF _outstate variables that are basically regional visits + national visits [create vars for number of schools and number of visits]


# ---- Example calls ----------------------------------------------------------

df_by_univ_eps %>% glimpse()
df_by_univ_eps %>% select(-c(starts_with('n_vistot_per'))) %>% select(starts_with('n_sch'),starts_with('n_vistot')) %>% glimpse()

# Pick the columns you want to use for supply & demand:
# (1) Total schools vs total visits

scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all") # all schools
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_priv") # private schools
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_pub") # public schools

# in-state visits
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all_instate") #
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_pub_instate") #
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_priv_instate") #

# regional visits
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all_inregion") # 
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_pub_inregion") # 
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_priv_inregion") # 

ses_concentration(scope_all, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI")
ses_overindex(scope_all, affluence_var = "mean_inc_house", ntiles = 10) %>% plot_overindex("Over-Index by Income Decile — ALL universities")

# national visits
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all_national") # all schools, national visits
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_priv_national") # private schools, national visits
scope_all <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_pub_national") # public schools, national visits

ses_concentration(scope_all, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI")
ses_overindex(scope_all, affluence_var = "mean_inc_house", ntiles = 10) %>% plot_overindex("Over-Index by Income Decile — ALL universities")


ses_concentration(scope_all, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI")
ses_concentration(scope_all, affluence_var = "pct_edu_baplus_all") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI")

ses_concentration(scope_all, affluence_var = "pct_nhisp_white") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI")
ses_concentration(scope_all, affluence_var = "pct_nhisp_black") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI") # try creating pct black + hispanic
ses_concentration(scope_all, affluence_var = "pct_nhisp_asian") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI") ## whoah.
ses_concentration(scope_all, affluence_var = "pct_hisp_all") %>% plot_concentration("Concentration — ALL universities", index_label = "ATI") ## 

### plot calls for individual groups

scope_object <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all")
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all",group_vals = c("private_libarts"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all",group_vals = c("private_national"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all",group_vals = c("public_research"))
ses_concentration(scope_object, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration", index_label = "ATI")

# in state
scope_object <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all_instate")
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_instate",group_vals = c("private_libarts"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_instate",group_vals = c("private_national"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_instate",group_vals = c("public_research"))

ses_concentration(scope_object, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration", index_label = "ATI")

# regional

scope_object <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all_inregion")
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_inregion",group_vals = c("private_libarts"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_inregion",group_vals = c("private_national"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_inregion",group_vals = c("public_research"))

ses_concentration(scope_object, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration", index_label = "ATI")

# national

scope_object <- build_scope(df_by_univ_eps, scope = "all",school_var = "n_sch_all_national")
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_national",group_vals = c("private_libarts"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_national",group_vals = c("private_national"))
scope_object <- build_scope(df_by_univ_eps,scope= "group", school_var = "n_sch_all_national",group_vals = c("public_research"))

ses_concentration(scope_object, affluence_var = "mean_inc_house") %>% plot_concentration("Concentration", index_label = "ATI")

# (3) PUBLIC schools vs PUBLIC visits (ALL)


# (4) ONE university, using any visit metric (e.g., total visits)
scope_one <- build_scope(df_by_univ_eps, scope = "one", univ_ids = "100751",
                         school_var = "n_sch_all",
                         visit_var  = "n_vistot_all")
plot_overindex(ses_overindex(scope_one), "Over-Index — Univ 100751")
plot_concentration(ses_concentration(scope_one), "Concentration — Univ 100751")
