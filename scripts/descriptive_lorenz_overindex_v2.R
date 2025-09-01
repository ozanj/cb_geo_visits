# ============================================================
#  Single- & Multi-Series Lorenz/Concentration & Over-Index
#  Parameterized by school_var (visit_var auto-derived)
#  Assumes tidyverse + ggplot2 loaded.
# ============================================================

####### RUN SCRIPT THAT CREATES OBJECT WITH ONE OBSERVATION PER UNIVERSITY, EPS THAT HAS VARIABLES ABOUT NUMBER OF SCHOOLS AND NUMBER OF VISITS TO THOSE SCHOOOLS

getwd()
source(file = file.path('scripts', 'create_univ_geo_df.R'))
getwd()


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

# ---- Scope builder (single series) ------------------------------------------
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

# ---- Decile over-index by affluence (single) --------------------------------
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

# ---- Concentration curve & index (single; low→high affluence) ---------------
# Returns list with data, ACI, ATI, and var names.
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

# ---- Plots (single) ---------------------------------------------------------
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

# AXES: label every 0.1 as .0, .1, .2, ...; add red vertical line at x = .5
plot_concentration <- function(cc, title = "Visits Concentration by Affluence",
                               index_label = c("ATI","ACI")) {
  index_label <- match.arg(index_label)
  idx <- if (index_label == "ATI") cc$ATI else cc$ACI
  d <- cc$data
  cs0 <- c(0, d$cs); cv0 <- c(0, d$cv)
  
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
      labels = fmt_dot,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = breaks_01,
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

# ============================================================================#
#                       MULTI–SERIES VERSIONS                                 #
# ============================================================================#

# ---- Build scope (multi series; long format) --------------------------------
# school_vars: character vector of n_sch* columns
# series_labels: optional labels; defaults to school_vars
build_scope_multi <- function(df,
                              scope = c("all", "group", "one"),
                              school_vars,
                              series_labels = NULL,
                              group_vals = NULL,
                              univ_ids   = NULL) {
  scope <- match.arg(scope)
  stopifnot(length(school_vars) >= 1)
  visit_vars <- vapply(school_vars, derive_visit_var, character(1), df_cols = names(df))
  if (is.null(series_labels)) series_labels <- school_vars
  stopifnot(length(series_labels) == length(school_vars))
  
  build_one <- function(sv, vv, lab) {
    if (scope == "all") {
      out <- df %>%
        dplyr::filter(.data$univ_id == "all") %>%
        dplyr::transmute(
          hs_eps_codename,
          series  = lab,
          schools = .data[[sv]],
          visits  = .data[[vv]],
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
          series  = lab,
          schools = dplyr::first(.data[[sv]]),
          visits  = sum(.data[[vv]], na.rm = TRUE),
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
          series  = lab,
          schools = .data[[sv]],
          visits  = .data[[vv]],
          mean_inc_house, pct_edu_baplus_all, pct_pov_yes,
          pct_nhisp_white, pct_nhisp_asian, pct_nhisp_black, pct_hisp_all
        )
    }
    out
  }
  
  long <- purrr::pmap_dfr(
    list(school_vars, visit_vars, series_labels),
    build_one
  ) %>%
    dplyr::mutate(
      schools = as.numeric(schools),
      visits  = as.numeric(visits)
    ) %>%
    dplyr::filter(!is.na(schools), schools > 0)
  
  # keep a tidy mapping for legends/annotations
  series_map <- tibble::tibble(
    series = series_labels,
    school_var = school_vars,
    visit_var  = visit_vars
  )
  attr(long, "series_map") <- series_map
  long
}

# ---- Concentration (multi) --------------------------------------------------
# Common affluence ordering per series; indices per series.
ses_concentration_multi <- function(scope_long, affluence_var = "mean_inc_house") {
  stopifnot("series" %in% names(scope_long))
  
  # compute cumulative shares within each series
  d <- scope_long %>%
    dplyr::arrange(.data[[affluence_var]], .by_group = FALSE) %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(.data[[affluence_var]], .by_group = TRUE) %>%
    dplyr::mutate(
      school_w = schools / sum(schools),
      visit_w  = visits  / sum(visits),
      cs = cumsum(school_w),
      cv = cumsum(visit_w)
    ) %>%
    dplyr::ungroup()
  
  # curves with an explicit (0,0) per series
  curves <- d %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(
      cs0 = c(0, cs),
      cv0 = c(0, cv),
      ord = dplyr::row_number(cs0),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()
  
  # indices (trapezoid AUC) per series
  indices <- curves %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(ord, .by_group = TRUE) %>%
    dplyr::summarise(
      auc = sum(diff(cs0) * (head(cv0, -1) + tail(cv0, -1)) / 2),
      ACI = 2 * auc - 1,
      ATI = -ACI,
      .groups = "drop"
    )
  
  # legend labels (series + var pairs); indices added on plotting
  series_map <- attr(scope_long, "series_map")
  out <- list(
    curves = curves,
    indices = indices,
    series_map = series_map
  )
  out
}

# ---- Over-index (multi) -----------------------------------------------------
# Uses common decile cut-points across ALL series; shares & OI computed within series.
ses_overindex_multi <- function(scope_long, affluence_var = "mean_inc_house", ntiles = 10) {
  stopifnot("series" %in% names(scope_long))
  d <- scope_long %>%
    dplyr::mutate(
      aff = .data[[affluence_var]],
      dec = dplyr::ntile(aff, ntiles)   # common breaks across all series
    ) %>%
    dplyr::group_by(series, dec) %>%
    dplyr::summarise(
      visits  = sum(visits,  na.rm = TRUE),
      schools = sum(schools, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(
      share_visits  = visits / sum(visits),
      share_schools = schools / sum(schools),
      overindex     = share_visits / share_schools
    ) %>%
    dplyr::ungroup()
  
  attr(d, "series_map") <- attr(scope_long, "series_map")
  d
}

# ---- Pretty legend labels ---------------------------------------------------
make_legend_labels <- function(series_map, indices = NULL, index_label = c("ATI","ACI")) {
  index_label <- match.arg(index_label)
  idx_tbl <- if (is.null(indices)) NULL else indices %>% dplyr::select(series, !!index_label := .data[[index_label]])
  lab <- series_map %>%
    dplyr::left_join(idx_tbl, by = "series") %>%
    dplyr::mutate(
      label = if (!is.null(idx_tbl)) {
        sprintf("%s (schools=%s; visits=%s; %s=%.3f)",
                series, school_var, visit_var, index_label, .data[[index_label]])
      } else {
        sprintf("%s (schools=%s; visits=%s)", series, school_var, visit_var)
      }
    )
  stats::setNames(lab$label, lab$series)
}

# ---- Plots (multi) ----------------------------------------------------------
plot_concentration_multi <- function(cc_multi,
                                     title = "Visits Concentration by Affluence",
                                     index_label = c("ATI","ACI")) {
  index_label <- match.arg(index_label)
  curves  <- cc_multi$curves
  indices <- cc_multi$indices
  series_map <- cc_multi$series_map
  
  # ticks every .1 with ".0", ".1", ...
  breaks_01 <- seq(0, 1, by = 0.1)
  fmt_dot <- function(x) {
    s <- sprintf("%.1f", x)
    ifelse(x < 1, sub("^0", "", s), s)
  }
  legend_labels <- make_legend_labels(series_map, indices, index_label)
  
  ggplot2::ggplot(curves, ggplot2::aes(x = cs0, y = cv0, color = series)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_vline(xintercept = 0.5, color = "red") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = breaks_01, labels = fmt_dot, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = breaks_01, labels = fmt_dot, expand = c(0, 0)) +
    ggplot2::scale_color_discrete(name = "Series", labels = legend_labels) +
    ggplot2::labs(
      x = "Cumulative share of schools",
      y = "Cumulative share of visits",
      title = title
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(5, "pt")
    )
}

plot_overindex_multi <- function(oi_multi,
                                 title = "Affluence Decile Over-Index") {
  series_map <- attr(oi_multi, "series_map")
  legend_labels <- make_legend_labels(series_map, indices = NULL)
  
  ggplot2::ggplot(oi_multi, ggplot2::aes(x = factor(dec), y = overindex, color = series, group = series)) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name = "Series", labels = legend_labels) +
    ggplot2::labs(
      x = "Affluence decile (low → high)",
      y = "Visits share ÷ Schools share",
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# ============================================================================#
#                               EXAMPLES                                      #
# ============================================================================#

# 1) ALL universities — compare Public vs Private, national scope
 scope_all_multi <- build_scope_multi(
   df_by_univ_eps,
   scope = "all",
   school_vars   = c("n_sch_all_national","n_sch_pub_national", "n_sch_priv_national"),
   series_labels = c("All schools - national","Public — national",  "Private — national")
 )
 cc_all_multi <- ses_concentration_multi(scope_all_multi)
 p_cc_all_multi <- plot_concentration_multi(cc_all_multi, "Concentration — ALL universities")
 p_cc_all_multi
#
# oi_all_multi <- ses_overindex_multi(scope_all_multi, ntiles = 10)
# p_oi_all_multi <- plot_overindex_multi(oi_all_multi, "Over-Index by Income Decile — ALL universities")
# p_oi_all_multi

# 2) GROUP — Private LACs; compare in-region vs national school bases
# scope_lac_multi <- build_scope_multi(
#   df_by_univ_eps,
#   scope = "group",
#   group_vals = c("private_libarts"),
#   school_vars   = c("n_sch_all_inregion", "n_sch_all_national"),
#   series_labels = c("All schools — in-region base", "All schools — national base")
# )
# plot_concentration_multi(ses_concentration_multi(scope_lac_multi),
#                          "Concentration — Private LACs")
# plot_overindex_multi(ses_overindex_multi(scope_lac_multi),
#                      "Over-Index — Private LACs")

# 3) ONE university (replace univ_id) — compare two bases
# scope_one_multi <- build_scope_multi(
#   df_by_univ_eps,
#   scope = "one",
#   univ_ids = "100751",
#   school_vars   = c("n_sch_all_instate", "n_sch_all_national"),
#   series_labels = c("In-state base", "National base")
# )
# plot_concentration_multi(ses_concentration_multi(scope_one_multi),
#                          "Concentration — Univ 100751")
# plot_overindex_multi(ses_overindex_multi(scope_one_multi),
#                      "Over-Index — Univ 100751")
