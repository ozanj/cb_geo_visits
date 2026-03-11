# ============================================================
#  Single- & Multi-Series Lorenz/Concentration & Over-Index
#  Parameterized by school_var (visit_var auto-derived)
#  Assumes tidyverse + ggplot2 loaded.
# ============================================================
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
  } else {
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

# ---- Concentration curve & index (single) -----------------------------------
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
    visit_var  = attr(scope_df, "visit_var")
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

plot_concentration <- function(cc, title = "Visits Concentration by Affluence",
                               index_label = c("ATI", "ACI")) {
  index_label <- match.arg(index_label)
  idx <- if (index_label == "ATI") cc$ATI else cc$ACI
  d <- cc$data
  cs0 <- c(0, d$cs); cv0 <- c(0, d$cv)
  
  breaks_01 <- seq(0, 1, by = 0.1)
  fmt_dot <- function(x) { s <- sprintf("%.1f", x); ifelse(x < 1, sub("^0", "", s), s) }
  
  ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_vline(xintercept = 0.5, color = "grey40") +          # changed: red → grey40
    ggplot2::geom_hline(yintercept = 0.5, color = "grey40") +          # added
    ggplot2::geom_line(ggplot2::aes(x = cs0, y = cv0)) +
    ggplot2::scale_x_continuous(limits = c(0,1), breaks = breaks_01, labels = fmt_dot, expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), breaks = breaks_01, labels = fmt_dot, expand = c(0,0)) +
    ggplot2::labs(
      x = sprintf("Cumulative share of schools (%s)", cc$school_var),
      y = sprintf("Cumulative share of visits (%s)", cc$visit_var),
      title = sprintf("%s (%s = %.3f)", title, index_label, idx)
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(linewidth = 0.3, color = "grey80"),
      panel.background  = ggplot2::element_rect(fill = "grey97", color = NA),
      axis.ticks.length = grid::unit(5, "pt")
    )
}

# ============================================================================#
#                       MULTI-SERIES VERSIONS                                 #
# ============================================================================#
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
          hs_eps_codename, series = lab,
          schools = .data[[sv]], visits = .data[[vv]],
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
    } else {
      stopifnot(length(univ_ids) == 1)
      out <- df %>%
        dplyr::filter(.data$univ_id == univ_ids) %>%
        dplyr::transmute(
          hs_eps_codename, series = lab,
          schools = .data[[sv]], visits = .data[[vv]],
          mean_inc_house, pct_edu_baplus_all, pct_pov_yes,
          pct_nhisp_white, pct_nhisp_asian, pct_nhisp_black, pct_hisp_all
        )
    }
    out
  }
  
  long <- purrr::pmap_dfr(list(school_vars, visit_vars, series_labels), build_one) %>%
    dplyr::mutate(schools = as.numeric(schools), visits = as.numeric(visits)) %>%
    dplyr::filter(!is.na(schools), schools > 0)
  
  series_map <- tibble::tibble(
    series = series_labels, school_var = school_vars, visit_var = visit_vars
  )
  attr(long, "series_map") <- series_map
  long
}

ses_concentration_multi <- function(scope_long, affluence_var = "mean_inc_house") {
  stopifnot("series" %in% names(scope_long))
  
  d <- scope_long %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(.data[[affluence_var]], .by_group = TRUE) %>%
    dplyr::mutate(
      school_w = schools / sum(schools),
      visit_w  = visits  / sum(visits),
      cs = cumsum(school_w),
      cv = cumsum(visit_w)
    ) %>%
    dplyr::ungroup()
  
  curves <- d %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(
      cs0 = c(0, cs), cv0 = c(0, cv),
      ord = dplyr::row_number(cs0),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()
  
  indices <- curves %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(ord, .by_group = TRUE) %>%
    dplyr::summarise(
      auc = sum(diff(cs0) * (head(cv0, -1) + tail(cv0, -1)) / 2),
      ACI = 2 * auc - 1,
      ATI = -ACI,
      .groups = "drop"
    )
  
  list(curves = curves, indices = indices, series_map = attr(scope_long, "series_map"))
}

ses_overindex_multi <- function(scope_long, affluence_var = "mean_inc_house", ntiles = 10) {
  stopifnot("series" %in% names(scope_long))
  d <- scope_long %>%
    dplyr::mutate(aff = .data[[affluence_var]], dec = dplyr::ntile(aff, ntiles)) %>%
    dplyr::group_by(series, dec) %>%
    dplyr::summarise(
      visits = sum(visits, na.rm = TRUE), schools = sum(schools, na.rm = TRUE),
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

make_legend_labels <- function(series_map, indices = NULL, index_label = c("ATI", "ACI")) {
  stats::setNames(series_map$series, series_map$series)
}

plot_concentration_multi <- function(cc_multi, title = NULL, index_label = c("ATI", "ACI")) {
  index_label <- match.arg(index_label)
  curves <- cc_multi$curves; series_map <- cc_multi$series_map
  
  breaks_01 <- seq(0, 1, by = 0.1)
  fmt_dot <- function(x) { s <- sprintf("%.1f", x); ifelse(x < 1, sub("^0", "", s), s) }
  legend_labels <- make_legend_labels(series_map)
  plot_title <- if (is.null(title) || title == "") NULL else title
  
  ggplot2::ggplot(curves, ggplot2::aes(x = cs0, y = cv0, color = series)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_vline(xintercept = 0.5, color = "grey40") +          # changed: red → grey40
    ggplot2::geom_hline(yintercept = 0.5, color = "grey40") +          # added
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(limits = c(0,1), breaks = breaks_01, labels = fmt_dot, expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), breaks = breaks_01, labels = fmt_dot, expand = c(0,0)) +
    ggplot2::scale_color_discrete(name = "Legend", labels = legend_labels) +
    ggplot2::labs(
      x = "Cumulative share of schools, ranked ascending by mean income",
      y = "Cumulative share of visits",
      title = plot_title
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(linewidth = 0.3, color = "grey80"),
      panel.background  = ggplot2::element_rect(fill = "grey97", color = NA),
      axis.ticks.length = grid::unit(5, "pt"),
      plot.title        = ggplot2::element_text(hjust = 0.5)
    )
}

plot_overindex_multi <- function(oi_multi, title = "Affluence Decile Over-Index") {
  series_map <- attr(oi_multi, "series_map")
  legend_labels <- make_legend_labels(series_map)
  
  ggplot2::ggplot(oi_multi, ggplot2::aes(x = factor(dec), y = overindex, color = series, group = series)) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name = "Series", labels = legend_labels) +
    ggplot2::labs(x = "Affluence decile (low → high)", y = "Visits share ÷ Schools share", title = title) +
    ggplot2::theme_minimal(base_size = 12)
}

# ============================================================================#
#                       CONCENTRATION GRID (flexible rows x cols)             #
# ============================================================================#

# ---- Single cell builder ----------------------------------------------------
plot_concentration_cell <- function(cc_multi,
                                    show_x_axis = FALSE,
                                    show_y_axis = FALSE,
                                    col_title   = NULL,
                                    row_title   = NULL,
                                    base_size   = 8) {
  curves     <- cc_multi$curves
  series_map <- cc_multi$series_map
  
  breaks_02 <- seq(0, 1, by = 0.2)
  fmt_dot <- function(x) { s <- sprintf("%.1f", x); ifelse(x < 1, sub("^0", "", s), s) }
  legend_labels <- stats::setNames(series_map$series, series_map$series)
  
  y_lab <- if (show_y_axis && !is.null(row_title)) row_title else if (show_y_axis) "Cumul. share of visits" else NULL
  x_lab <- if (show_x_axis) "Cumul. share of schools\n(ranked by mean income)" else NULL
  
  p <- ggplot2::ggplot(curves, ggplot2::aes(x = cs0, y = cv0, color = series)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0.5, color = "grey40", linewidth = 0.3) +  # changed: red → grey40
    ggplot2::geom_hline(yintercept = 0.5, color = "grey40", linewidth = 0.3) +  # added
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0,1), breaks = breaks_02, labels = fmt_dot, expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), breaks = breaks_02, labels = fmt_dot, expand = c(0,0)) +
    ggplot2::scale_color_discrete(name = NULL, labels = legend_labels) +
    ggplot2::labs(x = x_lab, y = y_lab, title = col_title) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(linewidth = 0.3, color = "grey80"),
      panel.background  = ggplot2::element_rect(fill = "grey97", color = NA),
      plot.title        = ggplot2::element_text(hjust = 0.5, size = base_size, face = "bold"),
      plot.margin       = ggplot2::margin(t = 3, r = 5, b = 3, l = 5)
    )
  
  if (!show_x_axis) p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  if (!show_y_axis) p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  p
}

# ---- Flexible concentration grid (any number of rows and columns) -----------
# cols: list of lists, each with $label (column header) and $group_vals (univ_classification value/s)
# rows: list of lists, each with $label (row header) and $school_vars (vector of 3 n_sch* vars)
# series_labels: labels for the 3 series (all / public / private school visits)
plot_concentration_grid <- function(df,
                                    base_size     = 8,
                                    cols = list(
                                      list(label = "Private Research", group_vals = "private_national"),
                                      list(label = "Private Lib Arts", group_vals = "private_libarts"),
                                      list(label = "Public Research",  group_vals = "public_research")
                                    ),
                                    rows = list(
                                      list(label = "In-state",               school_vars = c("n_sch_all_instate",  "n_sch_pub_instate",  "n_sch_priv_instate")),
                                      list(label = "In-region\n(out-state)", school_vars = c("n_sch_all_inregion", "n_sch_pub_inregion", "n_sch_priv_inregion")),
                                      list(label = "National",               school_vars = c("n_sch_all_national", "n_sch_pub_national", "n_sch_priv_national"))
                                    ),
                                    series_labels = c("All schools", "Public school visits", "Private school visits")) {
  
  n_rows <- length(rows)
  n_cols <- length(cols)
  cell_plots <- vector("list", n_rows * n_cols)
  
  for (r in seq_len(n_rows)) {
    for (c in seq_len(n_cols)) {
      idx <- (r - 1) * n_cols + c
      
      scope_obj <- build_scope_multi(
        df,
        scope         = "group",
        group_vals    = cols[[c]]$group_vals,
        school_vars   = rows[[r]]$school_vars,
        series_labels = series_labels
      )
      cc <- ses_concentration_multi(scope_obj)
      
      cell_plots[[idx]] <- plot_concentration_cell(
        cc,
        show_x_axis = (r == n_rows),
        show_y_axis = (c == 1),
        col_title   = if (r == 1) cols[[c]]$label else NULL,
        row_title   = if (c == 1) rows[[r]]$label else NULL,
        base_size   = base_size
      )
    }
  }
  
  patchwork::wrap_plots(cell_plots, ncol = n_cols, nrow = n_rows) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

# ============================================================================#
#                               GRAPH CALLS                                   #
# ============================================================================#
df_by_univ_eps %>% glimpse()
df_by_univ_eps %>% count(univ_classification)

# ----------------------------------------------------------------------------
# GRAPH 1: All visits, all universities — 3 series (all/public/private schools)
# Saved to results/ and embedded in index.qmd (fig-concentration)
# To change which universities are included, switch scope from "all" to "group"
# and add group_vals = c("private_national") etc.
# ----------------------------------------------------------------------------
scope_all <- build_scope_multi(
  df_by_univ_eps,
  scope         = "all",
  school_vars   = c("n_sch_all", "n_sch_pub", "n_sch_priv"),
  series_labels = c("All schools", "Public school visits", "Private school visits")
)
concentration_multi_object <- ses_concentration_multi(scope_all)
concentration_multi_graph  <- plot_concentration_multi(concentration_multi_object, title = "All visits")
concentration_multi_graph
ggplot2::ggsave(
  filename = "results/concentration_all_visits.pdf",
  plot     = concentration_multi_graph,
  width    = 11,
  height   = 7
)

# ----------------------------------------------------------------------------
# GRAPH 2: 3x3 grid
#   Columns: Private Research | Private Lib Arts | Public Research
#   Rows:    In-state | In-region (out-of-state) | National (out-of-region)
#   Each cell: 3 series (all / public / private school visits)
# Uses default rows and cols arguments — no need to specify them
# ----------------------------------------------------------------------------
grid_3x3 <- plot_concentration_grid(df_by_univ_eps, base_size = 8)
grid_3x3
ggplot2::ggsave(
  filename = "results/concentration_3x3.pdf",
  plot     = grid_3x3,
  width    = 11,
  height   = 9
)

# ----------------------------------------------------------------------------
# GRAPH 3: 3x2 grid
#   Columns: Private Research | Private Lib Arts | Public Research
#   Rows:    In-state | All out-of-state (in-region + national combined)
# ----------------------------------------------------------------------------
grid_3x2 <- plot_concentration_grid(
  df_by_univ_eps,
  base_size = 8,
  rows = list(
    list(label = "In-state",      school_vars = c("n_sch_all_instate",  "n_sch_pub_instate",  "n_sch_priv_instate")),
    list(label = "Out-of-state",  school_vars = c("n_sch_all_outstate", "n_sch_pub_outstate", "n_sch_priv_outstate"))
  )
)
grid_3x2
ggplot2::ggsave(
  filename = "results/concentration_3x2.pdf",
  plot     = grid_3x2,
  width    = 11,
  height   = 7
)

# ----------------------------------------------------------------------------
# ADDITIONAL GRAPHS (add here as needed)
# Template — copy and modify:
#
# scope_XX <- build_scope_multi(
#   df_by_univ_eps,
#   scope         = "all",            # "all", "group", or "one"
#   # group_vals  = "private_national", # use with scope = "group"
#   # univ_ids    = "100751",           # use with scope = "one"
#   school_vars   = c("n_sch_all_instate", "n_sch_pub_instate", "n_sch_priv_instate"),
#   series_labels = c("All schools", "Public school visits", "Private school visits")
# )
# graph_XX <- plot_concentration_multi(ses_concentration_multi(scope_XX), title = "YOUR TITLE")
# graph_XX
# ggplot2::ggsave("results/YOUR_FILENAME.pdf", plot = graph_XX, width = 11, height = 7)
#
# For a custom grid, pass rows and/or cols to plot_concentration_grid():
# grid_XX <- plot_concentration_grid(df_by_univ_eps, rows = list(...), cols = list(...))
# ggplot2::ggsave("results/YOUR_FILENAME.pdf", plot = grid_XX, width = 11, height = 7)
# ----------------------------------------------------------------------------