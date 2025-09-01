################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < descriptive_RQs.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/22/2025
## [ DESC ] < post-modeling descriptive RQs>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1000)
#options(width = 160)

library(tidyverse)
library(forcats)
library(scales)


####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES


getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()

# remove objects from cb_geo (ajs manuscript) mapping
rm(create_rq1_map,format_vars,get_palette)

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

###############
############### SUB-RQ1: TO WHAT EXTENT ARE VISIT PATTERNS CONSISTENT WITH RECOMMENDATIONS FROM THE MARKET SEGMENT MODEL?
###############

# MARKET SEGMENT MODEL RECOMMENDATIONS:
  #Schools with an in-state focus should visit in-state Geomarkets
  #Schools with a regional focus should primarily visit schools in affluent geomarkets in their region
  #Schools with a national focus should primarily visit schools in affluent geomarkets across the nation

# ANALYSES TO CREATE:
  pubprivhs_univ_df %>% glimpse()
pubprivhs_univ_df %>% count(hs_univ_market)

  # START WITH ONE UNIVERSITY, LET'S SAY SWARTHMORE

  # total number of visits
  pubprivhs_univ_df %>% filter(univ_id == '216287') %>% count(num_visits)
  pubprivhs_univ_df %>% filter(univ_id == '216287') %>% count(visit01)

  pubprivhs_univ_df %>% filter(univ_id == '216287') %>% summarize(
      tot_visits = sum(num_visits),
      visited_schools = sum(visit01)
      
    )
  
  # what is swarthmore's geomarket and market region 
  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(univ_eps_region)
  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(univ_eps_codename)
  
  # count of high schools by eps region [this is independent of swarthmore]
  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(hs_eps_region)
  
  # count of high schools by market segment [vis-a-vis swarthmore]
  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(hs_univ_market)
  
  # cross tabulation of high school eps [independent of swarthmore] region and high schools by market segment [vis-a-vis swarthmore]
  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(hs_univ_market,hs_eps_region)
  
  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(hs_univ_market)
  # by market segment (local, in-state, regional, national), the number, row-pct, and col-pct of visited high schools
  pubprivhs_univ_df %>% filter(univ_id == '160755',!is.na(hs_univ_market)) %>% group_by(hs_eps_region) %>% # middlebury: 230959; tulane = 160755
    summarize(
      # all schools
      n_sch = n(),
      n_vis = sum(visit01),
      n_no_vis = sum(visit01==0),
      prow_vis = n_vis/n_sch*100, # for each market segment, what is the number of visited high schools divided by total high schools in market segment
      # public schools
      n_pub_sch = sum(hs_control == 'public'),
      n_pub_vis = sum(visit01==1 & hs_control == 'public'),
      n_pub_no_vis = sum(visit01==0 & hs_control == 'public'),
      prow_pub_vis = n_pub_vis/n_pub_sch*100,
      # private schools
      n_priv_sch = sum(hs_control == 'private'),
      n_priv_vis = sum(visit01==1 & hs_control == 'private'),
      n_priv_no_vis = sum(visit01==0 & hs_control == 'private'),
      prow_priv_vis = n_priv_vis/n_priv_sch*100,
      .groups = "drop"
    ) %>% 
    mutate(
      pcol_vis = n_vis/sum(n_vis)*100, # for each market segment, what is number of visited high schools divided by total number of visited high schools across all market segments
      pcol_pub_vis = n_pub_vis/sum(n_pub_vis)*100,
      pcol_priv_vis = n_priv_vis/sum(n_priv_vis)*100,
    ) %>% relocate(pcol_vis, .after = prow_vis) %>% relocate(pcol_pub_vis, .after = prow_pub_vis) # %>% select(-c(contains('no_vis')))

  pubprivhs_univ_df %>% filter(univ_id == '216287',!is.na(hs_univ_market)) %>% count(hs_control)
    
# function to summarize visits
  
#############
############# CREATE FUNCTION THAT CREATES VARIABLES THAT DESCRIBE SUMMARIES OF VISITS AND ARE INPUTS INTO TABLES/GRAPHS
#############
  
summarize_visits <- function(
  df,
  id = univ_id,
  by = hs_eps_region,
  control = hs_control,
  visit = visit01,
  keep_wide = FALSE,
  digits = 1,
  include_index = TRUE
) {
  # ---- pull metadata from global `univ_df`
  meta_keep <- univ_df %>%
    transmute(
      univ_id = as.character(univ_id),
      univ_abbrev,
      univ_classification,
      univ_usnwr_rank
    )
  
  # ---- base counts by (univ x by x control)
  base <- df %>%
    filter(!is.na({{ by }})) %>%
    mutate(
      {{ id }} := as.character({{ id }}),
      ctrl = forcats::fct_drop(factor({{ control }}))  # <- fix #1
    ) %>%
    group_by({{ id }}, {{ by }}, ctrl) %>%
    summarize(
      n_sch = dplyr::n(),
      n_vis = sum({{ visit }} == 1, na.rm = TRUE),      # <- fix #2
      .groups = "drop"
    )
  
  # ---- add ALL (public + private)
  all_ctrl <- base %>%
    group_by({{ id }}, {{ by }}) %>%
    summarize(
      n_sch = sum(n_sch),
      n_vis = sum(n_vis),
      .groups = "drop"
    ) %>%
    mutate(ctrl = forcats::fct_relevel(factor("all"), "all"))
  
  long <- dplyr::bind_rows(base, all_ctrl) %>%
    arrange({{ id }}, {{ by }}, ctrl) %>%
    group_by({{ id }}, {{ by }}, ctrl) %>%
    mutate(prow_vis = 100 * n_vis / n_sch) %>%
    ungroup() %>%
    group_by({{ id }}, ctrl) %>%
    mutate(
      pcol_vis = 100 * n_vis / sum(n_vis),            # may be NaN if sum=0
      pcol_vis = ifelse(is.finite(pcol_vis), pcol_vis, NA_real_)  # guard
    ) %>%
    ungroup()
  
  if (include_index) {
    long <- long %>%
      group_by({{ id }}, ctrl) %>%
      mutate(
        exposure    = n_sch / sum(n_sch),
        over_index  = (pcol_vis / 100) - exposure,
        ratio_index = dplyr::if_else(exposure > 0, (pcol_vis / 100) / exposure, NA_real_)
      ) %>%
      ungroup()
  }
  
  long <- long %>%
    mutate(
      prow_vis = round(prow_vis, digits),
      pcol_vis = round(pcol_vis, digits)
    )
  
  out_long <- long %>%
    rename(univ_id = {{ id }}) %>%
    left_join(meta_keep, by = "univ_id") %>%
    arrange(univ_classification, is.na(univ_usnwr_rank), univ_usnwr_rank) %>%
    relocate(univ_id, univ_abbrev, univ_classification, univ_usnwr_rank)
  
  if (!keep_wide) return(out_long)
  
  out_wide <- out_long %>%
    select(univ_id, univ_abbrev, univ_classification, univ_usnwr_rank,
           {{ by }}, ctrl, n_sch, n_vis, prow_vis, pcol_vis) %>%
    tidyr::pivot_wider(
      names_from  = ctrl,
      values_from = c(n_sch, n_vis, prow_vis, pcol_vis),
      names_sep   = "_"
    ) %>%
    arrange(univ_classification, is.na(univ_usnwr_rank), univ_usnwr_rank)
  
  out_wide
}
  
summarize_visits(pubprivhs_univ_df, by = hs_eps_region) %>% print(n=200)

#############
############# HEAT MAP
#############

  # 1) HEAT MAP (matrix) — what you're using now
  #    - Show % visited (prow_vis) as fill across X = region/market, Y = college (ordered by rank).
  #    - Facet columns for control: All | Public | Private.
  #    - geom_tile() with a single-ended gradient (0 = white → high = red).
  #    - Good for scanning patterns across many colleges at once.
  
  
# --- Plot function -----------------------------------------------------------
# metric options:
#   - "percent_visited": percent of schools visited within each X group (prow_vis)
#   - "count_visited"  : number of schools visited (n_vis)
#   - "share_visited"  : within each college + facet, the share of all visits
#                        that fall in each X group (pcol_vis)

plot_visit_heatmaps <- function(vis_long, by, include_all = TRUE,
                                metric = c("percent_visited", "count_visited", "share_visited")) {
  metric <- match.arg(metric)
  by_quo <- rlang::enquo(by)
  by_sym <- rlang::ensym(by)
  
  # --- Desired ordering rules ------------------------------------------------
  # 1) Classification: put liberal arts at the top group, others follow.
  all_classes <- unique(vis_long$univ_classification)
  class_order <- if ("private_libarts" %in% all_classes) {
    c("private_libarts", setdiff(all_classes, "private_libarts"))
  } else {
    all_classes
  }
  
  # 2) College "home region" for row ordering inside each classification.
  #    Prefer univ_eps_region in vis_long; if absent, try to pull from `univ_df`
  #    (assumed to be in the environment). Otherwise fall back to NA (goes last).
  meta_y <- vis_long %>%
    dplyr::distinct(univ_id, univ_abbrev, univ_usnwr_rank, univ_classification)
  
  has_eps_in_vis <- "univ_eps_region" %in% names(vis_long)
  if (has_eps_in_vis) {
    meta_y <- meta_y %>%
      dplyr::left_join(
        vis_long %>% dplyr::distinct(univ_id, univ_eps_region),
        by = "univ_id"
      )
  } else if (exists("univ_df", inherits = TRUE) && "univ_eps_region" %in% names(get("univ_df"))) {
    meta_y <- meta_y %>%
      dplyr::left_join(
        get("univ_df") %>% dplyr::select(univ_id, univ_eps_region),
        by = "univ_id"
      )
  } else {
    meta_y$univ_eps_region <- NA_character_
  }
  
  pref_regions <- c("new_england", "middle_states", "midwest", "south", "southwest", "west")
  other_regions <- setdiff(as.character(stats::na.omit(unique(meta_y$univ_eps_region))), pref_regions)
  region_order <- c(pref_regions, other_regions)
  
  meta_y <- meta_y %>%
    dplyr::mutate(
      univ_classification = factor(univ_classification, levels = class_order),
      univ_eps_region     = factor(as.character(univ_eps_region), levels = region_order)
    ) %>%
    dplyr::arrange(univ_classification, univ_eps_region, univ_usnwr_rank, univ_abbrev)
  
  # Desired TOP→BOTTOM order of college rows
  y_top_to_bottom <- meta_y$univ_abbrev
  
  # --- X-axis order from chosen `by` variable (respect factor levels) --------
  by_vec  <- vis_long %>% dplyr::pull(!!by_quo)
  x_order <- if (is.factor(by_vec)) levels(by_vec) else unique(by_vec)
  
  # --- Panels to show + prep data -------------------------------------------
  ctrl_levels <- if (include_all) c("all", "public", "private") else c("public", "private")
  
  vis_plot <- vis_long %>%
    dplyr::filter(ctrl %in% ctrl_levels) %>%
    dplyr::mutate(
      ctrl        = factor(ctrl, levels = ctrl_levels),
      # ggplot draws the last factor level at the TOP on the y-axis.
      # To get y_top_to_bottom visually top→bottom, reverse the levels here.
      univ_abbrev = factor(univ_abbrev, levels = rev(y_top_to_bottom))
    ) %>%
    dplyr::mutate(!!by_sym := factor(.data[[rlang::as_string(by_sym)]], levels = x_order))
  
  # --- Choose fill variable + shared limits across panels --------------------
  if (metric == "percent_visited") {
    fill_col <- "prow_vis"  # percentage points (0–100)
    max_raw  <- max(vis_plot[[fill_col]], na.rm = TRUE)
    max_cap  <- max(ceiling(max_raw / 5) * 5, 5)
    fill_limits <- c(0, max_cap)
    scale_fill <- ggplot2::scale_fill_gradient(
      name   = "% visited",
      limits = fill_limits,
      low    = "white",
      high   = "#B2182B",
      breaks = scales::pretty_breaks(n = 5),
      labels = function(x) paste0(x, "%")
    )
  } else if (metric == "count_visited") {
    fill_col <- "n_vis"
    max_raw  <- max(vis_plot[[fill_col]], na.rm = TRUE)
    pr <- pretty(c(0, max_raw), n = 5)
    fill_limits <- c(min(pr), max(pr))
    scale_fill <- ggplot2::scale_fill_gradient(
      name   = "Visited (count)",
      limits = fill_limits,
      low    = "white",
      high   = "#B2182B",
      breaks = scales::pretty_breaks(n = 5),
      labels = scales::comma
    )
  } else { # metric == "share_visited"
    fill_col <- "pcol_vis"   # 0–100 within each (college x facet)
    fill_limits <- c(0, 100)
    scale_fill <- ggplot2::scale_fill_gradient(
      name   = "Share of visits",
      limits = fill_limits,
      low    = "white",
      high   = "#B2182B",
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = function(x) paste0(x, "%")
    )
  }
  
  # --- Panel builder (adds "College (Nvis)" labels for that facet) -----------
  panel <- function(df, title, y_levels_desc) {
    lab_map <- tibble::tibble(univ_abbrev = y_levels_desc) %>%
      dplyr::left_join(
        df %>%
          dplyr::group_by(univ_abbrev) %>%
          dplyr::summarise(total_vis = sum(n_vis, na.rm = TRUE), .groups = "drop"),
        by = "univ_abbrev"
      ) %>%
      dplyr::mutate(
        total_vis  = dplyr::coalesce(total_vis, 0L),
        univ_label = paste0(as.character(univ_abbrev), " (", total_vis, ")")
      )
    
    df2 <- df %>%
      dplyr::left_join(lab_map, by = "univ_abbrev") %>%
      dplyr::mutate(univ_label = factor(univ_label, levels = lab_map$univ_label))
    
    ggplot2::ggplot(
      df2,
      ggplot2::aes(x = !!by_sym, y = univ_label, fill = .data[[fill_col]])
    ) +
      ggplot2::geom_tile(width = 0.95, height = 0.95) +
      scale_fill +
      ggplot2::labs(title = title, x = NULL, y = NULL) +
      ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 10, face = "bold",
                                             hjust = 0.5, margin = ggplot2::margin(b = 4)),
        axis.text.x  = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
        axis.text.y  = ggplot2::element_text(size = 8),
        panel.grid   = ggplot2::element_blank(),
        plot.margin  = ggplot2::margin(t = 2, r = 2, b = 2, l = 2)
      )
  }
  
  y_levels_desc <- levels(vis_plot$univ_abbrev)
  
  panels <- list()
  if (include_all)
    panels <- c(panels, list(panel(dplyr::filter(vis_plot, ctrl == "all"),    "All",    y_levels_desc)))
  panels <- c(panels,
              list(
                panel(dplyr::filter(vis_plot, ctrl == "public"),  "Public",  y_levels_desc),
                panel(dplyr::filter(vis_plot, ctrl == "private"), "Private", y_levels_desc)
              ))
  
  patchwork::wrap_plots(plotlist = panels, ncol = length(panels), widths = rep(1, length(panels))) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(
      legend.position = "right",
      panel.spacing.x = grid::unit(0.15, "lines")
    )
}


# --- Build long summary for hs_univ_market (assumes summarize_visits already defined) ----
vis_long_market <- summarize_visits(pubprivhs_univ_df, by = hs_univ_market)

# GOAL IS TO SHOW THE READER WHTHER HS VISIT STRATEGY IS PREDOMENANTLY LOCAL, IN-STATE, REGIONAL, OR NATIONAL

# Share of a college’s visits that fall in each market (composition)
plot_visit_heatmaps(vis_long_market, by = hs_univ_market, include_all = TRUE, metric = "share_visited")
  # this tells us the extent to which a colleges recruiting strategy is local, in-state, regional, national
  # the graph cells don't tell us the extent of the high school recruiting visit strategy. 
    # but the numbers in parentheses tell us that, if the reader notices them

  # these calls don't do a good job of conveying the overall market segment (local, in-state, regional, national)
  # --- Example calls (by = hs_univ_market) -------------------------------------
  # Percent of schools visited within each market (per college)
  #plot_visit_heatmaps(vis_long_market, by = hs_univ_market, include_all = TRUE, metric = "percent_visited")
  
  # Count of schools visited within each market (per college)
  #plot_visit_heatmaps(vis_long_market, by = hs_univ_market, include_all = TRUE, metric = "count_visited")


# --- Build long summaries (examples; assumes summarize_visits already defined) ----
vis_long_region <- summarize_visits(pubprivhs_univ_df, by = hs_eps_region)
# vis_long_market <- summarize_visits(pubprivhs_univ_df, by = hs_univ_market)  # other dimension

# GOAL IS TO SHOW READER WHICH REGIONS OF THE COUNTRY THEY ARE DOING LOTS OF VISITS
# NOT SURE WHICH ONE TO PREFER. MAYBE metric = "share_visited"...

# Share of a college’s visits that fall in each region (composition)
plot_visit_heatmaps(vis_long_region, by = hs_eps_region,include_all = TRUE, metric = "share_visited")

  # Percent of schools visited within each region (per college)
  #plot_visit_heatmaps(vis_long_region, by = hs_eps_region,include_all = TRUE, metric = "percent_visited")
  
  # Count of schools visited within each region (per college)
  #plot_visit_heatmaps(vis_long_region, by = hs_eps_region,include_all = TRUE, metric = "count_visited")



#############
############# TREE MAP
#############

# 6) TREEMAP (share composition)
#    - For each college (facet_wrap), area = visits or share of visits (pcol_vis) by region/market.
#    - treemapify::geom_treemap() + geom_treemap_text() (fill by region/market).
#    - Answers: “Where do this college’s visits come from?” in one compact block.
library(treemapify)

# Robust treemap of visit composition by region/market
# vis_long : output of summarize_visits(..., keep_wide = FALSE)
# by       : grouping variable used in summarize_visits (e.g., hs_eps_region)
# ctrl     : "all" (default), "public", or "private"
# area     : "visits" (tile area = n_vis) or "share" (tile area = pcol_vis)
# colleges : optional character vector of univ_abbrev to include; default = all
# ncol     : facet columns (or patchwork columns if use_patchwork = TRUE)
# min_share: when area = "share", set values < this (in %) to 0 before plotting
# show_text: turn treemap labels on/off (labels sometimes trigger the grid error)
# use_patchwork: build one treemap per college and arrange (avoids facet grid bugs)
plot_visit_treemap <- function(vis_long, by,
                               ctrl = "all",
                               area = c("visits", "share"),
                               colleges = NULL,
                               ncol = 4,
                               min_share = 0,
                               show_text = TRUE,
                               use_patchwork = FALSE) {
  area   <- match.arg(area)
  by_sym <- rlang::ensym(by)
  by_str <- rlang::as_string(by_sym)
  
  df <- vis_long %>%
    dplyr::filter(ctrl == !!ctrl) %>%
    { if (!is.null(colleges)) dplyr::filter(., univ_abbrev %in% colleges) else . } %>%
    dplyr::group_by(univ_id, univ_abbrev, univ_classification, univ_usnwr_rank) %>%
    dplyr::mutate(total_vis = sum(n_vis, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_vis > 0)
  
  # Order facets: lib arts top, then by rank then name
  class_order <- if ("private_libarts" %in% unique(df$univ_classification)) {
    c("private_libarts", setdiff(unique(df$univ_classification), "private_libarts"))
  } else {
    unique(df$univ_classification)
  }
  df <- df %>%
    dplyr::mutate(univ_classification = factor(univ_classification, levels = class_order)) %>%
    dplyr::arrange(univ_classification, univ_usnwr_rank, univ_abbrev)
  
  # Aggregate to one row per (univ x region)
  df_agg <- df %>%
    dplyr::group_by(univ_id, univ_abbrev, univ_classification, univ_usnwr_rank, {{ by }}) %>%
    dplyr::summarise(
      n_vis    = sum(n_vis, na.rm = TRUE),
      pcol_vis = sum(pcol_vis, na.rm = TRUE),
      .groups  = "drop"
    )
  
  # Choose area & labels
  if (area == "visits") {
    df_agg <- df_agg %>%
      dplyr::mutate(
        area_val  = n_vis,
        label_val = paste0(.data[[by_str]], "\n", scales::comma(n_vis), " visits")
      )
  } else {
    df_agg <- df_agg %>%
      dplyr::mutate(
        pcol_vis  = dplyr::if_else(is.na(pcol_vis), 0, pcol_vis),
        pcol_vis  = dplyr::if_else(pcol_vis < min_share, 0, pcol_vis),
        area_val  = pcol_vis,
        label_val = paste0(.data[[by_str]], "\n", scales::number(pcol_vis, accuracy = 0.1), "%")
      )
  }
  
  # Drop zero/NA area rows
  df_agg <- df_agg %>% dplyr::filter(is.finite(area_val), area_val > 0)
  
  # Keep only colleges with positive total area
  keep_colleges <- df_agg %>%
    dplyr::group_by(univ_abbrev) %>%
    dplyr::summarise(tot = sum(area_val, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(tot > 0) %>%
    dplyr::pull(univ_abbrev)
  df_agg <- df_agg %>% dplyr::filter(univ_abbrev %in% keep_colleges)
  
  if (nrow(df_agg) == 0L) {
    stop("No colleges have positive area after filtering; try lowering min_share or switching to area = 'visits'.")
  }
  
  title_txt <- paste0(
    "Visit Composition by ", by_str, " — tiles show ",
    if (area == "visits") "number of visits" else "share of visits",
    " (ctrl = ", ctrl, ")"
  )
  
  # Helper to build one college treemap (used when use_patchwork = TRUE)
  build_one <- function(dsub) {
    p <- ggplot2::ggplot(
      dsub,
      ggplot2::aes(area = area_val, fill = .data[[by_str]], label = label_val)
    ) +
      treemapify::geom_treemap() +
      { if (show_text) treemapify::geom_treemap_text(place = "centre", grow = TRUE,
                                                     reflow = TRUE, min.size = 6,
                                                     colour = "white", fontface = "bold",
                                                     lineheight = 0.95) } +
      ggplot2::labs(title = unique(dsub$univ_abbrev), x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 2)),
        panel.grid = ggplot2::element_blank(),
        legend.position = "none"
      )
    p
  }
  
  if (use_patchwork) {
    # Build per-college plots and arrange with patchwork
    plots <- df_agg %>%
      dplyr::group_split(univ_abbrev, .keep = FALSE) %>%
      purrr::map(build_one)
    combined <- patchwork::wrap_plots(plots, ncol = ncol) +
      patchwork::plot_annotation(title = title_txt) &
      ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5))
    return(combined)
  }
  
  # Faceted version (may trigger grid bug if labels cause issues; toggle show_text)
  gg <- ggplot2::ggplot(
    df_agg,
    ggplot2::aes(area = area_val, fill = .data[[by_str]], label = label_val)
  ) +
    treemapify::geom_treemap() +
    { if (show_text) treemapify::geom_treemap_text(place = "centre", grow = TRUE,
                                                   reflow = TRUE, min.size = 6,
                                                   colour = "white", fontface = "bold",
                                                   lineheight = 0.95) } +
    ggplot2::facet_wrap(~ univ_abbrev, ncol = ncol, drop = TRUE) +
    ggplot2::labs(title = title_txt, x = NULL, y = NULL, fill = by_str) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 9)
    )
  
  gg
}

vis_long_market <- summarize_visits(pubprivhs_univ_df, by = hs_univ_market)
vis_long_market

plot_visit_treemap(vis_long_market, by = hs_univ_market, ctrl = "all", area = "visits",
                   ncol = 5, min_share = 0.2, show_text = FALSE)

# A) Facets, but TURN OFF TEXT (most common fix for the grid error)
#plot_visit_treemap(vis_long, by = hs_eps_region, ctrl = "all", area = "visits",ncol = 5, min_share = 0.2, show_text = FALSE)

# Build the long summary
vis_long <- summarize_visits(pubprivhs_univ_df, by = hs_eps_region)

# A) Facets, but TURN OFF TEXT (most common fix for the grid error)
plot_visit_treemap(vis_long, by = hs_eps_region, ctrl = "all", area = "visits",
                   ncol = 5, min_share = 0.2, show_text = FALSE)

plot_visit_treemap(vis_long, by = hs_eps_region, ctrl = "all", area = "visits",
                   ncol = 5, min_share = 0.2, use_patchwork = TRUE, show_text = TRUE)

# B) If A still errors on your setup, switch to PATCHWORK mode (bulletproof)
plot_visit_treemap(vis_long, by = hs_eps_region, ctrl = "all", area = "share",
                   ncol = 5, min_share = 0.2, use_patchwork = TRUE)

# C) Counts instead of shares (often avoids tiny-slice panels)
#plot_visit_treemap(vis_long, by = hs_eps_region, ctrl = "all", area = "visits",ncol = 5, show_text = TRUE)

rm(visit_long,visit_long_market)

#############
############# WHICH GEOMARKETS GET VISITS
#############

# data frames to use
events_df %>% glimpse()
allyr_anal_eps_sf %>% glimpse()
pubprivhs_univ_df %>% glimpse()
univ_df %>% glimpse()

#############
#############
#############

pubprivhs_univ_df %>% filter(univ_id == '147767') %>% count(hs_eps_region)
pubprivhs_univ_df %>% filter(univ_id == '147767') %>% count(univ_eps_region)

pubprivhs_univ_df %>% glimpse()
pubprivhs_univ_df %>% count(hs_univ_market)

library(dplyr)

# create function to spit out results
  # arguments
    # which universities
      # could be per classification or per university; 
    # which high schools
    # how many rows to show
    # calculates cumulative number and cu ulative percent [nah]
    # variables to show:
      # number of schools
      # total population
    
  # 
#created this dataset that shows number of recruiting visits received by each geomarket and some characteristics of the geomarket. interested in ranking which geomarkets have the most recruiting visits per school [already created these variables] and showing the characteristics of geomarkets that are highly vs. lowly ranked. probably should be visualized. how do you recommend doing this. what are a few good options
df_by_univ_eps %>% glimpse()
df_by_univ_eps %>% count(univ_abbrev) %>% print(n=50)

####!!!!!!!!!!!!!!!!!!!
# I think show this in the manuscript maybe just top 30 for public schools and top 30 for private schools]

df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_pub_national)) %>% select(hs_eps_codename,n_sch_pub_national,n_vistot_pub_national,n_vistot_per_sch_pub_national,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools
df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_priv_national)) %>% select(hs_eps_codename,n_sch_priv_national,n_vistot_priv_national,n_vistot_per_sch_priv_national,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools
#df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_all_national)) %>% select(hs_eps_codename,n_sch_all_national,n_vistot_all_national,n_vistot_per_sch_all_national,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools




df_by_univ_eps %>% filter(univ_id == 'all') %>% arrange(desc(n_vistot_per_sch_all)) %>% select(hs_eps_codename,n_sch_all,n_vistot_all,n_vistot_per_sch_all,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools
df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_pub)) %>% select(hs_eps_codename,n_sch_pub,n_vistot_pub,n_vistot_per_sch_pub,mean_inc_house,pct_edu_baplus_all,pct_pov_yes) %>% print(n=305) # public schools
df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_priv)) %>% select(hs_eps_codename,n_sch_priv,n_vistot_priv,n_vistot_per_sch_priv,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools


df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_all_national)) %>% select(hs_eps_codename,n_sch_all_national,n_vistot_all_national,n_vistot_per_sch_all_national,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools
df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_pub_national)) %>% select(hs_eps_codename,n_sch_pub_national,n_vistot_pub_national,n_vistot_per_sch_pub_national,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=150) # all schools
df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_priv_national)) %>% select(hs_eps_codename,n_sch_priv_national,n_vistot_priv_national,n_vistot_per_sch_priv_national,mean_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_white,pct_nhisp_asian,pct_nhisp_black,pct_hisp_all) %>% print(n=50) # all schools

# START BY RETHINKING THE RESEARCH QUESTIONS
  # herfinahl index -- share of visits captured by the top X geomarkets??/

# To what extent are visit patterns consistent with recommendations from the Market Segment Model? [descriptive]
# These recommendations are: 
#   Schools with an in-state focus should visit in-state Geomarkets
# Schools with a regional focus should primarily visit schools in affluent geomarkets in their region
# Schools with a national focus should primarily visit schools in affluent geomarkets across the nation [including their region]
# 
# What are the SES characteristics of the geomarkets that receive a lot of visits

# ---------------------------------------------------------------
# Approaches for analyzing and visualizing geomarket visit rates
# ---------------------------------------------------------------
# 1. Ranked Summary Table
#    - Arrange geomarkets by visits per school.
#    - View or export a clean table of top/bottom geomarkets and key variables.

# 2. Bar Chart of Top and Bottom Geomarkets
#    - Highlight the top and bottom 10 geomarkets visually.
#    - Use color to distinguish high vs. low ranking groups.

# 3. Correlation Heatmap
#    - Explore correlations between visits per school and market characteristics.
#    - Quick way to see which variables are strongly associated.
      # TRIED IT. DIDN'T LOVE IT.

# 4. Scatter Plots
#    - Plot visits per school against key demographics (e.g., income, % Hispanic).
#    - Add regression lines to show trends.

ggplot(df_by_univ_eps, aes(x = mean_inc_house, x = n_vistot_per_sch_all)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Mean Household Income", y = "Visits per School",
       title = "Relationship Between Income and Visits per School")

# 5. Geographic Maps
#    - Join the data to EPS shapefiles and map visits per school.
#    - Use leaflet for interactive maps with color bins and hover labels.

# 6. Cluster Analysis
#    - Group geomarkets into clusters based on visit rates and characteristics.
#    - Useful for identifying profiles of similar markets.
# ---------------------------------------------------------------




# ===============================================================
# Percentile Profile of Geomarkets by "visits per school"
# ---------------------------------------------------------------
# Goal:
#   Summarize the characteristics of geomarkets that sit at
#   key percentiles of n_vistot_per_sch_all (visits per school):
#   p10, p25, p50, p75, p90, p95, max.
#
# What this code does:
#   1) Computes percentile cutoffs on n_vistot_per_sch_all.
#   2) For each cutoff, selects the geomarket whose value is
#      closest to that cutoff (deterministic tie-break).
#   3) Returns a compact table with key characteristics.
#   4) (Optional) Produces a small figure showing the percentile
#      cutoffs and the chosen geomarkets.
#
# Notes:
#   - Uses explicit namespace calls (dplyr::, tibble::, etc.).
#   - Change `char_vars_to_show` to control which columns appear.
#   - Change `num_vars_to_show` to control which numeric features
#     are summarized in the output table.
#   - If you want *averages* at/near each percentile instead of a
#     single "closest" geomarket, replace the selection logic with
#     a small bandwidth window and summarize within that window.
# ===============================================================

# ---------------------------
# 0) Configuration
# ---------------------------
measure_col <- "n_vistot_per_sch_all"   # visits per school metric to rank on

# character / id columns to keep in the table
char_vars_to_show <- c("hs_eps_codename", "univ_abbrev", "univ_classification")

# numeric characteristics to display for each selected geomarket
num_vars_to_show <- c(
  "n_vistot_per_sch_all",
  "n_vis01_per_sch_all",
  "n_sch_all",
  "mean_inc_house",
  "med_inc_house",
  "pct_edu_baplus_all",
  "pct_hisp_all",
  "pct_nhisp_black",
  "pct_nhisp_api",
  "pct_pov_yes"
)

# percentiles of interest (named vector in display order)
pct_probs <- c(
  p10 = 0.10, p25 = 0.25, p50 = 0.50,
  p75 = 0.75, p90 = 0.90, p95 = 0.95, max = 1
)

# ---------------------------
# 1) Compute the percentile cutoffs
# ---------------------------
cutoffs <- stats::quantile(
  df_by_univ_eps[[measure_col]],
  probs = unname(pct_probs),
  na.rm = TRUE,
  names = FALSE
)

# keep as tibble for a clean join later
cut_tbl <- tibble::tibble(
  percentile = names(pct_probs),
  prob       = as.numeric(pct_probs),
  cutoff     = as.numeric(cutoffs)
)

# ---------------------------
# 2) For each cutoff, pick the geomarket closest to it
#    Deterministic tie-break:
#      - smallest absolute difference first
#      - then larger n_sch_all (if present)
#      - then alphabetic hs_eps_codename
# ---------------------------
pick_closest <- function(df, cutoff, measure = measure_col) {
  dplyr::arrange(
    df,
    dplyr::across(dplyr::all_of(measure), ~abs(.x - cutoff)),
    dplyr::desc(.data$n_sch_all),
    .data$hs_eps_codename
  ) |>
    dplyr::slice(1)
}

picked <- purrr::pmap_dfr(
  list(cut_tbl$percentile, cut_tbl$cutoff),
  function(pct_name, cutoff) {
    row <- pick_closest(df_by_univ_eps, cutoff, measure_col)
    row |>
      dplyr::mutate(
        percentile = pct_name,
        cutoff     = cutoff
      )
  }
)

# ---------------------------
# 3) Build the display table
# ---------------------------
# Ensure all requested columns exist; silently drop missing ones
cols_exist <- function(x, vars) vars[vars %in% colnames(x)]

out_cols <- c(
  "percentile", "cutoff",
  cols_exist(picked, char_vars_to_show),
  cols_exist(picked, num_vars_to_show)
)

percentile_profile_tbl <- picked |>
  dplyr::select(dplyr::all_of(out_cols)) |>
  # Optional formatting tweaks (rounding a few vars if present)
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c("n_vistot_per_sch_all", "n_vis01_per_sch_all")),
      ~round(.x, 3)
    ),
    dplyr::across(
      dplyr::any_of(c("mean_inc_house", "med_inc_house")),
      ~round(.x, 0)
    ),
    dplyr::across(
      dplyr::any_of(c("pct_edu_baplus_all", "pct_hisp_all", "pct_nhisp_black",
                      "pct_nhisp_api", "pct_pov_yes")),
      ~round(.x, 2)
    ),
    cutoff = round(cutoff, 3)
  )

# View the table in R (prints to console)
percentile_profile_tbl

# If knitting to a document and you want a nice table:
# knitr::kable(percentile_profile_tbl, digits = 3,
#              col.names = gsub("_", " ", names(percentile_profile_tbl), fixed = TRUE),
#              caption = "Geomarket characteristics at key percentiles of visits per school")

# ---------------------------
# 4) (Optional) Quick visualization
#    Shows the percentile cutoffs (x-axis is ordered labels)
# ---------------------------
# ggplot2::ggplot(percentile_profile_tbl,
#                 ggplot2::aes(x = factor(percentile, levels = names(pct_probs)),
#                              y = !!rlang::sym(measure_col))) +
#   ggplot2::geom_point() +
#   ggplot2::geom_line(group = 1) +
#   ggplot2::labs(
#     title = "Visits per School at Selected Percentiles",
#     x = "Percentile",
#     y = "Visits per School"
#   )

# ---------------------------
# 5) (Optional) Bandwidth alternative
#    Instead of picking the single closest geomarket, you can
#    compute an average within a small window around each cutoff:
#    - Define a window width (e.g., ±0.02 on the measure)
#    - Filter rows where |measure - cutoff| <= width
#    - Summarize means of the characteristics
#    This yields "typical" characteristics near that percentile.
# ---------------------------
# window_width <- 0.02
# pct_band_tbl <- purrr::pmap_dfr(
#   list(cut_tbl$percentile, cut_tbl$cutoff),
#   function(pct_name, cutoff) {
#     window_df <- df_by_univ_eps |>
#       dplyr::filter(abs(.data[[measure_col]] - cutoff) <= window_width)
#     if (nrow(window_df) == 0) return(NULL)
#     window_df |>
#       dplyr::summarise(
#         dplyr::across(dplyr::all_of(num_vars_to_show), ~mean(.x, na.rm = TRUE))
#       ) |>
#       dplyr::mutate(percentile = pct_name, cutoff = cutoff, .before = 1)
#   }
# )
# pct_band_tbl

