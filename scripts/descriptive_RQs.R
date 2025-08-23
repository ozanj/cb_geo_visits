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
  
  library(dplyr)
  library(tidyr)
  
  library(dplyr)
  library(tidyr)
  library(forcats)
  
  summarize_visits <- function(
    df,
    id = univ_id,           # university id column in df
    by = hs_eps_region,     # region/market/etc. column in df
    control = hs_control,   # 'public'/'private'
    visit = visit01,        # 0/1 visit indicator
    keep_wide = FALSE,      # TRUE returns wide per-control columns
    digits = 1,             # rounding for percents
    include_index = TRUE    # adds exposure & over/under-index
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
        ctrl = fct_drop({{ control }})
      ) %>%
      group_by({{ id }}, {{ by }}, ctrl) %>%
      summarize(
        n_sch = n(),
        n_vis = sum({{ visit }} == 1),
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
      mutate(ctrl = fct_relevel(factor("all"), "all"))
    
    long <- bind_rows(base, all_ctrl) %>%
      arrange({{ id }}, {{ by }}, ctrl) %>%
      group_by({{ id }}, {{ by }}, ctrl) %>%
      mutate(prow_vis = 100 * n_vis / n_sch) %>%
      ungroup() %>%
      group_by({{ id }}, ctrl) %>%
      mutate(pcol_vis = 100 * n_vis / sum(n_vis)) %>%
      ungroup()
    
    if (include_index) {
      long <- long %>%
        group_by({{ id }}, ctrl) %>%
        mutate(
          exposure    = n_sch / sum(n_sch),
          over_index  = (pcol_vis / 100) - exposure,
          ratio_index = (pcol_vis / 100) / exposure
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
      pivot_wider(
        names_from  = ctrl,
        values_from = c(n_sch, n_vis, prow_vis, pcol_vis),
        names_sep   = "_"
      ) %>%
      arrange(univ_classification, is.na(univ_usnwr_rank), univ_usnwr_rank)
    
    out_wide
  }
  
  # HEATMAP

  vis_long %>% 
    filter(ctrl == "all") %>%
    ggplot(aes(
      x = hs_eps_region,
      y = fct_rev(reorder(univ_abbrev, univ_usnwr_rank, na.last = TRUE)),
      fill = prow_vis
    )) +
    geom_tile() +
    scale_fill_viridis_c(name = "% visited", labels = percent_format(1)) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(position = "top") +  # move region labels to the top
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank()
    )
  
  vis_long %>%
    filter(ctrl %in% c("all", "public", "private")) %>%
    mutate(ctrl = factor(ctrl, levels = c("all", "public", "private"))) %>%
    ggplot(aes(
      x = hs_eps_region,
      y = fct_rev(reorder(univ_abbrev, univ_usnwr_rank, na.last = TRUE)),
      fill = prow_vis
    )) +
    geom_tile() +
    facet_grid(. ~ ctrl, space = "free_x", scales = "free_x", switch = "y") +  # repeat y labels
    scale_fill_viridis_c(name = "% visited", labels = percent_format(1)) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(position = "top") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
      axis.text.y = element_text(size = 8),  # y labels now show on each facet
      strip.text.x = element_text(size = 9, face = "bold"),
      panel.grid = element_blank(),
      panel.spacing.x = unit(0.2, "lines")
    )
  

###########
########### HEAT WAVE
########### 
  
plot_visit_heatmaps <- function(vis_long, include_all = TRUE) {
  # --- 1) Consistent ordering for axes (ranked within classification) ---
  y_order <- vis_long %>%
    dplyr::distinct(univ_abbrev, univ_usnwr_rank, univ_classification) %>%
    dplyr::arrange(univ_classification, univ_usnwr_rank, univ_abbrev) %>%
    dplyr::pull(univ_abbrev)

  x_order <- levels(vis_long$hs_eps_region)

  # --- 2) Filter data based on include_all flag ---
  ctrl_levels <- if (include_all) c("all", "public", "private") else c("public", "private")

  vis_plot <- vis_long %>%
    dplyr::filter(ctrl %in% ctrl_levels) %>%
    dplyr::mutate(
      ctrl          = factor(ctrl, levels = ctrl_levels),
      univ_abbrev   = factor(univ_abbrev, levels = rev(y_order)),  # top = best rank (Williams first)
      hs_eps_region = factor(hs_eps_region, levels = x_order)
    )

  # --- 3) Global limits for shared color scale ---
  fill_limits <- range(vis_plot$prow_vis, na.rm = TRUE)

  # --- 4) Panel builder with facet-specific y-labels "Name (Nvis)" ---
  panel <- function(df, title, y_levels_desc) {
    # build labels per facet: total visits per university in this ctrl
    lab_map <- df %>%
      dplyr::group_by(univ_abbrev) %>%
      dplyr::summarise(total_vis = sum(n_vis, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(univ_label = paste0(as.character(univ_abbrev), " (", total_vis, ")"))

    # order label levels to match the global y ordering (descending)
    label_levels_desc <- tibble::tibble(univ_abbrev = y_levels_desc) %>%
      dplyr::left_join(lab_map, by = "univ_abbrev") %>%
      dplyr::pull(univ_label)

    df2 <- df %>%
      dplyr::left_join(lab_map, by = "univ_abbrev") %>%
      dplyr::mutate(univ_label = factor(univ_label, levels = label_levels_desc))

    ggplot2::ggplot(df2,
      ggplot2::aes(x = hs_eps_region, y = univ_label, fill = prow_vis)
    ) +
      ggplot2::geom_tile(width = 0.95, height = 0.95) +
      ggplot2::scale_fill_viridis_c(
        name   = "% visited",
        labels = scales::percent_format(1),
        limits = fill_limits
      ) +
      ggplot2::labs(title = title, x = NULL, y = NULL) +
      ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 4)),
        axis.text.x  = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
        axis.text.y  = ggplot2::element_text(size = 8),
        panel.grid   = ggplot2::element_blank(),
        plot.margin  = ggplot2::margin(t = 2, r = 2, b = 2, l = 2)
      )
  }

  y_levels_desc <- levels(vis_plot$univ_abbrev)  # already reversed for top-first

  panels <- list()
  if (include_all) panels <- c(panels, list(panel(dplyr::filter(vis_plot, ctrl == "all"),    "All",    y_levels_desc)))
  panels <- c(panels,
    list(
      panel(dplyr::filter(vis_plot, ctrl == "public"), "Public",  y_levels_desc),
      panel(dplyr::filter(vis_plot, ctrl == "private"), "Private", y_levels_desc)
    )
  )

  patchwork::wrap_plots(panels, ncol = length(panels), widths = rep(1, length(panels))) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(
      legend.position = "right",
      panel.spacing.x = grid::unit(0.15, "lines")
    )
}

# ==== Example usage ====
 vis_long <- summarize_visits(pubprivhs_univ_df, by = hs_eps_region)

# With All + Public + Private (college labels show counts per respective panel)
 plot_visit_heatmaps(vis_long, include_all = TRUE)

# Only Public + Private
plot_visit_heatmaps(vis_long, include_all = FALSE)


#############
#############
#############

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







# B) Market on X [HEATMAP IS WORKING CORRECTLY, BUT I DON'T THINK EITHER THE CURRENT PERCENT OR COUNT GRAPHS ARE CONVEYING WHAT WE WANT TO CONVEY]
plot_visit_heatmaps(vis_long_market, by = hs_univ_market, include_all = TRUE,  metric = "percent")
plot_visit_heatmaps(vis_long_market, by = hs_univ_market, include_all = FALSE, metric = "count")




# next step: revise the above code to be able to handle a differen by variabhle (e.g., hs_univ_market)

# 

# 1) HEAT MAP (matrix) — what you're using now
#    - Show % visited (prow_vis) as fill across X = region/market, Y = college (ordered by rank).
#    - Facet columns for control: All | Public | Private.
#    - geom_tile() with a single-ended gradient (0 = white → high = red).
#    - Good for scanning patterns across many colleges at once.

# 2) CLEVELAND DOT PLOT (a.k.a. dot chart)
#    - For a chosen region/market, plot colleges on Y and % visited on X.
#    - Option A: one dot per control (All/Public/Private) with position_dodge.
#    - Option B: lollipop style (geom_segment + geom_point) for readability.
#    - Great when you want precise comparisons within a single region/market.

# 3) DUMBBELL / SLOPE COMPARISON (Public vs Private)
#    - For each college (per region/market), connect % visited for Public vs Private.
#    - Use ggalt::geom_dumbbell() or geom_segment + two geom_point layers.
#    - Highlights gaps between sectors at a glance.

# 4) BUMP CHART (ranking across regions/markets)
#    - Rank colleges by % visited within each region/market; X = region/market, Y = rank.
#    - Lines per college (group = college), use ggbump::geom_bump().
#    - Emphasizes how a college’s relative standing changes across geographies.

# 5) RIDGELINE (distributional view)
#    - Show the distribution of college-level % visited across regions (or vice versa).
#    - ggridges::geom_density_ridges(), one ridge per region or classification.
#    - Useful to compare spread and skew (e.g., some regions have many low-visit cells).

# 6) TREEMAP (share composition)
#    - For each college (facet_wrap), area = visits or share of visits (pcol_vis) by region/market.
#    - treemapify::geom_treemap() + geom_treemap_text() (fill by region/market).
#    - Answers: “Where do this college’s visits come from?” in one compact block.

# Bonus) ALLUVIAL / SANKEY (flow between categories)
#    - ggalluvial: flows from region → control (Public/Private) → (optionally) classification.
#    - Best for showing how visit counts split and recombine across categorical stages.
