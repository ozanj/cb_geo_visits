################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits >
## [ FILE ] < recruiting_enrollment_combined.R >
## [ AUTH ] < Ozan Jaquette >
## [ DESC ] < Combined heatmaps: recruiting visits (left) + enrollment (right)
##            Figure 1: by student market segment (local/in_state/regional/national)
##            Figure 2: by EPS region
##            Appendix A: standalone recruiting heatmap (4 market segments)
##            Appendix B: standalone enrollment bar chart (in-state/OOS/intl)  >
################################################################################
options(max.print = 1000)
library(tidyverse)
library(forcats)
library(scales)
library(patchwork)

####### SOURCE DATA SCRIPTS
getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()
source(file = file.path('scripts', 'create_univ_geo_df.R'))
getwd()
rm(create_rq1_map, format_vars, get_palette)

load(file = file.path('.', 'data', 'ipeds_migration', 'ipeds_migration_non_collapse_1617'))
ipeds_migration_non_collapse_1617 <- ipeds_migration_non_collapse_1617 %>%
  left_join(
    univ_df %>% select(univ_id, univ_classification),
    by = c('unitid' = 'univ_id')
  )

# ============================================================================ #
#   STEP 1: Shared university ordering (classification → region → rank → name) #
# ============================================================================ #
class_order  <- c("private_libarts", "private_national", "public_research")
region_order <- c("new_england", "middle_states", "midwest", "south", "southwest", "west")

univ_order <- univ_df %>%
  mutate(
    univ_classification = factor(univ_classification, levels = class_order),
    univ_eps_region     = factor(as.character(univ_eps_region), levels = region_order)
  ) %>%
  arrange(univ_classification, univ_eps_region, univ_usnwr_rank, univ_abbrev) %>%
  pull(univ_abbrev)

# ============================================================================ #
#   STEP 2: State → EPS region lookup                                          #
# ============================================================================ #
state_to_region <- c(
  CT = "new_england",   ME = "new_england",   MA = "new_england",
  NH = "new_england",   RI = "new_england",   VT = "new_england",
  NY = "middle_states", PA = "middle_states", DE = "middle_states",
  DC = "middle_states", MD = "middle_states", NJ = "middle_states",
  IL = "midwest",       IN = "midwest",       IA = "midwest",
  KS = "midwest",       MI = "midwest",       MN = "midwest",
  MO = "midwest",       NE = "midwest",       ND = "midwest",
  OH = "midwest",       SD = "midwest",       WV = "midwest",  WI = "midwest",
  AL = "south",         FL = "south",         GA = "south",
  KY = "south",         LA = "south",         MS = "south",
  NC = "south",         SC = "south",         TN = "south",    VA = "south",
  AR = "southwest",     NM = "southwest",     OK = "southwest", TX = "southwest",
  AK = "west",          AZ = "west",          CA = "west",
  CO = "west",          HI = "west",          ID = "west",
  MT = "west",          NV = "west",          OR = "west",
  UT = "west",          WA = "west",          WY = "west"
)

# FIX: exclude freshhs_us — "us" matches [a-z]{2} and is not a state
state_cols <- grep("^freshhs_[a-z]{2}$", names(ipeds_migration_non_collapse_1617), value = TRUE)
state_cols <- setdiff(state_cols, "freshhs_us")

# ============================================================================ #
#   STEP 3: Build enrollment data — by market segment                          #
# ============================================================================ #
ipeds_market_seg <- ipeds_migration_non_collapse_1617 %>%
  select(unitid, univ_abbrev, univ_state_code, univ_eps_region,
         univ_classification, freshhs_us, all_of(state_cols)) %>%
  tidyr::pivot_longer(
    cols      = all_of(state_cols),
    names_to  = "state_col",
    values_to = "enrollment"
  ) %>%
  mutate(
    hs_state_code  = toupper(sub("freshhs_", "", state_col)),
    hs_eps_region  = state_to_region[hs_state_code],
    market_segment = case_when(
      hs_state_code == univ_state_code                                           ~ "in_state",
      hs_eps_region == as.character(univ_eps_region) &
        hs_state_code != univ_state_code                                         ~ "regional",
      !is.na(hs_eps_region) &
        hs_eps_region != as.character(univ_eps_region)                           ~ "national",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(market_segment)) %>%
  group_by(unitid, univ_abbrev, univ_classification, univ_eps_region,
           freshhs_us, market_segment) %>%
  summarise(enrollment = sum(enrollment, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    pct_us         = enrollment / freshhs_us * 100,
    market_segment = factor(market_segment, levels = c("in_state", "regional", "national")),
    univ_abbrev    = factor(univ_abbrev, levels = rev(univ_order))
  )

ipeds_market_seg %>% glimpse()

# ============================================================================ #
#   STEP 4: Build enrollment data — by EPS region                              #
# ============================================================================ #
ipeds_region_seg <- ipeds_migration_non_collapse_1617 %>%
  select(unitid, univ_abbrev, univ_classification, univ_eps_region,
         freshhs_us, all_of(state_cols)) %>%
  tidyr::pivot_longer(
    cols      = all_of(state_cols),
    names_to  = "state_col",
    values_to = "enrollment"
  ) %>%
  mutate(
    hs_state_code = toupper(sub("freshhs_", "", state_col)),
    hs_eps_region = state_to_region[hs_state_code]
  ) %>%
  filter(!is.na(hs_eps_region)) %>%
  group_by(unitid, univ_abbrev, univ_classification, univ_eps_region,
           freshhs_us, hs_eps_region) %>%
  summarise(enrollment = sum(enrollment, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    pct_us        = enrollment / freshhs_us * 100,
    hs_eps_region = factor(hs_eps_region, levels = region_order),
    univ_abbrev   = factor(univ_abbrev, levels = rev(univ_order))
  )

ipeds_region_seg %>% glimpse()

# ============================================================================ #
#   STEP 5: Prepare recruiting data — by market segment                        #
#           Combine local + in_state → "local/in-state" for main figures       #
# ============================================================================ #
vis_long_market <- summarize_visits(pubprivhs_univ_df, by = hs_univ_market)

market_order <- c("local/in-state", "regional", "national")

vis_recruit_market <- vis_long_market %>%
  filter(ctrl == "all") %>%
  mutate(
    hs_univ_market = case_when(
      hs_univ_market %in% c("local", "in_state") ~ "local/in-state",
      TRUE ~ as.character(hs_univ_market)
    )
  ) %>%
  group_by(univ_abbrev, hs_univ_market) %>%
  summarise(n_vis = sum(n_vis, na.rm = TRUE), .groups = "drop") %>%
  group_by(univ_abbrev) %>%
  mutate(pcol_vis = n_vis / sum(n_vis, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(
    univ_abbrev    = factor(univ_abbrev, levels = rev(univ_order)),
    hs_univ_market = factor(hs_univ_market, levels = market_order)
  )

recruit_market_label_map <- vis_recruit_market %>%
  group_by(univ_abbrev) %>%
  summarise(total_vis = sum(n_vis, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    univ_label = paste0(as.character(univ_abbrev), " (",
                        scales::comma(total_vis, accuracy = 1), ")")
  )

recruit_market_label_levels <- recruit_market_label_map %>%
  arrange(univ_abbrev) %>%
  pull(univ_label)

vis_recruit_market <- vis_recruit_market %>%
  left_join(recruit_market_label_map, by = "univ_abbrev") %>%
  mutate(univ_label = factor(univ_label, levels = recruit_market_label_levels))

# ============================================================================ #
#   STEP 6: Prepare recruiting data — by EPS region                            #
# ============================================================================ #
vis_long_region <- summarize_visits(pubprivhs_univ_df, by = hs_eps_region)

vis_recruit_region <- vis_long_region %>%
  filter(ctrl == "all") %>%
  mutate(
    univ_abbrev   = factor(univ_abbrev, levels = rev(univ_order)),
    hs_eps_region = factor(hs_eps_region, levels = region_order)
  )

recruit_region_label_map <- vis_recruit_region %>%
  group_by(univ_abbrev) %>%
  summarise(total_vis = sum(n_vis, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    univ_label = paste0(as.character(univ_abbrev), " (",
                        scales::comma(total_vis, accuracy = 1), ")")
  )

recruit_region_label_levels <- recruit_region_label_map %>%
  arrange(univ_abbrev) %>%
  pull(univ_label)

vis_recruit_region <- vis_recruit_region %>%
  left_join(recruit_region_label_map, by = "univ_abbrev") %>%
  mutate(univ_label = factor(univ_label, levels = recruit_region_label_levels))

# ============================================================================ #
#   STEP 7: Enrollment y-axis labels                                           #
#           Recruiting panel:  "UnivAbbrev (N,visits)"                         #
#           Enrollment panel:  "(N,enrollment)" via scale_y_discrete(labels)   #
# ============================================================================ #
# Step 7a: enroll_label_map — freshhs_us dropped after use to avoid .x/.y clash
enroll_label_map <- ipeds_market_seg %>%
  distinct(univ_abbrev, freshhs_us) %>%
  mutate(
    enroll_label = paste0(as.character(univ_abbrev), " (",
                          scales::comma(freshhs_us, accuracy = 1), ")")
  ) %>%
  select(univ_abbrev, enroll_label)

enroll_label_levels <- enroll_label_map %>%
  arrange(univ_abbrev) %>%
  pull(enroll_label)

# Step 7b: enroll_n_display built independently so freshhs_us is still available
enroll_n_display <- ipeds_market_seg %>%
  distinct(univ_abbrev, freshhs_us) %>%
  mutate(
    enroll_label = paste0(as.character(univ_abbrev), " (",
                          scales::comma(freshhs_us, accuracy = 1), ")"),
    display      = paste0("(", scales::comma(freshhs_us, accuracy = 1), ")")
  ) %>%
  select(enroll_label, display) %>%
  tibble::deframe()

# Step 7c: join enroll_label onto both enrollment data frames
ipeds_market_seg <- ipeds_market_seg %>%
  left_join(enroll_label_map, by = "univ_abbrev") %>%
  mutate(enroll_label = factor(enroll_label, levels = enroll_label_levels))

ipeds_region_seg <- ipeds_region_seg %>%
  left_join(enroll_label_map, by = "univ_abbrev") %>%
  mutate(enroll_label = factor(enroll_label, levels = enroll_label_levels))

# ============================================================================ #
#   STEP 8: Shared fill scale factory + shared theme                           #
# ============================================================================ #
make_fill_scale <- function(legend_title = "Share (%)") {
  ggplot2::scale_fill_gradient(
    name   = legend_title,
    limits = c(0, 100),
    low    = "white",
    high   = "#B2182B",
    breaks = c(0, 20, 40, 60, 80, 100),
    labels = function(x) paste0(x, "%")
  )
}

heatmap_theme <- function(show_y_labels = TRUE) {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(size = 10, face = "bold",
                                           hjust = 0.5, margin = ggplot2::margin(b = 4)),
      axis.text.x  = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
      axis.text.y  = if (show_y_labels) ggplot2::element_text(size = 8) else ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank(),
      plot.margin  = ggplot2::margin(t = 2, r = 2, b = 2, l = 2)
    )
}

# ============================================================================ #
#   FIGURE 1: Market segment — recruiting (left) + enrollment (right)          #
# ============================================================================ #
p_recruit_market <- ggplot2::ggplot(
  vis_recruit_market,
  ggplot2::aes(x = hs_univ_market, y = univ_label, fill = pcol_vis)
) +
  ggplot2::geom_tile(width = 0.95, height = 0.95) +
  make_fill_scale() +
  ggplot2::scale_x_discrete(
    position = "top",
    expand   = c(0, 0),
    labels   = c("local/in-state" = "Local/In-state",
                 "regional"       = "Regional",
                 "national"       = "National")
  ) +
  ggplot2::labs(
    title = "Share of recruiting visits\nby market segment",
    x = NULL, y = NULL
  ) +
  heatmap_theme(show_y_labels = TRUE)

p_recruit_market

p_enroll_market <- ggplot2::ggplot(
  ipeds_market_seg,
  ggplot2::aes(x = market_segment, y = enroll_label, fill = pct_us)
) +
  ggplot2::geom_tile(width = 0.95, height = 0.95) +
  make_fill_scale() +
  ggplot2::scale_x_discrete(
    position = "top",
    expand   = c(0, 0),
    labels   = c("in_state"  = "In-state",
                 "regional"  = "Regional",
                 "national"  = "National")
  ) +
  ggplot2::scale_y_discrete(labels = enroll_n_display) +
  ggplot2::labs(
    title = "Share of U.S. freshman enrollment\nby market segment",
    x = NULL, y = NULL
  ) +
  heatmap_theme(show_y_labels = TRUE)

p_enroll_market

combined_recruit_enroll_market <-
  p_recruit_market + patchwork::plot_spacer() + p_enroll_market +
  patchwork::plot_layout(ncol = 3, widths = c(1.1, 0.08, 1.0), guides = "collect") &
  ggplot2::theme(legend.position = "right")

combined_recruit_enroll_market

ggplot2::ggsave(
  filename = file.path('results', 'recruiting_enrollment_market_combined.pdf'),
  plot     = combined_recruit_enroll_market,
  width    = 14,
  height   = 8.5
)

# ============================================================================ #
#   FIGURE 2: EPS region — recruiting (left) + enrollment (right)              #
# ============================================================================ #
p_recruit_region <- ggplot2::ggplot(
  vis_recruit_region,
  ggplot2::aes(x = hs_eps_region, y = univ_label, fill = pcol_vis)
) +
  ggplot2::geom_tile(width = 0.95, height = 0.95) +
  make_fill_scale() +
  ggplot2::scale_x_discrete(
    position = "top",
    expand   = c(0, 0),
    labels   = c("new_england"   = "New England",
                 "middle_states" = "Middle States",
                 "midwest"       = "Midwest",
                 "south"         = "South",
                 "southwest"     = "Southwest",
                 "west"          = "West")
  ) +
  ggplot2::labs(
    title = "Share of recruiting visits\nby EPS region",
    x = NULL, y = NULL
  ) +
  heatmap_theme(show_y_labels = TRUE)

p_recruit_region

p_enroll_region <- ggplot2::ggplot(
  ipeds_region_seg,
  ggplot2::aes(x = hs_eps_region, y = enroll_label, fill = pct_us)
) +
  ggplot2::geom_tile(width = 0.95, height = 0.95) +
  make_fill_scale() +
  ggplot2::scale_x_discrete(
    position = "top",
    expand   = c(0, 0),
    labels   = c("new_england"   = "New England",
                 "middle_states" = "Middle States",
                 "midwest"       = "Midwest",
                 "south"         = "South",
                 "southwest"     = "Southwest",
                 "west"          = "West")
  ) +
  ggplot2::scale_y_discrete(labels = enroll_n_display) +
  ggplot2::labs(
    title = "Share of U.S. freshman enrollment\nby EPS region",
    x = NULL, y = NULL
  ) +
  heatmap_theme(show_y_labels = TRUE)

p_enroll_region

combined_recruit_enroll_region <-
  p_recruit_region + patchwork::plot_spacer() + p_enroll_region +
  patchwork::plot_layout(ncol = 3, widths = c(1.1, 0.08, 1.0), guides = "collect") &
  ggplot2::theme(legend.position = "right")

combined_recruit_enroll_region

ggplot2::ggsave(
  filename = file.path('results', 'recruiting_enrollment_region_combined.pdf'),
  plot     = combined_recruit_enroll_region,
  width    = 16,
  height   = 8.5
)

# ============================================================================ #
#   APPENDIX FIGURE A: Standalone recruiting heatmap — all 4 market segments   #
#   (local and in-state kept separate; uses vis_long_market from Step 5)        #
# ============================================================================ #
market_order_4 <- c("local", "in_state", "regional", "national")

vis_recruit_market_app <- vis_long_market %>%
  filter(ctrl == "all") %>%
  mutate(
    univ_abbrev    = factor(univ_abbrev, levels = rev(univ_order)),
    hs_univ_market = factor(hs_univ_market, levels = market_order_4)
  )

recruit_app_label_map <- vis_recruit_market_app %>%
  group_by(univ_abbrev) %>%
  summarise(total_vis = sum(n_vis, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    univ_label = paste0(as.character(univ_abbrev), " (",
                        scales::comma(total_vis, accuracy = 1), ")")
  )

recruit_app_label_levels <- recruit_app_label_map %>%
  arrange(univ_abbrev) %>%
  pull(univ_label)

vis_recruit_market_app <- vis_recruit_market_app %>%
  left_join(recruit_app_label_map, by = "univ_abbrev") %>%
  mutate(univ_label = factor(univ_label, levels = recruit_app_label_levels))

p_recruit_app <- ggplot2::ggplot(
  vis_recruit_market_app,
  ggplot2::aes(x = hs_univ_market, y = univ_label, fill = pcol_vis)
) +
  ggplot2::geom_tile(width = 0.95, height = 0.95) +
  make_fill_scale("Share of visits (%)") +
  ggplot2::scale_x_discrete(
    position = "top",
    expand   = c(0, 0),
    labels   = c("local"    = "Local",
                 "in_state" = "In-state",
                 "regional" = "Regional",
                 "national" = "National")
  ) +
  ggplot2::labs(
    title = "Share of recruiting visits by market segment",
    x = NULL, y = NULL
  ) +
  heatmap_theme(show_y_labels = TRUE)

p_recruit_app

ggplot2::ggsave(
  filename = file.path('results', 'recruiting_heatmap_market_appendix.pdf'),
  plot     = p_recruit_app,
  width    = 9,
  height   = 8.5
)

# ============================================================================ #
#   APPENDIX FIGURE B: Standalone enrollment bar chart                         #
#                       In-state / Out-of-state / International                #
#   Uses pre-computed columns in ipeds_migration_non_collapse_1617:            #
#     freshhs_inst    = in-state freshman enrollment                            #
#     freshhs_usoutst = out-of-state U.S. freshman enrollment                  #
#     freshhs_for     = international (foreign) freshman enrollment             #
#     freshhs_tot     = total freshman enrollment (sum of above three)          #
# ============================================================================ #
enroll_bar_df <- ipeds_migration_non_collapse_1617 %>%
  select(unitid, univ_abbrev, univ_classification,
         freshhs_tot, freshhs_inst, freshhs_usoutst, freshhs_for) %>%
  tidyr::pivot_longer(
    cols      = c(freshhs_inst, freshhs_usoutst, freshhs_for),
    names_to  = "category",
    values_to = "enrollment"
  ) %>%
  mutate(
    pct_total   = enrollment / freshhs_tot * 100,
    # FIX: factor levels reversed so ggplot2 stacks International leftmost;
    # ggplot2 fills horizontal bars right-to-left in factor order
    category    = factor(category,
                         levels = c("freshhs_inst", "freshhs_usoutst", "freshhs_for"),
                         labels = c("In-state", "Out-of-state", "International")),
    univ_abbrev = factor(univ_abbrev, levels = rev(univ_order))
  )

# y-axis labels: "UnivAbbrev (N,total)" — total includes international
enroll_bar_label_map <- enroll_bar_df %>%
  distinct(univ_abbrev, freshhs_tot) %>%
  mutate(
    bar_label = paste0(as.character(univ_abbrev), " (",
                       scales::comma(freshhs_tot, accuracy = 1), ")")
  ) %>%
  select(univ_abbrev, bar_label)

bar_label_levels <- enroll_bar_label_map %>%
  arrange(univ_abbrev) %>%
  pull(bar_label)

enroll_bar_df <- enroll_bar_df %>%
  left_join(enroll_bar_label_map, by = "univ_abbrev") %>%
  mutate(bar_label = factor(bar_label, levels = bar_label_levels))

# Colors matched to labels (not factor order)
enroll_bar_colors <- c(
  "In-state"      = "#66C2A5",
  "Out-of-state"  = "#FC8D62",
  "International" = "#8DA0CB"
)

p_enroll_bar <- ggplot2::ggplot(
  enroll_bar_df,
  ggplot2::aes(x = pct_total, y = bar_label, fill = category)
) +
  ggplot2::geom_col(width = 0.8, position = "stack") +
  ggplot2::scale_fill_manual(values = enroll_bar_colors, name = NULL) +
  # FIX: reverse = TRUE so legend reads International, Out-of-state, In-state
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
  ggplot2::scale_x_continuous(
    limits = c(0, 101),
    labels = function(x) paste0(x, "%"),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    axis.text.x        = ggplot2::element_text(size = 8),
    axis.text.y        = ggplot2::element_text(size = 8),
    axis.ticks.y       = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = "grey90"),
    panel.grid.minor   = ggplot2::element_blank(),
    legend.position    = "top",
    plot.margin        = ggplot2::margin(t = 2, r = 10, b = 2, l = 2)
  )

p_enroll_bar

ggplot2::ggsave(
  filename = file.path('results', 'enrollment_bar_residency_appendix.pdf'),
  plot     = p_enroll_bar,
  width    = 8,
  height   = 8.5
)