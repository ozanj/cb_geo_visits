
pubprivhs_df %>% glimpse()

pubprivhs_df %>% as_tibble() %>% count(hs_control)


pubprivhs_df %>% as_tibble() %>% count(hs_control,hs_school_type)

# for publics, which schools have non-missing data on student achievement
#$ hs_num_took_math                  <dbl> 303, 83, 162, 119, 101, 712, 413, 410, 374, 106, 183, 332, 202, 121, 124, 206, 225, 212, 225, 44, 122, 440, 206, 595, 63, 153, 520, 23, 2…
#$ hs_num_prof_math                  <dbl> 51.510, 6.225, 19.440, 26.180, 22.220, 284.800, 194.110, 213.200, 183.260, 18.020, 31.110, 139.440, 54.540, 32.670, 3.100, 76.220, 60.750…
#$ hs_num_took_rla                   <dbl> 302, 83, 162, 119, 101, 712, 412, 410, 375, 106, 183, 332, 202, 121, 125, 206, 225, 212, 225, 44, 122, 440, 206, 596, 63, 153, 520, 23, 2…
#$ hs_num_prof_rla                   <dbl> 163.080, 39.010, 84.240, 85.680, 57.570, 534.000, 329.600, 332.100, 292.500, 76.320, 95.160, 272.240, 125.240, 68.970, 40.000, 158.620, 1…
#$ hs_pct_prof_math                  <dbl> 0.170, 0.075, 0.120, 0.220, 0.220, 0.400, 0.470, 0.520, 0.490, 0.170, 0.170, 0.420, 0.270, 0.270, 0.025, 0.370, 0.270, 0.120, 0.220, 0.15…
#$ hs_pct_prof_rla                   <dbl> 0.540, 0.470

pubprivhs_df %>%
  as_tibble() %>%
  filter(hs_control == "public") %>%
  mutate(has_test_data =
           !is.na(hs_pct_prof_math) |
           !is.na(hs_pct_prof_rla)) %>%
  group_by(hs_school_type) %>%
  summarise(
    n_schools = n(),
    n_with_data = sum(has_test_data),
    pct_with_data = mean(has_test_data)
  ) %>%
  arrange(desc(pct_with_data))

# ---------------------------------------------------------
# Diagnostic: Which public school types have achievement data?
#
# Results:
# regular school                ~94% have test data
# career & technical school     ~86% have test data
# alternative education school  ~50% have test data
#
# Interpretation:
# - Regular public schools almost always report test scores.
# - Career & technical schools mostly report scores as well.
# - Alternative education schools frequently lack standardized test data.
#
# Recommendation for analyses that use achievement variables:
# - Include: regular schools and career & technical schools
# - Exclude: alternative education schools
#
# Rationale:
# About half of alternative schools lack test score data, which would cause
# large numbers of observations to be dropped automatically in regressions
# that include achievement controls. Restricting to regular and CTE schools
# keeps the analytic sample stable across model specifications while
# retaining the vast majority of public schools.
# ---------------------------------------------------------

pubprivhs_df %>%
  as_tibble() %>%
  filter(hs_control == "public") %>%
  filter(hs_school_type %in% c("regular school", "career and technical school"))

events_df %>% glimpse()

# =========================================================
# QUICK-AND-DIRTY SCHOOL-LEVEL VISIT SUMMARY
#
# Goal:
# events_df has many rows per school because each row is an event.
# To study how visit rates differ across school types, first collapse
# visits to the school level, then merge onto the school dataframe.
#
# Recommended school-level outcomes:
# 1) ever_visited      = did the school receive any visit?
# 2) n_visits          = total number of visit events
# 3) n_univs_visited   = number of distinct colleges visiting the school
#
# For a simple descriptive comparison across school types, n_univs_visited
# is often the cleanest measure because it is not inflated by a college
# visiting the same school multiple times.
# =========================================================

visits_by_school <- events_df %>%
  as_tibble() %>%
  group_by(school_id) %>%
  summarise(
    n_visits = n(),
    n_univs_visited = n_distinct(univ_id),
    ever_visited = 1L,
    .groups = "drop"
  )

# Merge onto school dataframe
pubprivhs_visits_df <- pubprivhs_df %>%
  as_tibble() %>%
  left_join(visits_by_school, by = c("hs_ncessch" = "school_id")) %>%
  mutate(
    ever_visited    = if_else(is.na(ever_visited), 0L, ever_visited),
    n_visits        = if_else(is.na(n_visits), 0L, n_visits),
    n_univs_visited = if_else(is.na(n_univs_visited), 0L, n_univs_visited)
  )

# =========================================================
# QUICK DESCRIPTIVES: visit rates by control and school type
# =========================================================

pubprivhs_visits_df %>%
  count(hs_control, hs_school_type)

pubprivhs_visits_df %>%
  group_by(hs_control, hs_school_type) %>%
  summarise(
    # number of schools in category
    n_schools = n(),
    
    # share of schools receiving at least one visit
    pct_any_visit = mean(ever_visited),
    
    # average number of visit events across all schools (including zeros)
    mean_n_visits = mean(n_visits),
    
    # average number of distinct universities visiting (including zeros)
    mean_n_univs_visited = mean(n_univs_visited),
    
    # median number of universities visiting (including zeros)
    median_n_univs_visited = median(n_univs_visited),
    
    # ---- conditional intensity among visited schools ----
    mean_univs_when_visited =
      mean(n_univs_visited[ever_visited == 1], na.rm = TRUE),
    
    median_univs_when_visited =
      median(n_univs_visited[ever_visited == 1], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(hs_control, desc(pct_any_visit))


# ---------------------------------------------------------
# Excluding alternative education schools from analysis
#
# Descriptive diagnostics show that alternative schools receive
# very few recruiting visits relative to other school types.
#
# In the visit data:
# - Only ~5% of public alternative schools receive any visit
# - Only ~15% of private alternative schools receive any visit
#
# In addition, roughly half of public alternative schools lack
# standardized test score data, which would cause many schools
# to be dropped automatically in regressions including
# achievement controls.
#
# Because alternative schools are both weakly connected to the
# recruiting market and have substantial missing achievement
# data, they are excluded from the main analytic sample.

4. Alternative schools clearly sit outside the market

Look at visit probabilities:
  
  Type	Visit probability
Public alternative	4.8%
Private alternative	15.0%

These schools are basically not part of the recruiting pipeline.

Your instinct to drop them is well justified.
#
# The analysis therefore focuses on school types that represent
# the primary recruiting pipeline: regular schools and
# career/technical schools (public and private).
# ---------------------------------------------------------


> events_df %>% count(event_type)
# A tibble: 2 × 2
event_type     n
<chr>      <int>
1 priv_hs    13492
2 pub_hs     24369


make_rq2_fit_table <- function(tab_df) {
  
  fit_rows_num <- c(
    "N",
    "Parameters (incl. FE)",
    "Residual df",
    "R²",
    "Adj. R²",
    "Within R²",
    "RMSE"
  )
  
  fmt_int_rows <- c("N", "Parameters (incl. FE)", "Residual df")
  fmt_dec_rows <- c("R²", "Adj. R²", "Within R²", "RMSE")
  
  # highlight colors
  eps_col_bg  <- "#EEF4FF"   # light blue-gray
  adjr2_row_bg <- "#FFF4CC"  # light warm yellow
  
  # -----------------------------
  # Build numeric blocks first
  # -----------------------------
  tab_fe <- tab_df %>%
    filter(term %in% fit_rows_num) %>%
    select(term, 2:5) %>%
    setNames(c("term", "c1", "c2", "c3", "c4"))
  
  tab_cov <- tab_df %>%
    filter(term %in% fit_rows_num) %>%
    select(term, 6:9) %>%
    setNames(c("term", "c1", "c2", "c3", "c4"))
  
  # -----------------------------
  # Format numeric rows first
  # -----------------------------
  fmt_block <- function(df) {
    df %>%
      mutate(
        across(
          c(c1, c2, c3, c4),
          ~ dplyr::case_when(
            term %in% fmt_int_rows ~
              format(round(as.numeric(.), 0), big.mark = ",", scientific = FALSE, trim = TRUE),
            term %in% fmt_dec_rows ~
              sprintf("%.3f", as.numeric(.)),
            TRUE ~ as.character(.)
          )
        )
      ) %>%
      mutate(across(c(c1, c2, c3, c4), as.character))
  }
  
  tab_fe  <- fmt_block(tab_fe)
  tab_cov <- fmt_block(tab_cov)
  
  # -----------------------------
  # Add section rows LAST
  # -----------------------------
  tab_one <- bind_rows(
    tibble(
      term = "Without covariates",
      c1 = "\u200B(1)",
      c2 = "\u200B(2)",
      c3 = "\u200B(3)",
      c4 = "\u200B(4)"
    ),
    tab_fe,
    tibble(term = "", c1 = "", c2 = "", c3 = "", c4 = ""),
    tibble(term = "", c1 = "", c2 = "", c3 = "", c4 = ""),
    tibble(
      term = "With covariates",
      c1 = "\u200B(5)",
      c2 = "\u200B(6)",
      c3 = "\u200B(7)",
      c4 = "\u200B(8)"
    ),
    tab_cov
  )
  
  # -----------------------------
  # Add continuous borders across columns 2:5
  # Row 1  = model numbers for FE-only block
  # Row 8  = FE-only RMSE row
  # Row 11 = model numbers for covariate block
  # -----------------------------
  for (r in c(1, 8, 11)) {
    tab_one[r, c("c1", "c2", "c3", "c4")] <- lapply(
      tab_one[r, c("c1", "c2", "c3", "c4")],
      function(x) kableExtra::cell_spec(
        x,
        format = "html",
        extra_css = paste(
          "display:block;",
          "width:calc(100% + 28px);",
          "margin-left:-14px;",
          "margin-right:-14px;",
          "box-sizing:border-box;",
          "border-bottom:1px solid black;",
          "padding-left:14px;",
          "padding-right:14px;",
          "padding-bottom:2px;"
        )
      )
    )
  }
  
  tab_one <- tab_one %>%
    rename(" " = term)
  
  tbl <- tab_one %>%
    kbl(
      format = "html",
      caption = NULL,
      escape = FALSE,
      col.names = c(
        " ",
        "Univ FE",
        "Univ × State FE",
        "Univ × County FE",
        "Univ × EPS FE"
      ),
      align = c("l", "r", "r", "r", "r"),
      table.attr = "style='margin-left:0; margin-right:auto; width:auto;'"
    ) %>%
    kable_styling(
      font_size = 14,
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed"),
      position = "left"
    ) %>%
    row_spec(0, extra_css = "font-size: 1.15em;") %>%
    column_spec(1, width = "15em") %>%
    column_spec(
      2:5,
      width = "7em",
      extra_css = "padding-left: 14px; padding-right: 14px;"
    ) %>%
    column_spec(
      5,
      background = eps_col_bg,
      extra_css = "padding-left: 14px; padding-right: 14px;"
    ) %>%
    row_spec(1, bold = TRUE) %>%
    row_spec(6, background = adjr2_row_bg) %>%
    row_spec(11, bold = TRUE) %>%
    row_spec(16, background = adjr2_row_bg) %>%
    row_spec(18, extra_css = "border-bottom: 2px solid black;")
  
  cat("\n\n<div style='display:block; width:max-content; margin-left:0; margin-right:auto;'>\n")
  print(tbl)
  cat("\n</div>\n\n")
  cat("<div style='font-size:0.5em; margin-top:8px; margin-left:0; text-align:left;'>Parameters include absorbed fixed effects.</div>\n\n")
}
