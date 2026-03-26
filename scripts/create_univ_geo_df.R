################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_univ_geo_df.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/22/2025
## [ DESC ] < create university-geomarket level data on number of schools and number of visits>
################################################################################

####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES. note: assumes you have already run this:
#getwd()
#source(file = file.path("scripts", "create_cb_geo_hs_visits.R"))
#getwd()


###########
########### CREATE BJECT WITH ONE OBSERVATION PER UNIVERSITY, EPS THAT HAS VARIABLES ABOUT NUMBER OF SCHOOLS AND NUMBER OF VISITS TO THOSE SCHOOOLS
###########

# which schools to include
df <- pubprivhs_univ_df %>%
  filter(!is.na(hs_eps_codename),univ_id =='all') %>%
  mutate(
    visit01    = as.integer(visit01),
    num_visits = as.integer(num_visits)
  )

#df %>% glimpse()

df %>% group_by(hs_control,hs_school_type) %>% 
  summarise(
    n_schools = n(),
    n_vis_tot = sum(num_visits_all, na.rm = TRUE),
    mean_vis_tot = mean(num_visits_all, na.rm = TRUE)
  )

df %>% count(hs_control,hs_school_type)

# create a measure of g12 enrollment size and see how many visits are to schools above/below threshold
df %>% group_by(hs_control,hs_school_type) %>% 
  summarize(
    hs_g12_mean = mean(hs_g12, na.rm = TRUE)
  )

df %>%
  mutate(
    hs_g12_cat = case_when(
      hs_g12 < 50                   ~ "<50",
      hs_g12 >= 50 & hs_g12 <= 100  ~ "50–100",
      hs_g12 > 100                  ~ "100+",
      TRUE ~ NA_character_
    ),
    hs_g12_cat = factor(hs_g12_cat, levels = c("<50", "50–100", "100+"))
  ) %>%
  group_by(hs_control, hs_school_type, hs_g12_cat) %>%
  summarize(
    n_sch = n(),
    n_vis_tot = sum(num_visits_all, na.rm = TRUE),
    mean_vis_tot = mean(num_visits_all, na.rm = TRUE),
    .groups = "drop"   # optional, removes the warning about grouping
  )
# # A tibble: 9 × 6
# hs_control hs_school_type           hs_g12_cat n_sch n_vis_tot mean_vis_tot
# <fct>      <fct>                    <fct>      <int>     <dbl>        <dbl>
#   1 public     regular school           <50         4439       725        0.163
# 2 public     regular school           50–100      3893      1483        0.381
# 3 public     regular school           100+        9773     21708        2.22 
# 4 private    regular school           <50         2020      1395        0.691
# 5 private    regular school           50–100       929      4261        4.59 
# 6 private    regular school           100+         817      7167        8.77 
# 7 private    special program emphasis <50          142       170        1.20 
# 8 private    special program emphasis 50–100        37       201        5.43 
# 9 private    special program emphasis 100+          23       196        8.52 

rm(df)

# =========================================================
# create_univ_geo_df.R
# conservative refactor: same logic, less repetition
# added n_g12_* variables parallel to n_sch_*
# added per-1000-12th-graders rate variables, named *_per_g12k_*
# FIX: overall EPS-level n_g12_* now use distinct schools
# =========================================================

options(max.print = 1500)

library(tidyverse)
library(forcats)
library(rlang)

# ---------------------------------------------------------
# helpers
# ---------------------------------------------------------

slice_levels <- c("local", "instate", "inregion", "national", "outstate")
sector_levels <- c("all", "pub", "priv")

sector_cond <- list(
  all  = expr(TRUE),
  pub  = expr(hs_control == "public"),
  priv = expr(hs_control == "private")
)

slice_cond <- list(
  local    = expr(hs_univ_market == "local"),
  instate  = expr(hs_univ_market == "in_state"),
  inregion = expr(hs_univ_market == "regional"),
  national = expr(hs_univ_market == "national"),
  outstate = expr(hs_univ_market %in% c("regional", "national"))
)

safe_div_expr <- function(num_nm, den_nm) {
  expr(if_else(!!sym(den_nm) > 0, !!sym(num_nm) / !!sym(den_nm), NA_real_))
}

safe_div_1000_expr <- function(num_nm, den_nm) {
  expr(if_else(!!sym(den_nm) > 0, 1000 * !!sym(num_nm) / !!sym(den_nm), NA_real_))
}

make_pair_summary_exprs <- function() {
  out <- list()
  
  for (sec in sector_levels) {
    sec_expr <- sector_cond[[sec]]
    
    # overall (no market suffix)
    out[[paste0("n_sch_", sec)]] <- if (sec == "all") {
      expr(n())
    } else {
      expr(sum(!!sec_expr, na.rm = TRUE))
    }
    
    out[[paste0("n_g12_", sec)]] <- if (sec == "all") {
      expr(sum(hs_g12, na.rm = TRUE))
    } else {
      expr(sum(if_else(!!sec_expr, hs_g12, 0), na.rm = TRUE))
    }
    
    out[[paste0("n_vis01_", sec)]] <- if (sec == "all") {
      expr(sum(visit01 == 1, na.rm = TRUE))
    } else {
      expr(sum(visit01 == 1 & !!sec_expr, na.rm = TRUE))
    }
    
    out[[paste0("n_vistot_", sec)]] <- if (sec == "all") {
      expr(sum(num_visits, na.rm = TRUE))
    } else {
      expr(sum(if_else(!!sec_expr, num_visits, 0L), na.rm = TRUE))
    }
    
    # market-specific suffixes
    for (sl in slice_levels) {
      sl_expr <- slice_cond[[sl]]
      
      out[[paste0("n_sch_", sec, "_", sl)]] <- if (sec == "all") {
        expr(sum(!!sl_expr, na.rm = TRUE))
      } else {
        expr(sum(!!sec_expr & !!sl_expr, na.rm = TRUE))
      }
      
      out[[paste0("n_g12_", sec, "_", sl)]] <- if (sec == "all") {
        expr(sum(if_else(!!sl_expr, hs_g12, 0), na.rm = TRUE))
      } else {
        expr(sum(if_else(!!sec_expr & !!sl_expr, hs_g12, 0), na.rm = TRUE))
      }
      
      out[[paste0("n_vis01_", sec, "_", sl)]] <- if (sec == "all") {
        expr(sum(visit01 == 1 & !!sl_expr, na.rm = TRUE))
      } else {
        expr(sum(visit01 == 1 & !!sec_expr & !!sl_expr, na.rm = TRUE))
      }
      
      out[[paste0("n_vistot_", sec, "_", sl)]] <- if (sec == "all") {
        expr(sum(if_else(!!sl_expr, num_visits, 0L), na.rm = TRUE))
      } else {
        expr(sum(if_else(!!sec_expr & !!sl_expr, num_visits, 0L), na.rm = TRUE))
      }
    }
  }
  
  out
}

make_rate_exprs <- function() {
  out <- list()
  
  for (sec in sector_levels) {
    # overall per school
    out[[paste0("n_vis01_per_sch_", sec)]] <-
      safe_div_expr(
        num_nm = paste0("n_vis01_", sec),
        den_nm = paste0("n_sch_", sec)
      )
    
    out[[paste0("n_vistot_per_sch_", sec)]] <-
      safe_div_expr(
        num_nm = paste0("n_vistot_", sec),
        den_nm = paste0("n_sch_", sec)
      )
    
    # overall per 1,000 12th graders
    out[[paste0("n_vis01_per_g12k_", sec)]] <-
      safe_div_1000_expr(
        num_nm = paste0("n_vis01_", sec),
        den_nm = paste0("n_g12_", sec)
      )
    
    out[[paste0("n_vistot_per_g12k_", sec)]] <-
      safe_div_1000_expr(
        num_nm = paste0("n_vistot_", sec),
        den_nm = paste0("n_g12_", sec)
      )
    
    # by slice
    for (sl in slice_levels) {
      out[[paste0("n_vis01_per_sch_", sec, "_", sl)]] <-
        safe_div_expr(
          num_nm = paste0("n_vis01_", sec, "_", sl),
          den_nm = paste0("n_sch_", sec, "_", sl)
        )
      
      out[[paste0("n_vistot_per_sch_", sec, "_", sl)]] <-
        safe_div_expr(
          num_nm = paste0("n_vistot_", sec, "_", sl),
          den_nm = paste0("n_sch_", sec, "_", sl)
        )
      
      out[[paste0("n_vis01_per_g12k_", sec, "_", sl)]] <-
        safe_div_1000_expr(
          num_nm = paste0("n_vis01_", sec, "_", sl),
          den_nm = paste0("n_g12_", sec, "_", sl)
        )
      
      out[[paste0("n_vistot_per_g12k_", sec, "_", sl)]] <-
        safe_div_1000_expr(
          num_nm = paste0("n_vistot_", sec, "_", sl),
          den_nm = paste0("n_g12_", sec, "_", sl)
        )
    }
  }
  
  out
}

# EPS-level OVERALL totals across all universities:
# n_sch_* are distinct-school counts
# n_vis01_* and n_vistot_* remain pair-level sums
# n_g12_* will be added separately from a deduplicated school-level object
make_eps_overall_exprs <- function() {
  out <- list()
  
  for (sec in sector_levels) {
    sec_expr <- sector_cond[[sec]]
    
    out[[paste0("n_sch_", sec)]] <- if (sec == "all") {
      expr(n_distinct(hs_ncessch))
    } else {
      expr(n_distinct(hs_ncessch[!!sec_expr]))
    }
    
    out[[paste0("n_vis01_", sec)]] <- if (sec == "all") {
      expr(sum(visit01 == 1, na.rm = TRUE))
    } else {
      expr(sum(visit01 == 1 & !!sec_expr, na.rm = TRUE))
    }
    
    out[[paste0("n_vistot_", sec)]] <- if (sec == "all") {
      expr(sum(num_visits, na.rm = TRUE))
    } else {
      expr(sum(if_else(!!sec_expr, num_visits, 0L), na.rm = TRUE))
    }
  }
  
  out
}

# ---------------------------------------------------------
# data prep
# ---------------------------------------------------------

df_work <- pubprivhs_univ_df %>%
  filter(!is.na(hs_eps_codename), univ_id != "all") %>%
  mutate(
    visit01    = as.integer(visit01),
    num_visits = as.integer(num_visits)
  ) %>%
  # keep schools with sufficient numbers of 12th graders
  filter((hs_control == "public" & hs_g12 >= 100) | (hs_control == "private" & hs_g12 >= 50)) %>%
  mutate(
    # Assign NA market type to "national" so local + in_state + regional + national = all
    hs_univ_market = forcats::fct_na_value_to_level(hs_univ_market, level = "national")
  )

# ---------------------------------------------------------
# by university x EPS
# ---------------------------------------------------------

pair_summary_exprs <- make_pair_summary_exprs()
rate_exprs <- make_rate_exprs()

df_by_univ_eps <- df_work %>%
  group_by(univ_id, hs_eps_codename) %>%
  summarise(
    !!!pair_summary_exprs,
    .groups = "drop"
  ) %>%
  mutate(
    !!!rate_exprs
  ) %>%
  left_join(
    y = univ_df %>%
      select(univ_id, univ_classification, univ_abbrev, univ_usnwr_rank),
    by = "univ_id"
  )

# ---------------------------------------------------------
# by EPS across all universities
# ---------------------------------------------------------

# overall totals by EPS: distinct-school counts + pair-level visit totals
eps_overall_exprs <- make_eps_overall_exprs()

df_by_eps <- df_work %>%
  group_by(hs_eps_codename) %>%
  summarise(
    !!!eps_overall_exprs,
    .groups = "drop"
  )

# FIXED: overall 12th-grade totals by EPS must use distinct schools
df_eps_g12_overall <- df_work %>%
  distinct(hs_ncessch, .keep_all = TRUE) %>%
  group_by(hs_eps_codename) %>%
  summarise(
    n_g12_all  = sum(hs_g12, na.rm = TRUE),
    n_g12_pub  = sum(if_else(hs_control == "public",  hs_g12, 0), na.rm = TRUE),
    n_g12_priv = sum(if_else(hs_control == "private", hs_g12, 0), na.rm = TRUE),
    .groups = "drop"
  )

df_by_eps <- df_by_eps %>%
  left_join(df_eps_g12_overall, by = "hs_eps_codename")

# slice-specific visit totals across universities
df_by_eps_temp <- df_by_univ_eps %>%
  group_by(hs_eps_codename) %>%
  summarise(
    across(
      matches("^(n_vis01|n_vistot)_(all|pub|priv)_(local|instate|inregion|national|outstate)$"),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# slice-specific school denominators: average pair-based school counts per university
n_sch_pair <- df_by_univ_eps %>%
  group_by(hs_eps_codename) %>%
  summarise(
    n_univ = n_distinct(univ_id),
    across(
      matches("^n_sch_(all|pub|priv)_(local|instate|inregion|national|outstate)$"),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      matches("^n_sch_(all|pub|priv)_(local|instate|inregion|national|outstate)$"),
      ~ if_else(n_univ > 0, .x / n_univ, NA_real_),
      .names = "{.col}"
    )
  ) %>%
  select(-n_univ)

# slice-specific 12th-grade denominators:
# keep the same pair-relative average-across-universities logic
n_g12_pair <- df_by_univ_eps %>%
  group_by(hs_eps_codename) %>%
  summarise(
    n_univ = n_distinct(univ_id),
    across(
      matches("^n_g12_(all|pub|priv)_(local|instate|inregion|national|outstate)$"),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      matches("^n_g12_(all|pub|priv)_(local|instate|inregion|national|outstate)$"),
      ~ if_else(n_univ > 0, .x / n_univ, NA_real_),
      .names = "{.col}"
    )
  ) %>%
  select(-n_univ)

# combine EPS pieces, then add rates
df_by_eps <- df_by_eps %>%
  left_join(df_by_eps_temp, by = "hs_eps_codename") %>%
  left_join(n_sch_pair, by = "hs_eps_codename") %>%
  left_join(n_g12_pair, by = "hs_eps_codename") %>%
  mutate(
    !!!rate_exprs
  ) %>%
  mutate(
    univ_id = "all",
    univ_classification = "all",
    univ_abbrev = "all",
    univ_usnwr_rank = 999
  )

rm(df_eps_g12_overall, df_by_eps_temp, n_sch_pair, n_g12_pair)

# ---------------------------------------------------------
# append EPS-all row and merge EPS covariates
# ---------------------------------------------------------

df_by_univ_eps <- bind_rows(
  df_by_univ_eps,
  df_by_eps
) %>%
  arrange(univ_id, hs_eps_codename) %>%
  inner_join(
    y = allyr_anal_eps_sf %>%
      as_tibble() %>%
      filter(year == 2020) %>%
      select(
        eps, eps_name,
        pct_nhisp_all, pct_hisp_all, pct_nhisp_white, pct_nhisp_black,
        pct_nhisp_other, pct_nhisp_asian, pct_nhisp_nhpi, pct_nhisp_multi,
        pct_nhisp_api, pct_hisp_api, med_inc_house, med_inc_house_mean,
        pct_pov_yes, pct_edu_baplus_all
      ) %>%
      mutate(
        hs_eps_codename = str_c(str_trim(eps), " - ", str_trim(eps_name)) |> as_factor()
      ) %>%
      select(-c(eps, eps_name)),
    by = "hs_eps_codename"
  ) %>%
  rename(mean_inc_house = med_inc_house_mean)

# ---------------------------------------------------------
# cleanup
# ---------------------------------------------------------

rm(df_by_eps)
rm(df_work)
rm(pair_summary_exprs, rate_exprs, eps_overall_exprs)

# optional checks
df_by_univ_eps %>% glimpse()