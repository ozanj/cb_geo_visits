# ============================================================
#  ASK CLAUDE TO FILL IN BASIC INFO ABOUT SCRIPT. WHATEVER IS APPROPRIATE
# ============================================================

options(max.print = 1000)
library(tidyverse)
library(forcats)
library(scales)
library(patchwork)
library(kableExtra)

####### SOURCE DATA SCRIPTS
getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()
source(file = file.path('scripts', 'create_univ_geo_df.R'))
getwd()
rm(create_rq1_map, format_vars, get_palette)


#############
############# WHICH GEOMARKETS GET VISITS
#############

# data frames to use
events_df %>% glimpse()
allyr_anal_eps_sf %>% glimpse()
pubprivhs_univ_df %>% glimpse()
univ_df %>% glimpse()


#############
############# CLAUDE -- DON'T GET RID OF THESE LITTLE CHECKS HERE. I WANNA COME BACK TO THEM. LATER. I NOTICE THAT 51 OBS HAVE STATE CODE = 'BI' WHICH I THINK IS BUREAU OF INDIAN AFFAIRS. SO I WANT TO GO UPSTREAM, PRIOR TO CREATION OF pubprivhs_univ_df DATA FRAME AND ASSIGN THOSE TO THE STATES THAT ENCRICLE THE RESPECTIVE INDIAN RESERVATIONS
#############

pubprivhs_univ_df %>% filter(univ_id == '147767') %>% count(hs_state_code) %>% print(n=60)

pubprivhs_univ_df %>% filter(univ_id == '147767') %>% count(hs_eps_region)
pubprivhs_univ_df %>% filter(univ_id == '147767') %>% count(univ_eps_region)


pubprivhs_univ_df %>% count(hs_univ_market)


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

######## make figures for 2-pager

df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_pub)) %>% 
  select(hs_eps_codename,n_sch_pub,n_vistot_pub,n_vistot_per_sch_pub,med_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_black,pct_hisp_all) %>% print(n=30) # all schools
df_by_univ_eps %>% filter(univ_id == 'all') %>%  arrange(desc(n_vistot_per_sch_priv)) %>% 
  select(hs_eps_codename,n_sch_priv,n_vistot_priv,n_vistot_per_sch_priv,med_inc_house,pct_edu_baplus_all,pct_pov_yes,pct_nhisp_black,pct_hisp_all) %>% print(n=30) # all schools


make_geo_table <- function(df,
                           school_type,           # "all", "pub", or "priv"
                           market_segment = NULL, # NULL or "" = aggregate across all segments;
                           # or one of: "local", "instate",
                           #            "national", "outstate"
                           # ("inregion" is valid but not used in practice)
                           visit_type = "vistot", # "vistot" = all visits (total);
                           # "vis01"  = first-contact visits only
                           outfile,
                           caption,
                           schools_digits = NULL, # decimals for Schools column; auto-set if NULL:
                           #   0 when market_segment is NULL/"" (integer counts)
                           #   1 when market_segment is specified (dbl column)
                           visits_digits  = 0) {  # visits are always whole numbers; rarely needs changing
  
  # ── Validate inputs ───────────────────────────────────────────────────────
  school_type <- match.arg(school_type, c("all", "pub", "priv"))
  visit_type  <- match.arg(visit_type,  c("vistot", "vis01"))
  if (!is.null(market_segment) && nzchar(market_segment)) {
    market_segment <- match.arg(
      market_segment,
      c("local", "instate", "inregion", "national", "outstate")
    )
  } else {
    market_segment <- NULL   # normalise "" → NULL
  }
  
  # ── Construct variable names from school_type × market_segment ────────────
  # All relevant columns in df follow a consistent naming convention:
  #   n_sch_{school_type}[_{segment}]
  #   n_{visit_type}_{school_type}[_{segment}]
  #   n_{visit_type}_per_sch_{school_type}[_{segment}]
  # Omitting the segment suffix gives the aggregate across all segments.
  seg_suffix <- if (is.null(market_segment)) "" else paste0("_", market_segment)
  
  schools_var <- paste0("n_sch_",               school_type, seg_suffix)
  visits_var  <- paste0("n_", visit_type, "_",  school_type, seg_suffix)
  vps_var     <- paste0("n_", visit_type, "_per_sch_", school_type, seg_suffix)
  
  # Auto-set schools_digits: aggregate counts land as <int> (0 decimals);
  # segment-specific counts are <dbl> pre-filtered columns (1 decimal).
  if (is.null(schools_digits)) {
    schools_digits <- if (is.null(market_segment)) 0L else 1L
  }
  
  df_filtered <- df %>% filter(univ_id == "all")
  
  # ── Top-30 rows ───────────────────────────────────────────────────────────
  # Sort descending by vps_var, assign ranks, keep top 30.
  # vps_var is used as-is from the data — it was pre-computed at the
  # geomarket level before this function is called.
  #
  # NOTE: For school_type = "priv", vps_var is NA in geomarkets with zero
  # private schools (0/0 in the pre-computation). arrange(desc(...)) pushes
  # NAs to the bottom naturally, so they never appear in the top-30.
  df_top30 <- df_filtered %>%
    arrange(desc(.data[[vps_var]])) %>%
    mutate(rank = row_number()) %>%
    slice(1:30)
  
  # ── Average row: unweighted mean across ALL geomarkets ───────────────────
  # Simple (unweighted) means treat every EPS geomarket equally regardless of
  # size, giving the "average geomarket" — appropriate because the unit of
  # analysis in this table is the geomarket, not the individual or school.
  # A population-weighted mean would instead describe the "average American,"
  # which is a different and less relevant quantity here.
  #
  # vps_var is intentionally EXCLUDED from cols_to_avg and derived separately
  # below. We compute Visits/Sch as mean(Visits) / mean(Schools) — the ratio
  # of means — rather than mean(Visits/Schools) — the mean of per-geomarket
  # ratios. These differ when geomarket sizes vary (which they do). We use the
  # ratio of means so that a reader can divide the Schools column into the
  # Visits column and recover the Visits/Sch value exactly, keeping all three
  # columns arithmetically consistent.
  #
  # For school_type = "priv": schools_var and visits_var are raw counts (0
  # when no private schools exist in a geomarket, not NA), so na.rm = TRUE
  # handles only genuine missingness. The avg-row VPS is therefore the ratio
  # of mean visits to mean schools across all geomarkets — arithmetically
  # correct and unaffected by the NA vps_var values in the pre-computed column.
  cols_to_avg <- c(visits_var, schools_var,
                   "mean_inc_house", "pct_edu_baplus_all",
                   "pct_pov_yes", "pct_nhisp_white", "pct_nhisp_asian",
                   "pct_nhisp_black", "pct_hisp_all")
  
  avg_base <- df_filtered %>%
    summarise(across(all_of(cols_to_avg), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(
      rank            = NA_integer_,
      hs_eps_codename = "Mean (All Geomarkets)",
      !!vps_var       := .data[[visits_var]] / .data[[schools_var]]  # ratio of means; see note above
    )
  
  # ── Bind avg row on top of top-30 ────────────────────────────────────────
  # Convert hs_eps_codename to character in df_top30 to avoid a
  # factor/character type mismatch warning from bind_rows.
  df_combined <- bind_rows(
    avg_base,
    df_top30 %>% mutate(hs_eps_codename = as.character(hs_eps_codename))
  )
  
  # ── Format output tibble ─────────────────────────────────────────────────
  # MeanInc stored as "$Xk" character string (divided by 1000, rounded to
  # nearest integer). All percentage columns rounded to 1 decimal. Schools and
  # Visits rounding controlled by schools_digits / visits_digits arguments.
  df_out <- tibble(
    Rank         = if_else(is.na(df_combined$rank), "—", as.character(df_combined$rank)),
    EPS          = df_combined$hs_eps_codename,
    `Visits/Sch` = round(df_combined[[vps_var]],      1),
    Schools      = round(df_combined[[schools_var]], schools_digits),
    Visits       = round(df_combined[[visits_var]],  visits_digits),
    MeanInc      = paste0("$", round(df_combined$mean_inc_house / 1000), "k"),
    `%BA+`       = round(df_combined$pct_edu_baplus_all, 1),
    `%Pov`       = round(df_combined$pct_pov_yes,        1),
    `%White`     = round(df_combined$pct_nhisp_white,    1),
    `%Asian`     = round(df_combined$pct_nhisp_asian,    1),
    `%Black`     = round(df_combined$pct_nhisp_black,    1),
    `%Hisp`      = round(df_combined$pct_hisp_all,       1)
  )
  
  # ── Console, LaTeX, RDS ──────────────────────────────────────────────────
  print(df_out, n = 31)
  
  tex_table <-
    df_out %>%
    kbl(
      format   = "latex",
      booktabs = TRUE,
      digits   = 1,
      caption  = caption
    ) %>%
    kable_classic(full_width = FALSE)
  
  save_kable(tex_table, paste0(outfile, '.tex'))
  saveRDS(df_out, file = paste0(outfile, '.RDS'))
  
  # ── MANUSCRIPT TEXT (paste into paper as-is or lightly edited) ───────────
  # The first row of each table reports means across all geomarkets as a
  # reference point for interpreting the top-30 rankings. All statistics are
  # simple (unweighted) averages that treat each EPS geomarket equally
  # regardless of size. Visits per school in the mean row is computed as mean
  # visits divided by mean schools (ratio of means), ensuring that readers can
  # verify the value by dividing the Schools and Visits columns directly.
}

#### ==============================
#### CALL 1: PUBLIC SCHOOLS
#### ==============================
make_geo_table(
  df             = df_by_univ_eps,
  school_type    = "pub",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_pub_sch",
  caption        = "Top 30 Geomarkets ranked by visits per school (public school visits only)"
  # schools_digits auto-set to 0 (no market_segment → integer counts)
)
make_geo_table(
  df             = df_by_univ_eps,
  school_type    = "pub",
  market_segment = "national",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_pub_sch_national",
  caption        = "Top 30 Geomarkets ranked by visits per school (public school visits only), national market segment only"
  # schools_digits auto-set to 1 (market_segment specified → dbl column)
)

#### ==============================
#### CALL 2: PRIVATE SCHOOLS
#### ==============================
make_geo_table(
  df             = df_by_univ_eps,
  school_type    = "priv",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_priv_sch",
  caption        = "Top 30 Geomarkets ranked by visits per school (private school visits only)"
  # schools_digits auto-set to 0 (no market_segment → integer counts)
)
make_geo_table(
  df             = df_by_univ_eps,
  school_type    = "priv",
  market_segment = "national",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_priv_sch_national",
  caption        = "Top 30 Geomarkets ranked by visits per school (private school visits only), national market segment only"
  # schools_digits auto-set to 1 (market_segment specified → dbl column)
)

