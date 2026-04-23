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

make_geo_table <- function(df,
                           rate_type = c("both", "per_sch", "per_g12k"),
                           school_type,           # "all", "pub", or "priv"
                           market_segment = NULL, # NULL or "" = aggregate across all segments
                           visit_type = "vistot", # "vistot" or "vis01"
                           outfile,
                           caption,
                           schools_digits = NULL, # used only for per_sch tables
                           g12_digits = 1,        # used only for per_g12k tables
                           visits_digits  = 0) {
  
  rate_type   <- match.arg(rate_type)
  school_type <- match.arg(school_type, c("all", "pub", "priv"))
  visit_type  <- match.arg(visit_type,  c("vistot", "vis01"))
  
  if (!is.null(market_segment) && nzchar(market_segment)) {
    market_segment <- match.arg(
      market_segment,
      c("local", "instate", "inregion", "national", "outstate")
    )
  } else {
    market_segment <- NULL
  }
  
  seg_suffix <- if (is.null(market_segment)) "" else paste0("_", market_segment)
  
  df_filtered <- df %>% dplyr::filter(univ_id == "all")
  
  build_one_geo_table <- function(rate_type_one) {
    
    visits_var <- paste0("n_", visit_type, "_", school_type, seg_suffix)
    
    if (rate_type_one == "per_sch") {
      denom_var    <- paste0("n_sch_", school_type, seg_suffix)
      rate_var     <- paste0("n_", visit_type, "_per_sch_", school_type, seg_suffix)
      rate_label   <- "Visits/Sch"
      denom_label  <- "Schools"
      denom_digits <- if (is.null(schools_digits)) {
        if (is.null(market_segment)) 0L else 1L
      } else {
        schools_digits
      }
      outfile_use  <- paste0(outfile, "_sch")
      caption_use  <- caption
    } else {
      denom_var    <- paste0("n_g12_", school_type, seg_suffix)
      rate_var     <- paste0("n_", visit_type, "_per_g12k_", school_type, seg_suffix)
      rate_label   <- "Visits/G12k"
      denom_label  <- "G12k"
      denom_digits <- g12_digits
      outfile_use  <- paste0(outfile, "_g12k")
      caption_use  <- gsub("per school", "per 1,000 12th graders", caption, fixed = TRUE)
    }
    
    df_top30 <- df_filtered %>%
      dplyr::arrange(desc(.data[[rate_var]])) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::slice(1:30)
    
    cols_to_avg <- c(
      "mean_inc_house", "pct_edu_baplus_all",
      "pct_pov_yes", "pct_nhisp_white", "pct_nhisp_asian",
      "pct_nhisp_black", "pct_hisp_all"
    )
    
    avg_base <- allyr_anal_tract_sf %>% filter(year == 2020) %>% as_tibble() %>% 
      dplyr::summarise(dplyr::across(dplyr::all_of(cols_to_avg), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::mutate(
        rank            = NA_integer_,
        hs_eps_codename = "Mean (All Census Tracts)"
      )
    
    df_combined <- dplyr::bind_rows(
      avg_base,
      df_top30 %>% dplyr::mutate(hs_eps_codename = as.character(hs_eps_codename))
    )
    
    df_out <- tibble::tibble(
      Rank      = dplyr::if_else(is.na(df_combined$rank), "—", as.character(df_combined$rank)),
      EPS       = df_combined$hs_eps_codename,
      MeanInc   = paste0("$", round(df_combined$mean_inc_house / 1000), "k"),
      `%BA+`    = sprintf('%.1f', df_combined$pct_edu_baplus_all),
      `%Pov`    = sprintf('%.1f', df_combined$pct_pov_yes),
      `%White`  = sprintf('%.1f', df_combined$pct_nhisp_white),
      `%Asian`  = sprintf('%.1f', df_combined$pct_nhisp_asian),
      `%Black`  = sprintf('%.1f', df_combined$pct_nhisp_black),
      `%Hisp`   = sprintf('%.1f', df_combined$pct_hisp_all),
      Visits    = sprintf(paste0('%.', visits_digits, 'f'), df_combined[[visits_var]])
    )
    
    df_out[[rate_label]] <- sprintf('%.1f', df_combined[[rate_var]])
    
    if (rate_type_one == "per_sch") {
      df_out[[denom_label]] <- sprintf(paste0('%.', denom_digits, 'f'), df_combined[[denom_var]])
    } else {
      df_out[[denom_label]] <- sprintf(paste0('%.', denom_digits, 'f'), df_combined[[denom_var]] / 1000)
    }
    
    df_out <- df_out %>%
      dplyr::select(
        Rank, EPS,
        dplyr::all_of(rate_label),
        dplyr::all_of(denom_label),
        Visits,
        MeanInc, `%BA+`, `%Pov`, `%White`, `%Asian`, `%Black`, `%Hisp`
      ) %>% 
      mutate(across(everything(), ~ifelse(. == "NA", "-", as.character(.))))
    
    print(df_out, n = 31)
    
    tex_table <-
      df_out %>%
      kableExtra::kbl(
        format   = "latex",
        booktabs = TRUE,
        digits   = 1,
        caption  = caption_use
      ) %>%
      kableExtra::kable_classic(full_width = FALSE)
    
    kableExtra::save_kable(tex_table, paste0(outfile_use, ".tex"))
    saveRDS(df_out, file = paste0(outfile_use, ".RDS"))
    
    invisible(df_out)
  }
  
  if (rate_type == "both") {
    out1 <- build_one_geo_table("per_sch")
    out2 <- build_one_geo_table("per_g12k")
    return(invisible(list(per_sch = out1, per_g12k = out2)))
  } else {
    return(invisible(build_one_geo_table(rate_type)))
  }
}

#### ==============================
#### CALL 1: PUBLIC SCHOOLS
#### ==============================
make_geo_table(
  df             = df_by_univ_eps,
  rate_type      = "both",
  school_type    = "pub",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_pub",
  caption        = "Top 30 Geomarkets ranked by visits per school (public school visits only)"
)

make_geo_table(
  df             = df_by_univ_eps,
  rate_type      = "both",
  school_type    = "pub",
  market_segment = "national",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_pub_national",
  caption        = "Top 30 Geomarkets ranked by visits per school (public school visits only), national market segment only"
)

#### ==============================
#### CALL 2: PRIVATE SCHOOLS
#### ==============================
make_geo_table(
  df             = df_by_univ_eps,
  rate_type      = "both",
  school_type    = "priv",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_priv",
  caption        = "Top 30 Geomarkets ranked by visits per school (private school visits only)"
)

make_geo_table(
  df             = df_by_univ_eps,
  rate_type      = "both",
  school_type    = "priv",
  market_segment = "national",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_priv_national",
  caption        = "Top 30 Geomarkets ranked by visits per school (private school visits only), national market segment only"
)

#### ==============================
#### OPTIONAL: ALL SCHOOLS
#### ==============================
make_geo_table(
  df             = df_by_univ_eps,
  rate_type      = "both",
  school_type    = "all",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_all",
  caption        = "Top 30 Geomarkets ranked by visits per school (all schools)"
)

make_geo_table(
  df             = df_by_univ_eps,
  rate_type      = "both",
  school_type    = "all",
  market_segment = "national",
  visit_type     = "vistot",
  outfile        = "results/top30_geo_all_national",
  caption        = "Top 30 Geomarkets ranked by visits per school (all schools), national market segment only"
)
