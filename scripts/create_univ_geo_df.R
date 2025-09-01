################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_univ_geo_df.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/22/2025
## [ DESC ] < create university-geomarket level data on number of schools and number of visits>
################################################################################


###########
########### CREATE BJECT WITH ONE OBSERVATION PER UNIVERSITY, EPS THAT HAS VARIABLES ABOUT NUMBER OF SCHOOLS AND NUMBER OF VISITS TO THOSE SCHOOOLS
###########

df_by_univ_eps <- pubprivhs_univ_df %>%
  filter(!is.na(hs_eps_codename)) %>%
  mutate(
    visit01    = as.integer(visit01),
    num_visits = as.integer(num_visits)
  ) %>%
  group_by(univ_id,hs_eps_codename) %>%
  summarise(
    # ===================== ALL SCHOOLS =====================
    # number of schools
    n_sch_all          = n(),
    n_sch_all_local    = sum(hs_univ_market == 'local',    na.rm = TRUE),
    n_sch_all_instate  = sum(hs_univ_market == 'in_state', na.rm = TRUE),
    n_sch_all_inregion = sum(hs_univ_market == 'regional', na.rm = TRUE),
    n_sch_all_national = sum(hs_univ_market == 'national', na.rm = TRUE),
    
    # number of visits [01]
    n_vis01_all          = sum(visit01 == 1,                                  na.rm = TRUE),
    n_vis01_all_local    = sum(visit01 == 1 & hs_univ_market == 'local',     na.rm = TRUE),
    n_vis01_all_instate  = sum(visit01 == 1 & hs_univ_market == 'in_state',  na.rm = TRUE),
    n_vis01_all_inregion = sum(visit01 == 1 & hs_univ_market == 'regional',  na.rm = TRUE),
    n_vis01_all_national = sum(visit01 == 1 & hs_univ_market == 'national',  na.rm = TRUE),
    
    # total number of visits (vistot)
    n_vistot_all          = sum(num_visits,                                        na.rm = TRUE),
    n_vistot_all_local    = sum(if_else(hs_univ_market == 'local',    num_visits, 0L),    na.rm = TRUE),
    n_vistot_all_instate  = sum(if_else(hs_univ_market == 'in_state', num_visits, 0L),    na.rm = TRUE),
    n_vistot_all_inregion = sum(if_else(hs_univ_market == 'regional', num_visits, 0L),    na.rm = TRUE),
    n_vistot_all_national = sum(if_else(hs_univ_market == 'national', num_visits, 0L),    na.rm = TRUE),
    
    # ===================== PUBLIC SCHOOLS ==================
    # number of schools
    n_sch_pub          = sum(hs_control == 'public',                                         na.rm = TRUE),
    n_sch_pub_local    = sum(hs_control == 'public' & hs_univ_market == 'local',             na.rm = TRUE),
    n_sch_pub_instate  = sum(hs_control == 'public' & hs_univ_market == 'in_state',          na.rm = TRUE),
    n_sch_pub_inregion = sum(hs_control == 'public' & hs_univ_market == 'regional',          na.rm = TRUE),
    n_sch_pub_national = sum(hs_control == 'public' & hs_univ_market == 'national',          na.rm = TRUE),
    
    # number of visits [01]
    n_vis01_pub          = sum(visit01 == 1 & hs_control == 'public',                                   na.rm = TRUE),
    n_vis01_pub_local    = sum(visit01 == 1 & hs_control == 'public' & hs_univ_market == 'local',       na.rm = TRUE),
    n_vis01_pub_instate  = sum(visit01 == 1 & hs_control == 'public' & hs_univ_market == 'in_state',    na.rm = TRUE),
    n_vis01_pub_inregion = sum(visit01 == 1 & hs_control == 'public' & hs_univ_market == 'regional',    na.rm = TRUE),
    n_vis01_pub_national = sum(visit01 == 1 & hs_control == 'public' & hs_univ_market == 'national',    na.rm = TRUE),
    
    # total number of visits (vistot)
    n_vistot_pub          = sum(if_else(hs_control == 'public', num_visits, 0L),                              na.rm = TRUE),
    n_vistot_pub_local    = sum(if_else(hs_control == 'public' & hs_univ_market == 'local',    num_visits, 0L),    na.rm = TRUE),
    n_vistot_pub_instate  = sum(if_else(hs_control == 'public' & hs_univ_market == 'in_state', num_visits, 0L),    na.rm = TRUE),
    n_vistot_pub_inregion = sum(if_else(hs_control == 'public' & hs_univ_market == 'regional', num_visits, 0L),    na.rm = TRUE),
    n_vistot_pub_national = sum(if_else(hs_control == 'public' & hs_univ_market == 'national', num_visits, 0L),    na.rm = TRUE),
    
    # ===================== PRIVATE SCHOOLS ================
    # number of schools
    n_sch_priv          = sum(hs_control == 'private',                                        na.rm = TRUE),
    n_sch_priv_local    = sum(hs_control == 'private' & hs_univ_market == 'local',            na.rm = TRUE),
    n_sch_priv_instate  = sum(hs_control == 'private' & hs_univ_market == 'in_state',         na.rm = TRUE),
    n_sch_priv_inregion = sum(hs_control == 'private' & hs_univ_market == 'regional',         na.rm = TRUE),
    n_sch_priv_national = sum(hs_control == 'private' & hs_univ_market == 'national',         na.rm = TRUE),
    
    # number of visits [01]
    n_vis01_priv          = sum(visit01 == 1 & hs_control == 'private',                                  na.rm = TRUE),
    n_vis01_priv_local    = sum(visit01 == 1 & hs_control == 'private' & hs_univ_market == 'local',      na.rm = TRUE),
    n_vis01_priv_instate  = sum(visit01 == 1 & hs_control == 'private' & hs_univ_market == 'in_state',   na.rm = TRUE),
    n_vis01_priv_inregion = sum(visit01 == 1 & hs_control == 'private' & hs_univ_market == 'regional',   na.rm = TRUE),
    n_vis01_priv_national = sum(visit01 == 1 & hs_control == 'private' & hs_univ_market == 'national',   na.rm = TRUE),
    
    # total number of visits (vistot)
    n_vistot_priv          = sum(if_else(hs_control == 'private', num_visits, 0L),                             na.rm = TRUE),
    n_vistot_priv_local    = sum(if_else(hs_control == 'private' & hs_univ_market == 'local',    num_visits, 0L),    na.rm = TRUE),
    n_vistot_priv_instate  = sum(if_else(hs_control == 'private' & hs_univ_market == 'in_state', num_visits, 0L),    na.rm = TRUE),
    n_vistot_priv_inregion = sum(if_else(hs_control == 'private' & hs_univ_market == 'regional', num_visits, 0L),    na.rm = TRUE),
    n_vistot_priv_national = sum(if_else(hs_control == 'private' & hs_univ_market == 'national', num_visits, 0L),    na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    ######### ALL SCHOOLS
    # 0/1 school visit
    n_vis01_per_sch_all          = if_else(n_sch_all          > 0, n_vis01_all          / n_sch_all,          NA_real_),
    n_vis01_per_sch_all_local    = if_else(n_sch_all_local    > 0, n_vis01_all_local    / n_sch_all_local,    NA_real_),
    n_vis01_per_sch_all_instate  = if_else(n_sch_all_instate  > 0, n_vis01_all_instate  / n_sch_all_instate,  NA_real_),
    n_vis01_per_sch_all_inregion = if_else(n_sch_all_inregion > 0, n_vis01_all_inregion / n_sch_all_inregion, NA_real_),
    n_vis01_per_sch_all_national = if_else(n_sch_all_national > 0, n_vis01_all_national / n_sch_all_national, NA_real_),
    
    # total visits per school
    n_vistot_per_sch_all          = if_else(n_sch_all          > 0, n_vistot_all          / n_sch_all,          NA_real_),
    n_vistot_per_sch_all_local    = if_else(n_sch_all_local    > 0, n_vistot_all_local    / n_sch_all_local,    NA_real_),
    n_vistot_per_sch_all_instate  = if_else(n_sch_all_instate  > 0, n_vistot_all_instate  / n_sch_all_instate,  NA_real_),
    n_vistot_per_sch_all_inregion = if_else(n_sch_all_inregion > 0, n_vistot_all_inregion / n_sch_all_inregion, NA_real_),
    n_vistot_per_sch_all_national = if_else(n_sch_all_national > 0, n_vistot_all_national / n_sch_all_national, NA_real_),
    
    ######### PUBLIC SCHOOLS
    # 0/1 school visit
    n_vis01_per_sch_pub          = if_else(n_sch_pub          > 0, n_vis01_pub          / n_sch_pub,          NA_real_),
    n_vis01_per_sch_pub_local    = if_else(n_sch_pub_local    > 0, n_vis01_pub_local    / n_sch_pub_local,    NA_real_),
    n_vis01_per_sch_pub_instate  = if_else(n_sch_pub_instate  > 0, n_vis01_pub_instate  / n_sch_pub_instate,  NA_real_),
    n_vis01_per_sch_pub_inregion = if_else(n_sch_pub_inregion > 0, n_vis01_pub_inregion / n_sch_pub_inregion, NA_real_),
    n_vis01_per_sch_pub_national = if_else(n_sch_pub_national > 0, n_vis01_pub_national / n_sch_pub_national, NA_real_),
    
    # total visits per school
    n_vistot_per_sch_pub          = if_else(n_sch_pub          > 0, n_vistot_pub          / n_sch_pub,          NA_real_),
    n_vistot_per_sch_pub_local    = if_else(n_sch_pub_local    > 0, n_vistot_pub_local    / n_sch_pub_local,    NA_real_),
    n_vistot_per_sch_pub_instate  = if_else(n_sch_pub_instate  > 0, n_vistot_pub_instate  / n_sch_pub_instate,  NA_real_),
    n_vistot_per_sch_pub_inregion = if_else(n_sch_pub_inregion > 0, n_vistot_pub_inregion / n_sch_pub_inregion, NA_real_),
    n_vistot_per_sch_pub_national = if_else(n_sch_pub_national > 0, n_vistot_pub_national / n_sch_pub_national, NA_real_),
    
    ######### PRIVATE SCHOOLS
    # 0/1 school visit
    n_vis01_per_sch_priv          = if_else(n_sch_priv          > 0, n_vis01_priv          / n_sch_priv,          NA_real_),
    n_vis01_per_sch_priv_local    = if_else(n_sch_priv_local    > 0, n_vis01_priv_local    / n_sch_priv_local,    NA_real_),
    n_vis01_per_sch_priv_instate  = if_else(n_sch_priv_instate  > 0, n_vis01_priv_instate  / n_sch_priv_instate,  NA_real_),
    n_vis01_per_sch_priv_inregion = if_else(n_sch_priv_inregion > 0, n_vis01_priv_inregion / n_sch_priv_inregion, NA_real_),
    n_vis01_per_sch_priv_national = if_else(n_sch_priv_national > 0, n_vis01_priv_national / n_sch_priv_national, NA_real_),
    
    # total visits per school
    n_vistot_per_sch_priv          = if_else(n_sch_priv          > 0, n_vistot_priv          / n_sch_priv,          NA_real_),
    n_vistot_per_sch_priv_local    = if_else(n_sch_priv_local    > 0, n_vistot_priv_local    / n_sch_priv_local,    NA_real_),
    n_vistot_per_sch_priv_instate  = if_else(n_sch_priv_instate  > 0, n_vistot_priv_instate  / n_sch_priv_instate,  NA_real_),
    n_vistot_per_sch_priv_inregion = if_else(n_sch_priv_inregion > 0, n_vistot_priv_inregion / n_sch_priv_inregion, NA_real_),
    n_vistot_per_sch_priv_national = if_else(n_sch_priv_national > 0, n_vistot_priv_national / n_sch_priv_national, NA_real_)
  ) %>% 
  # merge in university name and rank
  left_join(
    y = univ_df %>% select(univ_id,univ_classification,univ_abbrev,univ_usnwr_rank),
    by = c('univ_id')
  )

# df_by_univ_eps %>% glimpse()
# df_by_univ_eps %>% count(univ_classification) %>% print(n=50)

# Case Western Reserve University            201645
# Emory University                           139658
# Baylor University                          223232
# Tulane University of Louisiana             160755
# Southern Methodist University              228246

# df_by_univ_eps %>% glimpse()
# 
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_all,n_sch_all_local,n_sch_all_instate,n_sch_all_inregion,n_sch_all_national) %>% print(n=350) # all schools
# 
# 
# # ANY GEOMARKET
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_all,n_vistot_all,n_vistot_per_sch_all) %>% arrange(desc(n_vistot_per_sch_all)) %>% print(n=50) # all schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_pub,n_vistot_pub,n_vistot_per_sch_pub) %>% arrange(desc(n_vistot_per_sch_pub)) %>% print(n=50) # public schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_priv,n_vistot_priv,n_vistot_per_sch_priv) %>% arrange(desc(n_vistot_per_sch_priv)) %>% print(n=50) # private schools
# 
# # SCHOOLS IN GEOMARKET
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_all_local,n_vistot_all_local,n_vistot_per_sch_all_local) %>% arrange(desc(n_vistot_per_sch_all_local)) %>% print(n=50) # all schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_pub_local,n_vistot_pub_local,n_vistot_per_sch_pub_local) %>% arrange(desc(n_vistot_per_sch_pub_local)) %>% print(n=50) # public schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_priv_local,n_vistot_priv_local,n_vistot_per_sch_priv_local) %>% arrange(desc(n_vistot_per_sch_priv_local)) %>% print(n=50) # private schools
# 
# # SCHOOLS IN STATE
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_all_instate,n_vistot_all_instate,n_vistot_per_sch_all_instate) %>% arrange(desc(n_vistot_per_sch_all_instate)) %>% print(n=50) # all schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_pub_instate,n_vistot_pub_instate,n_vistot_per_sch_pub_instate) %>% arrange(desc(n_vistot_per_sch_pub_instate)) %>% print(n=50) # public schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_priv_instate,n_vistot_priv_instate,n_vistot_per_sch_priv_instate) %>% arrange(desc(n_vistot_per_sch_priv_instate)) %>% print(n=50) # private schools
# 
# # SCHOOLS IN REGION
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_all_inregion,n_vistot_all_inregion,n_vistot_per_sch_all_inregion) %>% arrange(desc(n_vistot_per_sch_all_inregion)) %>% print(n=50) # all schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_pub_inregion,n_vistot_pub_inregion,n_vistot_per_sch_pub_inregion) %>% arrange(desc(n_vistot_per_sch_pub_inregion)) %>% print(n=50) # public schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_priv_inregion,n_vistot_priv_inregion,n_vistot_per_sch_priv_inregion) %>% arrange(desc(n_vistot_per_sch_priv_inregion)) %>% print(n=50) # private schools
# 
# # NATIONAL SCHOOLS
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_all_national,n_vistot_all_national,n_vistot_per_sch_all_national) %>% arrange(desc(n_vistot_per_sch_all_national)) %>% print(n=50) # all schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_pub_national,n_vistot_pub_national,n_vistot_per_sch_pub_national) %>% arrange(desc(n_vistot_per_sch_pub_national)) %>% print(n=50) # public schools
# df_by_univ_eps %>% filter(univ_id == '228246') %>% select(hs_eps_codename,n_sch_priv_national,n_vistot_priv_national,n_vistot_per_sch_priv_national) %>% arrange(desc(n_vistot_per_sch_priv_national)) %>% print(n=50) # private schools

# by eps and across all universities, calcualte total number of schools, and total number of visits
df_by_eps <- pubprivhs_univ_df %>%
  filter(!is.na(hs_eps_codename)) %>%
  mutate(
    visit01    = as.integer(visit01),
    num_visits = as.integer(num_visits)
  ) %>%
  group_by(hs_eps_codename) %>%
  summarise(
    # optional context
    #hs_eps_region = first(hs_eps_region),
    
    # ---------- NUMBER OF SCHOOLS (no hs_univ_market) ----------
    n_sch_all  = n_distinct(hs_ncessch),
    n_sch_pub  = n_distinct(hs_ncessch[hs_control == "public"]),
    n_sch_priv = n_distinct(hs_ncessch[hs_control == "private"]),
    
    # -------------------------- VISITS --------------------------
    # 0/1 visit counts
    n_vis01_all  = sum(visit01 == 1, na.rm = TRUE),
    n_vis01_pub  = sum(visit01 == 1 & hs_control == "public",  na.rm = TRUE),
    n_vis01_priv = sum(visit01 == 1 & hs_control == "private", na.rm = TRUE),
    
    # total visit counts
    n_vistot_all  = sum(num_visits, na.rm = TRUE),
    n_vistot_pub  = sum(if_else(hs_control == "public",  num_visits, 0L), na.rm = TRUE),
    n_vistot_priv = sum(if_else(hs_control == "private", num_visits, 0L), na.rm = TRUE),
    
    .groups = "drop"
  )

# by eps code and across universities, calculate number of local visits, number of in-state visits, number of in-region visits, number of national visits
df_by_eps_temp <- df_by_univ_eps %>%
  group_by(hs_eps_codename) %>%
  summarise(
    across(
      matches("^(n_vis01|n_vistot)_(all|pub|priv)_(local|instate|inregion|national)$"),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# by eps_code, sum the number of local high schools across all universities, do same for number of in-state high schools across all universities, etc.
n_sch_pair <- df_by_univ_eps %>%
  group_by(hs_eps_codename) %>%
  summarise(
    # how many distinct universities have any pairing with this EPS?
    n_univ = n_distinct(univ_id),
    
    # pair-based counts (sum across university–EPS pairs)
    across(
      matches("^n_sch_(all|pub|priv)_(local|instate|inregion|national)$"),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  # per-university versions (average per university)
  mutate(
    across(
      matches("^n_sch_(all|pub|priv)_(local|instate|inregion|national)$"),
      ~ if_else(n_univ > 0, .x / n_univ, NA_real_),
      .names = "{.col}" # .names = "{.col}_per_univ"
    )
  ) %>% select(-n_univ)

n_sch_pair %>% glimpse()
# merge input datasets together
df_by_eps <- df_by_eps %>%
  left_join(df_by_eps_temp, by = "hs_eps_codename") %>%
  left_join(n_sch_pair, by = "hs_eps_codename") %>% 
  mutate(
    # 0/1 visit per school
    n_vis01_per_sch_all  = if_else(n_sch_all  > 0, n_vis01_all  / n_sch_all,  NA_real_),
    n_vis01_per_sch_pub  = if_else(n_sch_pub  > 0, n_vis01_pub  / n_sch_pub,  NA_real_),
    n_vis01_per_sch_priv = if_else(n_sch_priv > 0, n_vis01_priv / n_sch_priv, NA_real_),
    
    # total visits per school
    n_vistot_per_sch_all  = if_else(n_sch_all  > 0, n_vistot_all  / n_sch_all,  NA_real_),
    n_vistot_per_sch_pub  = if_else(n_sch_pub  > 0, n_vistot_pub  / n_sch_pub,  NA_real_),
    n_vistot_per_sch_priv = if_else(n_sch_priv > 0, n_vistot_priv / n_sch_priv, NA_real_)
  ) %>% 
  mutate(
    # ALL schools: 0/1 visit per school (by slice)
    n_vis01_per_sch_all_local     = if_else(n_sch_all_local    > 0, n_vis01_all_local    / n_sch_all_local,    NA_real_),
    n_vis01_per_sch_all_instate   = if_else(n_sch_all_instate  > 0, n_vis01_all_instate  / n_sch_all_instate,  NA_real_),
    n_vis01_per_sch_all_inregion  = if_else(n_sch_all_inregion > 0, n_vis01_all_inregion / n_sch_all_inregion, NA_real_),
    n_vis01_per_sch_all_national  = if_else(n_sch_all_national > 0, n_vis01_all_national / n_sch_all_national, NA_real_),
    
    # ALL schools: total visits per school (by slice)
    n_vistot_per_sch_all_local    = if_else(n_sch_all_local    > 0, n_vistot_all_local    / n_sch_all_local,    NA_real_),
    n_vistot_per_sch_all_instate  = if_else(n_sch_all_instate  > 0, n_vistot_all_instate  / n_sch_all_instate,  NA_real_),
    n_vistot_per_sch_all_inregion = if_else(n_sch_all_inregion > 0, n_vistot_all_inregion / n_sch_all_inregion, NA_real_),
    n_vistot_per_sch_all_national = if_else(n_sch_all_national > 0, n_vistot_all_national / n_sch_all_national, NA_real_),
    
    # PUBLIC: 0/1 visit per school (by slice)
    n_vis01_per_sch_pub_local     = if_else(n_sch_pub_local    > 0, n_vis01_pub_local    / n_sch_pub_local,    NA_real_),
    n_vis01_per_sch_pub_instate   = if_else(n_sch_pub_instate  > 0, n_vis01_pub_instate  / n_sch_pub_instate,  NA_real_),
    n_vis01_per_sch_pub_inregion  = if_else(n_sch_pub_inregion > 0, n_vis01_pub_inregion / n_sch_pub_inregion, NA_real_),
    n_vis01_per_sch_pub_national  = if_else(n_sch_pub_national > 0, n_vis01_pub_national / n_sch_pub_national, NA_real_),
    
    # PUBLIC: total visits per school (by slice)
    n_vistot_per_sch_pub_local    = if_else(n_sch_pub_local    > 0, n_vistot_pub_local    / n_sch_pub_local,    NA_real_),
    n_vistot_per_sch_pub_instate  = if_else(n_sch_pub_instate  > 0, n_vistot_pub_instate  / n_sch_pub_instate,  NA_real_),
    n_vistot_per_sch_pub_inregion = if_else(n_sch_pub_inregion > 0, n_vistot_pub_inregion / n_sch_pub_inregion, NA_real_),
    n_vistot_per_sch_pub_national = if_else(n_sch_pub_national > 0, n_vistot_pub_national / n_sch_pub_national, NA_real_),
    
    # PRIVATE: 0/1 visit per school (by slice)
    n_vis01_per_sch_priv_local     = if_else(n_sch_priv_local    > 0, n_vis01_priv_local    / n_sch_priv_local,    NA_real_),
    n_vis01_per_sch_priv_instate   = if_else(n_sch_priv_instate  > 0, n_vis01_priv_instate  / n_sch_priv_instate,  NA_real_),
    n_vis01_per_sch_priv_inregion  = if_else(n_sch_priv_inregion > 0, n_vis01_priv_inregion / n_sch_priv_inregion, NA_real_),
    n_vis01_per_sch_priv_national  = if_else(n_sch_priv_national > 0, n_vis01_priv_national / n_sch_priv_national, NA_real_),
    
    # PRIVATE: total visits per school (by slice)
    n_vistot_per_sch_priv_local    = if_else(n_sch_priv_local    > 0, n_vistot_priv_local    / n_sch_priv_local,    NA_real_),
    n_vistot_per_sch_priv_instate  = if_else(n_sch_priv_instate  > 0, n_vistot_priv_instate  / n_sch_priv_instate,  NA_real_),
    n_vistot_per_sch_priv_inregion = if_else(n_sch_priv_inregion > 0, n_vistot_priv_inregion / n_sch_priv_inregion, NA_real_),
    n_vistot_per_sch_priv_national = if_else(n_sch_priv_national > 0, n_vistot_priv_national / n_sch_priv_national, NA_real_)
  ) %>% 
  # university id and name
  mutate(
    univ_id = 'all',
    univ_classification = 'all',
    univ_abbrev = 'all',
    univ_usnwr_rank = 999
  )
# remove input dataframe
rm(df_by_eps_temp,n_sch_pair)

df_by_univ_eps %>% glimpse()
df_by_eps %>% glimpse()


#append df_by_univ_eps and df_by_eps
df_by_univ_eps <- bind_rows(
  df_by_univ_eps,
  df_by_eps
) %>% arrange(univ_id,hs_eps_codename) %>% 
  # merge in eps SES and demographic vars
  inner_join(
    y = allyr_anal_eps_sf %>% as_tibble() %>% filter(year ==2020) %>% 
      select(eps,eps_name,pct_nhisp_all,pct_hisp_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_other,pct_nhisp_asian,pct_nhisp_nhpi,pct_nhisp_multi,pct_nhisp_api,pct_hisp_api,mean_inc_house,med_inc_house,pct_pov_yes,pct_edu_baplus_all) %>% 
      mutate(hs_eps_codename = str_c(str_trim(eps), " - ", str_trim(eps_name)) |> as_factor()) %>% select(-c(eps,eps_name)),
    by = c('hs_eps_codename')
  )

df_by_univ_eps %>% glimpse()
