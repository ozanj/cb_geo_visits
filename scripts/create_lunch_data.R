library(tidyverse)


# Data source: https://nces.ed.gov/ccd/files.asp

data_dir <- file.path('data')


# --------
# 2014-15
# --------

# Lunch Program Eligibility
lunch_1415 <- read_tsv(file.path(data_dir, 'ccd_sch_033_1415_w_0216161a.txt'), na = c('', 'NA', '-1')) %>%  # -1 denotes missing, not available, or not reported data items, according to 2014-15_CCD_Companion_SCH_Free_Lunch.xlsx
  rename_with(tolower)

# Variables include:
  # `frelch`: Count of students eligible to participate in the Free Lunch Program under the National School Lunch Act
  # `redlch`: Count of students eligible to participate in the Reduced-Price Lunch Program under the National School Lunch Act
  # `totfrl`: Total of free lunch eligible and reduced-price lunch eligible
  #lunch_1415 %>% select(ncessch, frelch, redlch, totfrl) %>% View()

# Confirm `totfrl` is always `frelch` + `redlch`
  #lunch_1415 %>% select(ncessch, frelch, redlch, totfrl) %>% mutate(totfrl_calc = frelch + redlch) %>% filter(totfrl != totfrl_calc) %>% View()


# Membership
membership_1415 <- read_tsv(file.path(data_dir, 'ccd_sch_052_1415_w_0216161a.txt')) %>% 
  rename_with(tolower)

# Variables include:
  # `total`: Total students, all grades (includes Adult Education)
  # `member`: Total elementary/ secondary students (excludes Adult Education)
  #membership_1415 %>% select(ncessch, total, member) %>% View()

# `total` and `member` generally the same except for 25 schools
  # membership_1415 %>% select(ncessch, total, member) %>% filter(total != member, total >= 0, member >= 0) %>% View()


# Merge lunch and membership tables
lunch_membership_1415 <- lunch_1415 %>% select(ncessch, frelch, redlch, totfrl) %>% 
  left_join(membership_1415 %>% select(ncessch, total, member), by = 'ncessch')

# 6 rows where number of free/reduced lunch is greater than total number of students in school
#lunch_membership_1415 %>% filter(member >= 0, totfrl > member) %>% View()

# Create percentage variables (exclude Adult Education in denominator?) - 3 rows where pct_free_reduced_lunch > 100
lunch_membership_1415 <- lunch_membership_1415 %>%
  rename(
    'free_lunch' = 'frelch',
    'reduced_lunch' = 'redlch',
    'free_reduced_lunch' = 'totfrl'
  ) %>% 
  filter(member > 0) %>%  # exclude rows where member is missing (i.e., -1 or -2) or 0 count
  mutate(
    pct_free_lunch = free_lunch / member * 100,
    pct_reduced_lunch = reduced_lunch / member * 100,
    pct_free_reduced_lunch = free_reduced_lunch / member * 100
  )
  rm(membership_1415,lunch_1415)
  
  lunch_membership_1415 %>% count(pct_free_reduced_lunch>100)

# --------
# 2017-18
# --------

# Membership [had to move file because to large for git repo]
membership_1718 <- read_csv(file.path('..','cb_geomarket_shape','ccd_school_membership_17_18', 'ccd_SCH_052_1718_l_1a_083118.csv')) %>% rename_with(tolower)
  #membership_1718 <- read_csv(file.path(data_dir, 'ccd_SCH_052_1718_l_1a_083118.csv')) %>% rename_with(tolower)

# `total_indicator` specifies different breakdowns of `student_count`, including:
  # Education Unit Total (similar to `total` in 2014-15 data above)
  # Derived - Education Unit Total minus Adult Education Count (similar to `member` in 2014-15 data above)
#membership_1718 %>% filter(total_indicator == 'Education Unit Total') %>% View()
#membership_1718 %>% filter(total_indicator == 'Derived - Education Unit Total minus Adult Education Count') %>% View()


# Lunch Program Eligibility
lunch_1718 <- read_csv(file.path(data_dir, 'ccd_sch_033_1718_l_1a_083118.csv')) %>%
  rename_with(tolower)

table(lunch_1718$lunch_program, useNA = 'always')

# `lunch_program` variable has these 5 possible values, always present for each school:
  # Free lunch qualified
  # Reduced-price lunch qualified
  # No Category Codes (usually the sum of 'Free lunch qualified' and 'Reduced-price lunch qualified' - see below)
  # Not Applicable (could be regular lunch? https://github.com/ozanj/cb_geo_visits/blob/04ee61ae9a677de00e8b9d1328d934f298ffe714/scripts/create_event_hs_data.R#L21-L25)
  # Missing (always either NA or 0)
table((lunch_1718 %>% filter(lunch_program == 'Missing'))$student_count, useNA = 'always')

# Remove rows where lunch_program == 'Missing' and rename variables
lunch_1718 <- lunch_1718 %>%
  filter(lunch_program != 'Missing') %>% 
  mutate(
    lunch_program = recode(
      lunch_program,
      'Free lunch qualified' = 'free_lunch',
      'Reduced-price lunch qualified' = 'reduced_lunch',
      'No Category Codes' = 'no_category_codes',
      'Not Applicable' = 'reg_lunch'
    )
  )

table(lunch_1718$lunch_program, useNA = 'always')

# `reg_lunch` is indeed missing for many rows...
lunch_1718 %>% select(lunch_program, student_count) %>% group_by(lunch_program) %>% summarise(sum(!is.na(student_count)))

# Pivot wider (1 row per school) and manually calculate `free_reduced_lunch`
lunch_1718 <- lunch_1718 %>% 
  pivot_wider(
    id_cols = ncessch,
    names_from = lunch_program,
    values_from = student_count
  ) %>% 
  mutate(
    free_reduced_lunch = free_lunch + reduced_lunch,
    total_lunch = free_reduced_lunch + reg_lunch
  )

# 'No Category Codes' is generally 'Free lunch qualified' + 'Reduced-price lunch qualified', but not always, so use manually calculated version
  #lunch_1718 %>% filter(free_reduced_lunch != no_category_codes) %>% View()


# Merge w/ membership data instead
lunch_membership_1718 <- lunch_1718 %>%
  select(ncessch, free_lunch, reduced_lunch, free_reduced_lunch) %>%
  left_join(membership_1718 %>% filter(total_indicator == 'Education Unit Total') %>% select(ncessch, student_count) %>% rename('total' = 'student_count'), by = 'ncessch') %>%
  left_join(membership_1718 %>% filter(total_indicator == 'Derived - Education Unit Total minus Adult Education Count') %>% select(ncessch, student_count) %>% rename('member' = 'student_count'), by = 'ncessch')

# 54 rows where number of free/reduced lunch is greater than total number of students in school
  #lunch_membership_1718 %>% filter(member >= 0, free_reduced_lunch > member) %>% View()

# Create percentage variables (exclude Adult Education in denominator?) - 28 rows where pct_free_reduced_lunch > 100
lunch_membership_1718 <- lunch_membership_1718 %>%
  filter(member > 0) %>%  # exclude rows where member is missing (i.e., -1 or -2) or 0 count
  mutate(
    pct_free_lunch = free_lunch / member * 100,
    pct_reduced_lunch = reduced_lunch / member * 100,
    pct_free_reduced_lunch = free_reduced_lunch / member * 100
  )

lunch_membership_1718 %>% glimpse()
lunch_membership_1718 %>% count(total == member)
lunch_membership_1718 %>% filter(total != member) %>% print(n=100)

rm(membership_1718,lunch_1718)

##### clean up datasets before saving

lunch_membership_1415 %>% glimpse()
lunch_membership_1718 %>% glimpse()

lunch_membership_1415 %>% count(pct_free_reduced_lunch>100) # 3 observations TRUE; 1435 NA; 92,205 obs FALSE
lunch_membership_1718 %>% count(pct_free_reduced_lunch>100) # 28 observations TRUE; 10,240 obs NA; 86,381 obs FALSE

lunch_membership_1415 <- lunch_membership_1415 %>% select(-c(total)) %>% rename(member_lunch =  member) %>% 
  # delete observations where percent free reduced lunch greater than 100
  # delete observations where percent free reduced lunch == NA
  filter(pct_free_reduced_lunch<=100)

lunch_membership_1718 <- lunch_membership_1718 %>% select(-c(total)) %>% rename(member_lunch = member) %>% 
  # delete observations where percent free reduced lunch greater than 100
  # delete observations where percent free reduced lunch == NA
  filter(pct_free_reduced_lunch<=100)
  
# save

getwd()
save(lunch_membership_1415,lunch_membership_1718, file = file.path('.','data','fr_lunch_1415_1718.RData'))

rm(lunch_membership_1415,lunch_membership_1718)

load(file = file.path('.','data','fr_lunch_1415_1718.RData'))

