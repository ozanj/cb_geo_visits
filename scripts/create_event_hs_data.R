
library(igraph)
library(tidyverse)
library(labelled)
library(haven)

#rm(list = ls())

# read in data on free reduced lunch for 2017-18 academic year

pubhs_frlunch_1718 <- read_csv(file.path("data", "ccd_sch_033_1718_l_1a_083118.csv")) %>% rename_with(tolower) %>% 
  # filter out observations that are not needed
  filter(lunch_program != 'Not Applicable') %>% # these are students not on free-reduced lunch
  filter(lunch_program != 'Missing') %>%  # not sure what this obs is but it exists for each school
  select(ncessch,lunch_program,student_count) %>% 
  # recode lunch_program variable to be consistent with naming of 2014-15 free reduced lunch data
  mutate(lunch_program = case_when(
    lunch_program == 'Free lunch qualified' ~ 'free_lunch',
    lunch_program == 'Reduced-price lunch qualified' ~ 'reduced_lunch',
    lunch_program == 'No Category Codes' ~ 'free_reduced_lunch')
  )

pubhs_frlunch_1718 %>% glimpse()
pubhs_frlunch_1718 %>% group_by(ncessch) %>% summarise(n_per_group=n()) %>% ungroup %>% count(n_per_group) # ALWAYS 3 obs per school
pubhs_frlunch_1718 %>% group_by(ncessch,lunch_program) %>% summarise(n_per_group=n()) %>% ungroup %>% count(n_per_group) # ALWAYS 1 obs per school

pubhs_frlunch_1718 %>% count(lunch_program)
#pubhs_frlunch_1718 %>% select(statename,sch_name,ncessch,lunch_program,total_indicator,student_count) %>% print(n=50) # commented this out because I dropped some of the variables

# reshape from long to wide

pubhs_frlunch_1718 <- pubhs_frlunch_1718 %>%
  pivot_wider(
    names_from = lunch_program,   # values in `type` become variable names
    values_from = student_count  # values in `count` become values in the new `cases` and `population` cols
  )
pubhs_frlunch_1718

# this script was originally made from the recruiting chapter repo
setwd(dir = file.path('.','..','recruiting-chapter'))

## ----------
## LOAD DATA
## ----------

# Recruiting events data
# universities to exclude:
  # Wellesley college; data seem suspect
  # NC State; univ_id ==199193
    # rationale: data seem suspect
  # UC Irvine: univ_id == 110653 [5/23/2025 -- keeping this now]
    # rationale: we have four UCs: berkeley; san diego; irvine; riverside; don't want so many; Irvine and San Diego have same ranking but San Diego has more out-of-state visits and more private hs visits so keep that one; and keep riverside because it has very different rank

#events_data <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data_temp1 <- read.csv('./data/events_data_2020-10-20.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data_marquette <- read.csv('./data/events_data_marquette.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data <- events_data_temp1 %>% bind_rows(events_data_marquette) %>% 
  #filter(!(univ_id %in% c('168218','199193','110653')))  # Wellesley, NCSU, UCI
  filter(!(univ_id %in% c('168218','199193')))  # Wellesley, NCSU, UCI
rm(events_data_marquette,events_data_temp1)

# University data from IPEDS
univ_data <- readRDS('./data/ipeds_1718.RDS')
univ_ipeds <- read.csv('./data/ipeds_data.csv', header = TRUE, na.strings = c('', 'NULL', 'NA'), stringsAsFactors = FALSE, colClasses = c('unitid' = 'character')) %>% as_tibble() %>%
  filter(endyear == 2017)
univ_info <- read.csv('./data/univ_data.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'zip_code' = 'character')) %>% as_tibble() %>% 
  filter(!(univ_id %in% c('168218','199193','149222')))  # Wellesley, NCSU, UCI, SIU-Carbondale

# Public HS data from CCD
pubhs_data_1718 <- readRDS('./data/ccd_1718.RDS') %>% 
  # create 0/1 flag for magnet school. note: counted "missing" "not applicable" "not reported" and NA as 0.
  mutate(magnet01 = if_else(magnet_text == "Yes", 1,0, , missing = 0)) %>% select(-magnet_text) %>% 
  # create factor school type variable
  mutate(school_type = as_factor(sch_type)) %>% select(-c(sch_type,sch_type_text)) %>% 
  rename(zip5 = lzip)
  
  pubhs_data_1718 %>% count(school_type,magnet01) %>% print(n=50) # magnet schools are in every category of school type
  pubhs_data_1718 %>% count(school_type)
  pubhs_data_1718 %>% glimpse()
  
  pubhs_data_1718 %>% mutate(zip_len = str_length(zip5)) %>% count(zip_len) # always = 5 characters
  
  # racial percent variables are school-wide, not just 12th grade
  #pubhs_data_1718 %>% mutate(pct_white2 = total_white/total_students*100,pct_hispanic2 = total_hispanic/total_students*100) %>% select(sch_name,total_white,total_black,total_students,pct_white,pct_white2,pct_hispanic,pct_hispanic2) %>% View()
  
  # merge in 2017-18 free reduced lunch data
  pubhs_data_1718 %>% glimpse()
  pubhs_frlunch_1718 %>% glimpse()  
  
  pubhs_data_1718 <- pubhs_data_1718 %>% left_join(
    y = pubhs_frlunch_1718 %>% mutate(one = 1),
    by = c('ncessch')
  ) %>% select(-one) # %>% count(one) # note: all obs from pubhs_frlunch_1718 merge to pubhs_data_1718; but there are 3,388 obs from pubhs_data_1718 that are not in pubhs_frlunch_1718
  
  rm(pubhs_frlunch_1718)
  
pubhs_data_1415 <- read.csv('./data/meta_high_school_public.csv', header = TRUE, na.strings = c('', 'NA', 'NULL'), stringsAsFactors = FALSE, colClasses = c('ncessch' = 'character')) %>%  # original set used for merging
  as_tibble() %>% 
  mutate(  # total_students = wh + am + as + hi + bl + hp + tr (each is sum of 9-12 grades)
    pct_amerindian = am / total_students * 100,
    pct_asian = as / total_students * 100,
    pct_black = bl / total_students * 100,
    pct_hispanic = hi / total_students * 100,
    pct_nativehawaii = hp / total_students * 100,
    pct_tworaces = tr / total_students * 100,
    pct_white = wh / total_students * 100
  ) %>% 
  # create 0/1 flag for magnet school. note: counted "missing" "not applicable" "not reported" and NA as 0.
  mutate(magnet01 = if_else(magnet_text == "Yes", 1,0, , missing = 0))  %>% select(-c(magnet_text,magnet)) %>% 
  # school_type
  select(-school_type) %>% 
  mutate(
    school_type = case_when(
      str_to_lower(str_trim(school_type_text)) == "regular school"              ~ "regular school",
      str_to_lower(str_trim(school_type_text)) == "special education school"    ~ "special education school",
      str_to_lower(str_trim(school_type_text)) == "alternative education school"~ "alternative education school",
      str_to_lower(str_trim(school_type_text)) == "vocational education school" ~ "career and technical school",
      TRUE                                                                      ~ NA_character_
    ),
    school_type = factor(school_type,
                         levels = levels(pubhs_data_1718$school_type))
  ) %>% rename(zip5 = l_zip_code) %>%  # note: l_zip_code is location zip code; m_zip_code is mailing zip code
  mutate(
    zip5 = str_pad(as.character(zip5), width = 5, pad = "0")
  )

pubhs_data_1415 %>% glimpse()

pubhs_data_1415 %>% mutate(zip_len = str_length(zip5)) %>% count(zip_len)

# Private HS data from PSS
privhs_data_1718 <- readRDS('./data/pss_1718.RDS')
privhs_data_1516 <- readRDS('./data/pss_1516.RDS')
privhs_data_1314 <- readRDS('./data/pss_1314.RDS')
privhs_overrides <- read.csv('./data/pss_updated_id.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()

privhs_data_1314 %>% glimpse()
privhs_data_1516 %>% glimpse()
privhs_data_1718 %>% glimpse()

privhs_data_1314 %>% filter(total_12 >= 10) %>% count(school_type)
privhs_data_1516 %>% filter(total_12 >= 10) %>% count(school_type)
privhs_data_1718 %>% filter(total_12 >= 10) %>% count(school_type)

# Rankings data from Niche
niche_data <- read_csv('./data/niche_private.csv', col_types = list('ncessch' = 'c', 'ceeb' = 'c')) %>% as_tibble()
niche_extra_data <- read_csv('./data/niche_private_middlehigh_2021.csv', col_types = list('ncessch' = 'c')) %>% as_tibble()
niche_overrides <- read_csv('./data/niche_updated_id.csv', col_types = list('ncessch' = 'c')) %>% as_tibble()

niche_data <- niche_data %>% bind_rows(niche_extra_data %>% filter(ncessch == 'A9700305'))
rm(niche_extra_data)

niche_traditional <- read_csv('../cb_geo_visits/data/niche_traditional.csv', col_types = list('ncessch' = 'c')) %>% as_tibble()
niche_charter <- read_csv('../cb_geo_visits/data/niche_charter.csv', col_types = list('ncessch' = 'c')) %>% as_tibble()
niche_magnet <- read_csv('../cb_geo_visits/data/niche_magnet.csv', col_types = list('ncessch' = 'c')) %>% as_tibble()

# Filter out 52 overlapping schools between charter and magnet from magnet to get rid of duplicates
niche_magnet <- niche_magnet %>% filter(!ncessch %in%intersect(niche_charter$ncessch, niche_magnet$ncessch))

niche_public <- bind_rows(niche_traditional, niche_charter, niche_magnet)

# Rankings data from US News & World Report
usnews_data <- read.csv('./data/usnews_rankings.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character')) %>% as_tibble()


## ----------
## PREP DATA
## ----------

# Add overrides for miscategorized events
# View(events_data %>% filter(!(pid %in% c(55696, 44420, 43964, 30678, 25278, 27707, 52235, 32037, 34938, 82251, 43944, 46788, 84552, 64238, 25637, 63281, 68942, 44224, 24830)) | !(school_id %in% c('00299041', 'A0109336', 'A0701311', 'A0702146', 'A0901459', 'A1192055', 'A1303450', 'A9101558', 'A9101599', 'A9105352', 'A9300600', 'BB060710', 'BB161076'))))
table(events_data$event_type, useNA = 'always')

events_data$event_type[events_data$pid %in% c(55696, 44420, 43964, 30678, 25278, 27707, 32037, 82251, 46788, 63281, 68942, 24830, 33944) | events_data$school_id %in% c('00299041', 'A0109336', 'A0701311', 'A0901459', 'A1303450', 'A9101599', 'A9300600')] <- 'pub_hs'
events_data$school_id[events_data$pid == 55696] <- '340126002704'  # Bayonne High School
events_data$school_id[events_data$pid == 44420] <- '170993006467'  # Acero Chtr Netrk - Major Garcia
events_data$school_id[events_data$pid == 43964] <- '170993006454'  # Noble St Chtr-hansberry Prep Slvr
events_data$school_id[events_data$pid == 30678] <- '220117000939'  # Mcdonogh #35 College Preparatory School
events_data$school_id[events_data$pid == 25278] <- '340840003152'  # Lawrence High School
events_data$school_id[events_data$pid == 27707] <- '080002006437'  # Thomas Maclaren State Charter School
events_data$school_id[events_data$pid == 32037] <- '069100514059'  # Contra Costa School Of Performing Arts
events_data$school_id[events_data$pid == 82251] <- '080687001171'  # Telluride High School
events_data$school_id[events_data$pid == 46788] <- '420006100503'  # Philadelphia Academy Cs
events_data$school_id[events_data$pid == 63281] <- '481134000612'  # Bridgeport H S
events_data$school_id[events_data$pid == 68942] <- '450390101620'  # Green Charter School
events_data$school_id[events_data$pid == 24830] <- '422109006413'  # Scranton HS
events_data$school_id[events_data$pid == 33944] <- '401059000474'  # Memorial HS
events_data$school_id[events_data$school_id == '00299041'] <- '130012004145'  # Atlanta Classical Academy
events_data$school_id[events_data$school_id == 'A0109336'] <- '481527013128'  # Veterans Memorial H S
events_data$school_id[events_data$school_id == 'A0701311'] <- '251143001887'  # Swampscott High
events_data$school_id[events_data$school_id == 'A0901459'] <- '170993006499'  # Noble St Chtr Rauner College Prep
events_data$school_id[events_data$school_id == 'A1303450'] <- '482364002483'  # Debakey H S For Health Prof
events_data$school_id[events_data$school_id == 'A9101599'] <- '080336006441'  # VENTURE PREP HIGH SCHOOL
events_data$school_id[events_data$school_id == 'A9300600'] <- '340075103180'  # Great Oaks Legacy Charter School

events_data$event_type[events_data$school_id == 'A9101558'] <- 'pub_4yr'
events_data$ipeds_id[events_data$school_id == 'A9101558'] <- '126775'  # Colorado School of Mines

events_data$event_type[events_data$pid %in% c(52235, 43944, 84552, 64238) | events_data$school_id == 'BB161076'] <- 'other'  # non-profit org, outreach program, etc.
events_data$school_id[events_data$pid %in% c(52235, 43944, 84552, 64238) | events_data$school_id %in% c('BB161076', 'A9101558')] <- NA

events_data <- events_data %>% filter(!(pid %in% c(34938, 44224, 33941, 25637)), !(school_id %in% c('A0702146', 'A1192055', 'BB060710')))  # private HS w/ no NCES ID
table(events_data$event_type, useNA = 'always')


# Clean up events data (drop visits to HS not meeting criteria - 45533 to 44660 obs)
events_df <- events_data %>% 
  select(univ_id, univ_state, event_type, school_id, event_date, event_state, event_loc)

# Override school_id with updated 2017-18 PSS ID
get_pss_override <- function(x) {
  new_id <- privhs_overrides[privhs_overrides$old_id == x, ]$new_id
  ifelse(length(new_id) == 0, x, new_id)
}
v_get_pss_override <- Vectorize(get_pss_override, USE.NAMES = FALSE)

events_df <- events_df %>% mutate(school_id = v_get_pss_override(school_id))


# Combine 2017-18 PSS data w/ past years as needed
pss_missing_ncessch <- setdiff(events_df$school_id[events_df$event_type == 'priv_hs'], privhs_data_1718$ncessch)
pss_1516_ncessch <- privhs_data_1516[privhs_data_1516$ncessch %in% pss_missing_ncessch, ]$ncessch
pss_1314_ncessch <- setdiff(pss_missing_ncessch, pss_1516_ncessch)

privhs_data <- privhs_data_1718 %>%
  dplyr::union(privhs_data_1516 %>% filter(ncessch %in% pss_1516_ncessch)) %>%
  dplyr::union(privhs_data_1314 %>% filter(ncessch %in% pss_1314_ncessch))


# Verify no unmerged pss id
setdiff(events_df$school_id[events_df$event_type == 'priv_hs'], privhs_data$ncessch)

# Criteria for private HS to be included in our analysis is 12th grade enrollment of 10 or more (we're no longer restricting by school type)
# This is the universe of private HS in our analysis (23184 to 4439 obs) - all 2017-18 schools + any previous year's schools that were visited
privhs_data <- privhs_data %>% filter(total_12 >= 10)

privhs_data %>% group_by(year) %>% count()  # 4168 from 2017-18, 116 from 2015-16, 155 from 2013-14


# Filter private HS events to only ones that meet criteria (13880 to 13459 obs)
events_df <- events_df %>% filter(event_type != 'priv_hs' | school_id %in% privhs_data$ncessch)
table(events_df$event_type, useNA = 'always')


pubhs_data_1718 %>% glimpse()
pubhs_data_1415 %>% glimpse()
# Determine public HS that meet criteria (use 2017-18 if available, otherwise original 2014-15 dataset)
ccd_missing_ncessch <- setdiff(events_df$school_id[events_df$event_type == 'pub_hs'], pubhs_data_1718$ncessch)
ccd_meet_criteria_1718 <- pubhs_data_1718 %>%
  filter(g_12_offered == 'Yes', g12 >= 10, virtual %in% c('NOTVIRTUAL', 'SUPPVIRTUAL'), fipst < 60, updated_status %in% c('1', '3', '8')) %>% 
  mutate(year = '1718') %>% 
  select(year, ncessch, state_code, g12, pct_amerindian, pct_asian, pct_black, pct_hispanic, pct_nativehawaii, pct_tworaces, pct_white,latitude,
         longitude,sch_name,magnet01,school_type,zip5, free_lunch, reduced_lunch, free_reduced_lunch)

# Verify no unmerged ccd id
setdiff(ccd_missing_ncessch, pubhs_data_1415$ncessch)
ccd_meet_criteria_1415 <- pubhs_data_1415 %>%
  filter(g12offered == 1, g12 >= 10, virtual == 0, state_fips_code < 60, updated_status %in% c(1, 3, 8)) %>% 
  mutate(year = '1415') %>% 
  select(year, ncessch, state_code, g12, pct_amerindian, pct_asian, pct_black, pct_hispanic, pct_nativehawaii, pct_tworaces, pct_white,latitude,
         longitude,name,magnet01,school_type,zip5,free_lunch, reduced_lunch, free_reduced_lunch) %>% 
  rename(sch_name = name)

# Universe of public HS meeting criteria (20809 obs)
pubhs_data <- ccd_meet_criteria_1718 %>%
  dplyr::union(ccd_meet_criteria_1415 %>% filter(ncessch %in% ccd_missing_ncessch))
rm(ccd_meet_criteria_1718, ccd_meet_criteria_1415)

pubhs_data %>% group_by(year) %>% count()  # 20756 from 2017-18, 53 from 2014-15

# Filter public HS events to only ones that meet criteria (24397 to 23945 obs)
events_df <- events_df %>% filter(event_type != 'pub_hs' | school_id %in% pubhs_data$ncessch)
table(events_df$event_type, useNA = 'always')

# Add race & enroll categorical variables to public HS data
pubhs_data <- pubhs_data %>% mutate(
  pct_white_cat = case_when(
    pct_white < 50 ~ 'c1_lt50',
    pct_white < 75 ~ 'c2_50to75',
    pct_white < 85 ~ 'c3_75to85',
    pct_white >= 85 ~ 'c4_85+'
  ),
  pct_blacklatinxnative = pct_black + pct_hispanic + pct_amerindian + pct_nativehawaii,
  pct_blacklatinxnative_cat = case_when(
    pct_blacklatinxnative < 10 ~ 'c1_lt10',
    pct_blacklatinxnative < 20 ~ 'c2_10to20',
    pct_blacklatinxnative < 50 ~ 'c3_20to50',
    pct_blacklatinxnative >= 50 ~ 'c4_50+'
  ),
  enroll_cat1 = case_when(
    g12<50  ~ 'c1_lt50',
    g12>=50 & g12<100  ~ 'c2_50to100',
    g12>=100 & g12<150  ~ 'c3_100to150',
    g12>=150  ~ 'c4_gt150'
  ),
  enroll_cat2 = case_when(
    g12<100  ~ 'c1_lt100',
    g12>=100 & g12<150  ~ 'c2_100to150',
    g12>=150 & g12<200  ~ 'c3_150to200',
    g12>=200  ~ 'c4_gt200'
  )
)

# Save dataframes
saveRDS(pubhs_data, file = str_c('./data/pubhs_universe.RDS'))
saveRDS(events_df %>% left_join(univ_info %>% select(univ_id, univ_abbrev, classification), by ='univ_id'), file = str_c('./data/events_data.RDS'))


# Add ranking from Niche data
niche_df <- bind_rows(niche_data, niche_public) %>% mutate(overall_niche_letter_grade = case_when(
  overall_niche_grade == 4.33 ~ 'A+',
  overall_niche_grade == 4 ~ 'A',
  overall_niche_grade == 3.66 ~ 'A-',
  overall_niche_grade == 3.33 ~ 'B+',
  overall_niche_grade == 3 ~ 'B',
  overall_niche_grade == 2.66 ~ 'B-',
  overall_niche_grade == 2.33 ~ 'C+',
  overall_niche_grade == 2 ~ 'C',
  overall_niche_grade == 1.66 ~ 'C-',
  TRUE ~ 'Unranked'
))

# Universe of private HS - NCES data + Niche data
privhs_data %>% count(school_type)
privhs_df <- privhs_data %>% left_join(dplyr::union(
    niche_df %>% select(ncessch, overall_niche_letter_grade, rank_within_category) %>% distinct(),
    niche_overrides %>% left_join(niche_df %>% select(guid, overall_niche_letter_grade, rank_within_category), by = 'guid') %>% select(-guid)
  ), by = 'ncessch'
  ) %>%
  mutate(type = 'priv hs', control = 'private') %>% rename(zip5 = zip_code)

# Universe of public HS - NCES data + Niche data
pubhs_data %>% count(school_type)

pubhs_data %>% left_join(niche_df, by = 'ncessch') %>% filter(is.na(overall_niche_letter_grade)) %>% View()  # check unmerged rows (143 rows)

niche_overrides_public <- niche_df %>%
  select(ncessch, name, state_code, guid, overall_niche_letter_grade, rank_within_category) %>%
  mutate(ncessch = case_when(
    ncessch == '060134212511' ~ '062515012511',  # Aspire Vanguard College Preparatory Academy
    guid == '0a58d345-4592-4bb1-95aa-4236256f00a6' ~ '090537311223',  # The Woodstock Academy
    ncessch == '050717000457' ~ '050741000457',  # Hartford High School
    ncessch == '050041900919' ~ '051185000919',  # JACKSONVILLE HIGH SCHOOL
    ncessch == '069107811842' ~ '062271011842',  # Magnolia Science Academy 2
    ncessch == '220026102277' ~ '220004302277',  # Lake Area New Tech Early College High School
    ncessch == '220018600981' ~ '220004400981',  # Sophie B. Wright Institute of Academic Excellence
    ncessch == '220019700953' ~ '220004500953',  # KIPP Renaissance High School
    ncessch == '220028100234' ~ '220117000234',  # New Orleans Charter Science and Mathematics HS
    ncessch == '220029900888' ~ '220117000888',  # Benjamin Franklin High School
    ncessch == '220029400911' ~ '220117000911',  # Edna Karr High School
    ncessch == '220029700926' ~ '220117000926',  # Lusher Charter School
    ncessch == '220029100945' ~ '220117000945',  # Eleanor McMain Secondary School
    ncessch == '260110304669' ~ '261200004669',  # Cass Technical High School
    ncessch == '260110304906' ~ '261200004906',  # Renaissance High School
    ncessch == '420459007554' ~ '420459001147',  # Butler Area SHS
    ncessch == '420806007559' ~ '420806005193',  # East Allegheny HS
    ncessch == '420994007586' ~ '420994001191',  # Forest Hills HS
    ncessch == '422613007578' ~ '422613001293',  # Westmont Hilltop HS
    ncessch == '450390100460' ~ '450231000460',  # Greenville Technical Charter High
    ncessch == '480003012139' ~ '480026912139',  # UPLIFT EDUCATION-PEAK PREP HS
    ncessch == '480003012200' ~ '480139912200',  # UPLIFT EDUCATION-HAMPTON PREP HS
    ncessch == '480003012175' ~ '480140112175',  # SUMMIT INTERNATIONAL PREPARATORY
    ncessch == '480003012012' ~ '480140612012',  # UPLIFT EDUCATION-WILLIAMS PREP HS
    ncessch == '500039500120' ~ '500000600120',  # Essex Community Education Center UHSD #46
    ncessch == '500039600082' ~ '500303000082',  # Champlain Valley UHSD #15
    ncessch == '500040300148' ~ '500458000148',  # Harwood UHSD #19
    ncessch == '500040200189' ~ '500552000189',  # Middlebury Senior UHSD #3
    ncessch == '500038800210' ~ '500584000210',  # Mt. Mansfield USD #17
    ncessch == '500039200234' ~ '500630000234',  # Otter Valley UHSD #8
    ncessch == '500930000539' ~ '500930000386'  # Windsor High School
  )) %>% 
  filter(!is.na(ncessch)) %>%
  select(-name, -state_code, -guid) %>% 
  add_row(ncessch = '090537301223', overall_niche_letter_grade = (niche_df %>% filter(guid == '0a58d345-4592-4bb1-95aa-4236256f00a6'))$overall_niche_letter_grade, rank_within_category = (niche_df %>% filter(guid == '0a58d345-4592-4bb1-95aa-4236256f00a6'))$rank_within_category)  # Woodstock Academy from 14-15 data

pubhs_df <- pubhs_data %>% left_join(dplyr::union(
    niche_df %>% select(ncessch, overall_niche_letter_grade, rank_within_category) %>% distinct(),
    niche_overrides_public
  ), by = 'ncessch'
) %>%
  mutate(type = 'pub hs', control = 'public')

pubhs_df %>% filter(is.na(overall_niche_letter_grade)) %>% View()  # check unmerged rows (111 rows)

# Add ranking from US News & World Report data
usnews_df <- usnews_data %>%
  mutate(type = recode(source,
                       'national-liberal-arts-colleges' = 'lib arts',
                       'national-universities' = 'univ'
                       )) %>%
  select(univ_id, type, control, score_text, rank)

# Create variables from IPEDS data
univ_ipeds <- univ_ipeds %>% 
  mutate(
    pgrnt_p = pgrnt_n / cohortsfaef * 100,
    pctfreshwh = ugftptfreshwhmf / ugftptfreshtot * 100,
    pctfreshbl = ugftptfreshblmf / ugftptfreshtot * 100,
    pctfreshap = ugftptfreshapmf / ugftptfreshtot * 100,  # ap = as + nh
    pctfreshhi = ugftptfreshhimf / ugftptfreshtot * 100,
    pctfreshal = ugftptfreshalmf / ugftptfreshtot * 100,
    pctfreshmr = ugftptfreshmrmf / ugftptfreshtot * 100,
    pctfreshna = ugftptfreshnamf / ugftptfreshtot * 100,
    pctfreshun = ugftptfreshunmf / ugftptfreshtot * 100,
    pctfreshas = ugftptfreshasmf / ugftptfreshtot * 100,
    pctfreshnh = ugftptfreshnhmf / ugftptfreshtot * 100
  )

# Sample of universities - IPEDS data + USNWR data
univ_df <- univ_info %>% select(univ_id, univ_abbrev, classification, state_code) %>%
  left_join(univ_data %>% select(-control, -state_code), by = 'univ_id') %>% 
  left_join(univ_ipeds %>% select(-locale, -sector), by = c('univ_id' = 'unitid')) %>% 
  left_join(usnews_df, by = 'univ_id')


# Select variables of interest from univ sample data to build attributes_df
univ_df <- univ_df %>%
  select(univ_id, univ_abbrev, city, state_code, region, religion, pctfreshwh, pctfreshbl, pctfreshhi, pctfreshap, pctfreshna, pctfreshnh, pctfreshmr, ugftptfreshtot, type, control, score_text, rank)

