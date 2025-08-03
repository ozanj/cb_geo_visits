library(tidyverse)


data_dir <- file.path('.', 'data')

# See: https://github.com/ksalazar3/recruiting-static-visuals/blob/master/update_achievement_data.py
percentage <- list(
  'LT50' = 25,
  'GT50' = 75,
  'GE50' = 75,
  'GE20' = 60,
  'LE20' = 10,
  '21-39' = 30,
  '40-59' = 49.5,
  '60-79' = 69.5,
  'GE80' = 90,
  'LE10' = 5,
  '11-19' = 15,
  '20-29' = 24.5,
  '30-39' = 34.5,
  '40-49' = 44.5,
  '50-59' = 54.5,
  '60-69' = 64.5,
  '70-79' = 74.5,
  '80-89' = 84.5,
  'GE90' = 95,
  'LE5' = 2.5,
  '6-9' = 7.5,
  '10-14' = 12,
  '15-19' = 17,
  '20-24' = 22,
  '25-29' = 27,
  '24-29' = 26.5,
  '30-34' = 32,
  '35-39' = 37,
  '40-44' = 42,
  '45-49' = 47,
  '50-54' = 52,
  '55-59' = 57,
  '60-64' = 62,
  '65-69' = 67,
  '70-74' = 72,
  '75-79' = 77,
  '80-84' = 82,
  '85-89' = 87,
  '90-94' = 92,
  'GE95' = 97.5,
  'LE1' = 0.5,
  'GE99' = 99.5
)

get_pct <- Vectorize(function(x) {
  if (is.na(x)) return(NA_real_)
  
  if (x %in% names(percentage)) return(percentage[[x]])
  
  return(as.numeric(x))
})

get_data <- function(file) {
  achievement_df <- read_csv(file.path(data_dir, file), na = c('', 'NA', '.', 'n/a', 'PS')) %>% 
    select(NCESSCH, matches('HSNUMVALID|HSPCTPROF')) %>% 
    pivot_longer(
      cols = -NCESSCH,
      names_to = c('group', '.value'),
      names_pattern = '([A-Z]+)_[A-Z]{3}([A-Z]+)_1415'
    ) %>% 
    rename_with(\(x) str_replace(str_to_lower(x), 'hs', '')) %>% 
    mutate(
      group = recode(
        group,
        'ALL' = 'all',  # All students in the school
        'MAM' = 'amerindian',  # American Indian/Alaska Native students
        'MAS' = 'asian',  # Asian/Pacific Islander students
        'MBL' = 'black',  # Black students
        'MHI' = 'hispanic',  # Hispanic students
        'MTR' = 'tworaces',  # Two or More Races
        'MWH' = 'white',  # White students
        'F' = 'female',  # Female students
        'M' = 'male',  # Male students
        'CWD' = 'disabled',  # Children with disabilities (IDEA)
        'ECD' = 'pov',  # Economically disadvantaged students
        'LEP' = 'esl',  # Limited English proficient students
        'HOM' = 'homeless',  # Homeless enrolled students
        'MIG' = 'migrant',  # Migrant students
      ),
      pct = get_pct(pctprof),
      numprof = numvalid * pct / 100
    ) %>% 
    select(-pctprof, -pct) %>% 
    pivot_wider(
      id_cols = 'ncessch',
      names_from = 'group',
      values_from = c('numvalid', 'numprof'),
      names_vary = 'slowest'
    )
}


math_df <- get_data('math-achievement-sch-sy2014-15.csv')
rla_df <- get_data('rla-achievement-sch-sy2014-15.csv')
