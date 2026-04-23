regions_data <- data.frame(
  region = flatten_chr(map(names(all_codes), \(x) rep(x, length(all_codes[[x]]$eps)))),
  region_name = flatten_chr(map(names(all_codes), \(x) rep(all_codes[[x]]$name, length(all_codes[[x]]$eps)))),
  eps = flatten_chr(map(names(all_codes), \(x) all_codes[[x]]$eps))
) %>%
  left_join(allyr_anal_tract_sf %>% filter(year == 2020) %>% select(eps, geometry), by = 'eps') %>%
  group_by(region, region_name) %>%
  summarise(
    eps = list(unique(eps)),
    geometry = st_union(geometry),
    latitude = st_coordinates(st_centroid(geometry))[, 1],
    longitude = st_coordinates(st_centroid(geometry))[, 2],
    .groups = 'drop'
  )

base_vars <- list(
  sum_tot_all = list(name = 'Total Population', abbrev = 'pop'),
  pct_nhisp_white = list(name = '% White, non-Hispanic', abbrev = 'nhisp_white'),
  pct_nhisp_black = list(name = '% Black, non-Hispanic', abbrev = 'nhisp_black'),
  pct_hisp_all = list(name = '% Hispanic', abbrev = 'hisp_all'),
  pct_nhisp_asian = list(name = '% Asian, non-Hispanic', abbrev = 'nhisp_asian'),
  pct_nhisp_nhpi = list(name = '% NHPI, non-Hispanic', abbrev = 'nhisp_nhpi'),
  pct_nhisp_native = list(name = '% AIAN, non-Hispanic', abbrev = 'nhisp_native'),
  pct_nhisp_multi = list(name = '% 2+ Races, non-Hispanic', abbrev = 'nhisp_multi'),
  med_inc_house = list(name = 'Median Income', abbrev = 'income'),
  pct_pov_yes = list(name = '% in Poverty', abbrev = 'pov'),
  pct_edu_baplus_all = list(name = '% with BA+', abbrev = 'edu')
)

race_vars <- data.frame(
  abbrev = c('all', 'white', 'black', 'hispanic', 'asian', 'nhpi', 'aian', 'multi', 'other', 'norace'),
  name = c('All', 'White, non-Hispanic', 'Black, non-Hispanic', 'Hispanic', 'Asian, non-Hispanic', 'NHPI, non-Hispanic', 'AIAN, non-Hispanic', '2+ Races, non-Hispanic', 'Other', 'Unknown/Missing')
)

edu_vars <- data.frame(
  abbrev = c('all', 'nocollege', 'somecollege', 'nosomecollege', 'notfirstgen', 'noedu'),
  name = c('All', 'No college', 'Some college', 'No or some college', 'BA+', 'Unknown/Missing')
)

univ_vars <- data.frame(
  abbrev = c('all', 'public_research', 'private', 'private_national', 'private_libarts'),
  name = c('All', 'Public Research', 'Private', 'Private National', 'Private Liberal Arts')
)

market_vars <- data.frame(
  abbrev = c('all', 'all_in', 'local', 'in_state', 'all_out', 'regional', 'national'),
  name = c('All', 'All In-state', 'Local', 'In-state', 'All Out-of-state', 'Regional', 'National')
)

format_vars <- function(data_df) {
  data_df %>% 
    mutate(
      sum_tot_all_text = format(round(sum_tot_all), big.mark = ',', trim = T),
      med_inc_house_text = formattable::currency(med_inc_house, digits = 0L),
      pct_nhisp_white_text = if_else(is.na(pct_nhisp_white), 'NA', paste0(sprintf('%.1f', pct_nhisp_white), '%')),
      pct_nhisp_black_text = if_else(is.na(pct_nhisp_black), 'NA', paste0(sprintf('%.1f', pct_nhisp_black), '%')),
      pct_hisp_all_text = if_else(is.na(pct_hisp_all), 'NA', paste0(sprintf('%.1f', pct_hisp_all), '%')),
      pct_nhisp_asian_text = if_else(is.na(pct_nhisp_asian), 'NA', paste0(sprintf('%.1f', pct_nhisp_asian), '%')),
      pct_nhisp_nhpi_text = if_else(is.na(pct_nhisp_nhpi), 'NA', paste0(sprintf('%.1f', pct_nhisp_nhpi), '%')),
      pct_nhisp_native_text = if_else(is.na(pct_nhisp_native), 'NA', paste0(sprintf('%.1f', pct_nhisp_native), '%')),
      pct_nhisp_multi_text = if_else(is.na(pct_nhisp_multi), 'NA', paste0(sprintf('%.1f', pct_nhisp_multi), '%')),
      pct_pov_yes_text = if_else(is.na(pct_pov_yes), 'NA', paste0(sprintf('%.1f', pct_pov_yes), '%')),
      pct_edu_baplus_all_text = if_else(is.na(pct_edu_baplus_all), 'NA', paste0(sprintf('%.1f', pct_edu_baplus_all), '%'))
    )
}

eps_data <- allyr_anal_eps_sf %>% 
  filter(eps %in% flatten_chr(regions_data$eps)) %>%
  select(year, eps, eps_name, n_tracts, sum_tot_all, med_inc_house, pct_nhisp_white, pct_nhisp_black, pct_hisp_all, pct_nhisp_asian, pct_nhisp_nhpi, pct_nhisp_native, pct_nhisp_multi, pct_pov_yes, pct_edu_baplus_all, geometry) %>% 
  format_vars()

tract_data <- allyr_anal_tract_sf %>% 
  filter(eps %in% flatten_chr(regions_data$eps)) %>% 
  select(year, eps, eps_name, gisjoin, geoid, proportion, tot_all, med_inc_house, pct_nhisp_white, pct_nhisp_black, pct_hisp_all, pct_nhisp_asian, pct_nhisp_nhpi, pct_nhisp_native, pct_nhisp_multi, pct_pov_yes, pct_edu_baplus_all, geometry) %>% 
  rename('sum_tot_all' = 'tot_all') %>% 
  format_vars() %>% 
  mutate(
    state_code = if_else(is.na(gisjoin), str_sub(geoid, 1, 2), str_sub(gisjoin, 2, 3)),
    county_code = if_else(is.na(gisjoin), str_sub(geoid, 3, 5), str_sub(gisjoin, 5, 7)),
    tract_code = as.numeric(if_else(is.na(gisjoin), str_sub(geoid, 6), str_pad(str_sub(gisjoin, 9), width = 6, side = 'right', pad = '0'))) / 100
  )

get_palette <- function(variable, domain, level, pal = 'YlGnBu') {
  palette <- colorNumeric(pal, domain)
  label_format <- labelFormat()
  
  if (str_detect(variable, '_inc_')) {
    if (level == 'tract') {
      bins <- pretty(range(domain, na.rm = T), n = 6, min.n = 6)
    } else {
      bins <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 1000) * 1000
    }
    palette <- colorBin(pal, domain, bins)
    
    label_format <- function(type, cuts) {
      cuts <- cuts / 1000
      
      f <- cuts[[2]]
      l <- cuts[[length(cuts) - 1]]
      delta <- cuts[[2]] - cuts[[1]]
      
      cuts <- paste0('$', cuts, '-', cuts + delta - 1, 'k')
      cuts[[1]] <- paste0('<$', f, 'k')
      cuts[[length(cuts) - 1]] <- paste0('$', l, 'k+')
      cuts[[length(cuts)]] <- 'NA'
      
      cuts
    }
  } else if (str_detect(variable, 'pct_')) {
    bins <- pretty(range(0, max(domain, na.rm = T)), n = 5, min.n = 5)
    palette <- colorBin(pal, domain, bins)
    
    label_format <- function(type, cuts) {
      f <- cuts[[2]]
      l <- cuts[[length(cuts) - 1]]
      delta <- cuts[[2]] - cuts[[1]]
      d <- if_else(delta > 1, 1, 0)
      
      cuts <- paste0(cuts, '-', cuts + delta - d, '%')
      cuts[[length(cuts) - 1]] <- paste0(l, '%+')
      cuts[[length(cuts)]] <- 'NA'
      
      cuts
    }
  }
  
  list(palette = palette, label_format = label_format)
}
