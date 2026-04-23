library(tidyverse)
library(leaflet)


scripts_dir <- file.path('.', 'scripts')

# Load HS visits data
source(file.path(scripts_dir, 'create_cb_geo_hs_visits.R'))

# Load EPS codes
source(file.path('..', 'cb_geo', 'scripts', 'metro_eps_codes.R'))

# Load map functions
source(file.path(scripts_dir, 'map_functions.R'))

# Load geomarket border distance data
hs_geomarket_distance_df <- readRDS(file.path('data', 'hs_geomarket_distance_df.RDS')) %>% 
  as.tibble()

View(pubprivhs_univ_df %>% select(hs_ncessch, hs_sch_name, univ_id, univ_name, hs_univ_market, univ_classification))

View(pubprivhs_univ_df %>% select(hs_ncessch, hs_control, hs_school_type) %>% distinct() %>% group_by(hs_control, hs_school_type) %>% summarise(n = n()))


create_map <- function(metro) {
  js <- read_file(file.path(scripts_dir, 'hs_visits_maps.js'))
  
  region <- regions_data %>% filter(region == metro)
  eps_codes <- region$eps[[1]]
  
  eps <- eps_data %>% filter(eps %in% eps_codes, year == 2020)
  tract <- tract_data %>% filter(eps %in% eps_codes, year == 2020)
  
  hs_visits <- pubprivhs_univ_df %>% filter(hs_eps %in% eps_codes, univ_id != 'all') %>% 
    mutate(has_visit = if_else(num_visits > 0, 1, 0))
  
  choices <- list(
    region_choices = region %>% select(region, region_name, latitude, longitude),
    univ_vars = univ_vars,
    market_vars = market_vars
  )
  
  highlight_shp <- highlightOptions(weight = 1, color = '#606060', dashArray = '')
  
  m <- leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0, zoomDelta = 0.5)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Metro outline
    addPolygons(data = eps, opacity = 1, color = 'purple', fillOpacity = 0, weight = 2, label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b>') %>% lapply(htmltools::HTML), group = 'MSA', options = c(className = paste0('metro-shape metro-', metro))) %>% 
    
    # EPS outline
    addPolylines(data = eps, opacity = 1, color = 'purple', weight = 2, options = c(className = paste0('metro-shape metro-line-', metro))) %>% 
    
    addPolygons(data = st_buffer(sf::st_set_geometry(hs_geomarket_distance_df %>% select(border, geometry) %>% filter(str_detect(border, str_c(eps_codes, collapse = '|'))) %>% distinct(), 'geometry') %>% st_transform(crs = 3857), dist = 2 * 1609.34) %>% st_transform(crs = 4326), color = 'gray', fillOpacity = 0.25, weight = 0, label = ~paste0('<b>', border, '</b>') %>% lapply(htmltools::HTML), options = c(className = 'metro-distance metro-distance-2')) %>% 
    addPolygons(data = st_buffer(sf::st_set_geometry(hs_geomarket_distance_df %>% select(border, geometry) %>% filter(str_detect(border, str_c(eps_codes, collapse = '|'))) %>% distinct(), 'geometry') %>% st_transform(crs = 3857), dist = 1 * 1609.34) %>% st_transform(crs = 4326), color = 'gray', fillOpacity = 0.25, weight = 0, label = ~paste0('<b>', border, '</b>') %>% lapply(htmltools::HTML), options = c(className = 'metro-distance metro-distance-1')) %>% 
    addPolygons(data = st_buffer(sf::st_set_geometry(hs_geomarket_distance_df %>% select(border, geometry) %>% filter(str_detect(border, str_c(eps_codes, collapse = '|'))) %>% distinct(), 'geometry') %>% st_transform(crs = 3857), dist = 0.5 * 1609.34) %>% st_transform(crs = 4326), color = 'gray', fillOpacity = 0.25, weight = 0, label = ~paste0('<b>', border, '</b>') %>% lapply(htmltools::HTML), options = c(className = 'metro-distance metro-distance-half')) %>% 
    
    addLabelOnlyMarkers(data = st_point_on_surface(eps), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b>') %>% lapply(htmltools::HTML), labelOptions = labelOptions(noHide = T, direction = 'top', className = 'label'))
  
  for (v in names(base_vars)) {
    
    color_pal_tract <- get_palette(v, tract[[v]], 'tract')
    
    group_name <- if_else(str_detect(base_vars[[v]]$name, 'Hispanic'), base_vars[[v]]$name, paste0('MSA by ', base_vars[[v]]$name))
    
    m <- m %>% 
      
      # Shapes
      addPolygons(data = tract, opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_tract$palette(get(v)), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b><br><b>Tract ', tract_code, '</b>: ', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro))) %>%
      
      addLegend(data = tract,
                position = 'topright', pal = color_pal_tract$palette, values = ~get(v),
                title = base_vars[[v]]$name,
                className = paste0('info legend legend-', base_vars[[v]]$abbrev, '-', metro),
                labFormat = color_pal_tract$label_format,
                na.label = 'N/A',
                opacity = 1)
  }
  
  for (univ_type in univ_vars$abbrev) {
    
    if (univ_type == 'all') {
      hs_by_univ <- hs_visits
    } else if (univ_type == 'private') {
      hs_by_univ <- hs_visits %>% filter(univ_classification %in% c('private_national', 'private_libarts'))
    } else {
      hs_by_univ <- hs_visits %>% filter(univ_classification == univ_type)
    }
    
    for (market_type in market_vars$abbrev) {
      if (market_type == 'all') {
        hs <- hs_by_univ
      } else if (market_type == 'all_in') {
        hs <- hs_by_univ %>% filter(hs_univ_market %in% c('local', 'in_state'))
      } else if (market_type == 'all_out') {
        hs <- hs_by_univ %>% filter(hs_univ_market %in% c('regional', 'national'))
      } else {
        hs <- hs_by_univ %>% filter(hs_univ_market == market_type)
      }
      
      hs <- hs %>% 
        group_by(
          hs_control, hs_school_type, hs_ncessch, hs_sch_name, hs_state_code, hs_overall_niche_letter_grade, hs_religion_5,
          hs_eps_region, hs_eps, hs_eps_name, hs_geometry, hs_pct_free_reduced_lunch,
          hs_tot_students, hs_pct_white, hs_pct_black, hs_pct_hispanic, hs_pct_asian, hs_pct_nativehawaii, hs_pct_amerindian, hs_pct_tworaces
        ) %>%
        summarise(
          n_univs = sum(has_visit),
          total_visits = sum(num_visits),
          visiting_univs = str_c(if_else(num_visits == 0, '', str_c(univ_abbrev, ': ', num_visits)), collapse = ',') %>% str_replace_all(',+', ',') %>% str_replace_all('^,+|,+$', ''),
          .groups = 'drop'
        ) %>%
        mutate(
          hs_label = paste0(
            '<b>', hs_sch_name, '</b><br>',
            'School Control: ', str_to_sentence(hs_control), '<br>',
            'School Type: ', str_to_sentence(hs_school_type), '<br>',
            if_else(hs_control == 'private', paste0('Religion: ', str_to_title(if_else(hs_religion_5 == 'other_religion', 'Other', hs_religion_5)) %>% str_replace('_', ' '), '<br>'), ''),
            'Niche Ranking: ', if_else(hs_overall_niche_letter_grade == 'unrank_na', 'NA', hs_overall_niche_letter_grade), '<br>',
            'Region: ', str_to_sentence(hs_eps_region), '<br><br>',
            '<b>Total Enrollment</b>: ', format(hs_tot_students, big.mark = ',', trim = T), '<br>',
            '<ul><li>% White: ', sprintf('%.1f', hs_pct_white), '</li>',
            '<li>% Black: ', sprintf('%.1f', hs_pct_black), '</li>',
            '<li>% Hispanic: ', sprintf('%.1f', hs_pct_hispanic), '</li>',
            '<li>% Asian: ', sprintf('%.1f', hs_pct_asian), '</li>',
            '<li>% NHPI: ', sprintf('%.1f', hs_pct_nativehawaii), '</li>',
            '<li>% AIAN: ', sprintf('%.1f', hs_pct_amerindian), '</li>',
            '<li>% 2+ Races: ', sprintf('%.1f', hs_pct_tworaces), '</li></ul>',
            if_else(hs_control == 'public', paste0('<br><b>% Free/Reduced Lunch</b>: ', sprintf('%.1f', hs_pct_free_reduced_lunch), '<br>'), ''),
            if_else(
              total_visits > 0,
              paste0(
                '<br><b>Total Visits</b>: ', total_visits, ' by ', n_univs, ' ', if_else(n_univs > 1, 'universities', 'university'), '<br>',
                '<ul><li>', str_replace_all(visiting_univs, ',', '</li><li>'), '</li></ul>'
              ),
              ''
            )
          )
        )
      
      m <- m %>% 
        # Add public HS
        addCircleMarkers(data = sf::st_set_geometry(hs %>% filter(hs_control == 'public', n_univs > 0), 'hs_geometry'), popup = ~hs_label, weight = 1, opacity = 1, fillOpacity = 0, color = 'blue', radius = ~sqrt(n_univs) + 2, group = 'Visited Public High Schools', options = pathOptions(className = paste0('hs-pin hs-', univ_type, '-', market_type))) %>%
        addCircleMarkers(data = sf::st_set_geometry(hs %>% filter(hs_control == 'public', n_univs == 0), 'hs_geometry'), popup = ~hs_label, weight = 1, opacity = 1, fillOpacity = 0.5, fillColor = 'blue', color = 'red', radius = ~sqrt(n_univs) + 2, group = 'Non-Visited Public High Schools', options = pathOptions(className = paste0('hs-pin hs-', univ_type, '-', market_type))) %>%

        # Add private HS
        addCircleMarkers(data = sf::st_set_geometry(hs %>% filter(hs_control == 'private', n_univs > 0), 'hs_geometry'), popup = ~hs_label, weight = 1, opacity = 1, fillOpacity = 0, color = '#ffa01c', radius = ~sqrt(n_univs) + 2, group = 'Visited Private High Schools', options = pathOptions(className = paste0('hs-pin hs-', univ_type, '-', market_type))) %>%
        addCircleMarkers(data = sf::st_set_geometry(hs %>% filter(hs_control == 'private', n_univs == 0), 'hs_geometry'), popup = ~hs_label, weight = 1, opacity = 1, fillOpacity = 0.5, fillColor = '#ffa01c', color = 'red', radius = ~sqrt(n_univs) + 2, group = 'Non-Visited Private High Schools', options = pathOptions(className = paste0('hs-pin hs-', univ_type, '-', market_type)))
    }
  }
  
  m %>% 
    
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', flatten_chr(map(base_vars, \(x) if_else(str_detect(x$name, 'Hispanic'), x$name, str_c('MSA by ', x$name))))),
      overlayGroups = c('Visited Public High Schools', 'Visited Private High Schools', 'Non-Visited Public High Schools', 'Non-Visited Private High Schools'),
      options = layersControlOptions(collapsed = F)
    ) %>% 
    htmlwidgets::onRender(js, choices)
}


for (region in regions_data$region) {
  print(region)
  saveWidget(create_map(region), str_c('./results/maps/rq0_map_', region, '.html'), background = 'transparent', selfcontained = T)
}
