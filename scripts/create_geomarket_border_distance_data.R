library(tidyverse)


source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))


# Load 2020 EPS shapes
y2020_anal_eps_sf <- allyr_anal_eps_sf %>% filter(year == 2020)

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = y2020_anal_eps_sf, label = ~eps, weight = 1, opacity = 1)


# Identify borders between all pairs of geomarkets
geomarket_borders <- st_touches(y2020_anal_eps_sf, retain_unique = T)

geomarket_borders_df <- st_sf(
  geomarket_1 = character(),
  geomarket_2 = character(),
  geometry = st_sfc(),
  stringsAsFactors = F
)

for (i in seq(geomarket_borders)) {
  # Manually add adjacent geomarkets not detected by st_touches()
  extra_geomarkets <- c()
  if (y2020_anal_eps_sf$eps[[i]] == 'CA11') extra_geomarkets <- match('CA32', y2020_anal_eps_sf$eps)
  if (y2020_anal_eps_sf$eps[[i]] == 'TX13') extra_geomarkets <- match(c('TX 4', 'TX 6', 'TX14'), y2020_anal_eps_sf$eps)
  if (y2020_anal_eps_sf$eps[[i]] == 'PA13') extra_geomarkets <- match(c('PA 9', 'PA12', 'WV 2'), y2020_anal_eps_sf$eps)
  
  for (j in c(geomarket_borders[[i]], extra_geomarkets)) {
    border <- tryCatch(
      ms_innerlines(y2020_anal_eps_sf[c(i, j), ]),
      error = function(e) {
        message(str_c('Check border: ', y2020_anal_eps_sf$eps[[i]], ' + ', y2020_anal_eps_sf$eps[[j]], '\n', e$message))
        # Geomarkets that border only a corner: AZ 3 + CO 3, FL 3 + FL 7, FL 4 + FL 5, IN10 + OH 9, IN12 + OH 7, NM 1 + UT 2, NY17 + NY20, NY18 + NY19
        
        return(NULL)
      }
    )
    
    if (is.null(border)) next
    if (length(border) != 1) message(str_c('ms_innerlines() length != 1: ', y2020_anal_eps_sf$eps[[i]], ' + ', y2020_anal_eps_sf$eps[[j]]))
    
    geomarket_borders_df <- add_row(
      geomarket_borders_df,
      geomarket_1 = y2020_anal_eps_sf$eps[[i]],
      geomarket_2 = y2020_anal_eps_sf$eps[[j]],
      geometry = st_sfc(border[[1]])
    )
  }
}

st_crs(geomarket_borders_df) <- 4326

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = y2020_anal_eps_sf, label = ~eps, weight = 1, opacity = 1, highlightOptions = highlightOptions(fillColor = 'black')) %>% 
  addPolylines(data = geomarket_borders_df, color = 'green', label = ~paste0(geomarket_1, ' + ', geomarket_2), highlightOptions = highlightOptions(color = 'black'))


# Calculate shortest distance between each HS and border
st_crs(y2020_anal_eps_sf)  # EPSG:4326
st_crs(geomarket_borders_df)  # EPSG:4326
st_crs(pubprivhs_df)  # EPSG:4326

# Takes 10-15 min to run
hs_geomarket_distance_df <- pubprivhs_df %>%
  select(hs_eps, hs_ncessch, hs_sch_name) %>%
  left_join(geomarket_borders_df %>% mutate(border = str_c(geomarket_1, '-', geomarket_2)) %>% pivot_longer(cols = c(geomarket_1, geomarket_2), values_to = 'geomarket') %>% select(-name) %>% as.data.frame(), by = c('hs_eps' = 'geomarket'), relationship = 'many-to-many') %>% 
  rowwise() %>% 
  mutate(
    distance_mi = st_distance(st_transform(hs_geometry, 3857), st_transform(geometry, 3857)) %>% as.numeric() * 0.000621371  # converts meters to miles
  )
saveRDS(hs_geomarket_distance_df, file.path('data', 'hs_geomarket_distance_df.RDS'))

# Plot all HS within 2 miles of a border
leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = y2020_anal_eps_sf, label = ~eps, weight = 1, opacity = 1, highlightOptions = highlightOptions(fillColor = 'black')) %>% 
  addPolygons(data = st_buffer(geomarket_borders_df, dist = 2 * 1609.34), color = 'pink', label = ~paste0(geomarket_1, ' + ', geomarket_2)) %>% 
  addPolylines(data = geomarket_borders_df, color = 'green', label = ~paste0(geomarket_1, ' + ', geomarket_2), highlightOptions = highlightOptions(color = 'black')) %>% 
  addMarkers(data = hs_geomarket_distance_df %>% filter(distance_mi < 2), label = ~hs_sch_name)

# Filter for all HS-geomarket border distance that's less than X miles
distance_threshold <- 2

hs_geomarket_distance_df %>% 
  filter(distance_mi < distance_threshold) %>% 
  View()

# For each HS, show how many borders it is less than X miles from
hs_geomarket_distance_df %>% 
  mutate(
    close_to_border = if_else(distance_mi < distance_threshold, 1, 0)
  ) %>% 
  group_by(hs_eps, hs_ncessch, hs_sch_name, hs_geometry) %>% 
  summarise(
    num_close_borders = sum(close_to_border),
    borders = str_c(border[close_to_border == 1], collapse = ';')
  ) %>% 
  View()
