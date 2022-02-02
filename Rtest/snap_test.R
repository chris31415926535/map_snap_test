

swiss_lines <- leaflet::gadmCHE %>%
  sf::st_as_sf(crs = "WGS84") %>%
  as_tibble() %>%
  sf::st_as_sf(crs = "WGS84") 


point <- tibble(lat = 46.943060, lon = 7.441868) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84",  remove = FALSE)

pointonline <- lines %>%
  mutate(line = sf::st_nearest_points(geometry, point)) %>%
  mutate(length = sf::st_length(line)) %>%
  select(length, line) %>%
  arrange(length) %>%
  filter(length > units::as_units(0, "m")) %>%
  slice_head(n=1) %>% 
  #  sf::st_set_geometry(NULL) %>%
  pull(line) %>%
  sf::st_cast("POINT") %>%
  purrr::map(as.matrix) %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  as_tibble() %>%
  rename(lat = V2, lon = V1) %>%
  filter (lat != point$lat, lon != point$lon)


leaflet() %>%
  addTiles() %>%
  addMarkers(data = point) %>%
  addMarkers(data = pointonline) %>%
  addPolylines(data = lines)
