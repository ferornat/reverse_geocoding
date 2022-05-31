rm(list=ls())
gc()
.rs.restartR()
setwd("")

# Librerías
library("sf")
library("dplyr")
library("mapview")
library("tmap")
library("xlsx")
library("ggplot2")
library("tidyverse")
library("bigrquery")
library("sp")
library("revgeo") # This geocoder didn't run to mne
library("tidygeocoder") # https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html#:~:text=To%20perform%20reverse%20geocoding%20(obtaining,from%20the%20geocoder%20query%20above.

# We get our data frame directly from Google Bigquery
billing <- '' # Reemplazar con el Proyect ID que querramos
query = ''

df <- bq_table_download(bq_project_query(billing, query))

# Turn into an object
test_points <- test_data %>%
  # lng/lat value are missing in some records
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Render
mapview_test_points = mapview(test_points, cex = 3, alpha = .5, popup = NULL)
mapview_test_points

# We get the hexagrids (they could be a "fishgrid")
area_honeycomb_grid = st_make_grid(test_points, c(0.007, 0.007), what = "polygons", square = FALSE)

# To sf and add grid ID
honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

# We count the number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
honeycomb_grid_sf$n_colli = lengths(st_intersects(honeycomb_grid_sf, test_points))

# remove grid without value of 0 (i.e. no points in side that grid)
honeycomb_count = filter(honeycomb_grid_sf, n_colli > 0)

tmap_mode("view")

map_honeycomb = tm_shape(honeycomb_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_honeycomb


# Now we filter by counting condition

honeycomb_count2 = filter(honeycomb_count, n_colli > 10)
mapview(honeycomb_count2, zcol = "n_colli")

# We get the Centroides and Extremes

centroids <- st_centroid(honeycomb_count2) %>% st_coordinates()
directions <- reverse_geocode(as.data.frame(centroids), method = 'osm', lat = Y , long = X)
colnames(directions) <- c("lon","lat","address")
directions$class <- "centroide"

# Here we filter just the ones that have 3 extremes in common
extremes <- st_intersection(honeycomb_count2)[st_intersection(honeycomb_count2)$n.overlaps==3,] %>% st_coordinates()
extremes_location <- reverse_geocode(as.data.frame(extreme_location), method = 'osm', lat = Y , long = X)
colnames(extremes_location) <- c("lon","lat","address")
extremes_location$class <- "extremo"

# We render the points obtained
mapview(rbind(st_centroid(honeycomb_count2)[1],st_intersection(honeycomb_count2)[st_intersection(honeycomb_count2)$n.overlaps==3,][0]))

all_points = rbind(directions, extreme_location)
write.csv(direcciones_scrap,'direcciones.csv')