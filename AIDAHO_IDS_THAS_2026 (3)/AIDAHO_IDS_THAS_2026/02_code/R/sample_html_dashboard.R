# ==============================================================================
# sample_html_dashboard.R
#
# Task 6.1: Generate a self-contained static HTML dashboard from the
# Task 4.1 leaflet map, ready to be served by NGINX as a static file.
# ==============================================================================

library(leaflet)
library(dplyr)
library(readr)
library(htmlwidgets)

sample_points <- read_csv("01_data/sample_intervals.csv", show_col_types = FALSE) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(speed))

speed_palette <- colorNumeric(palette = "viridis", domain = sample_points$speed)

map_static <- leaflet(sample_points) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    radius = 3, stroke = FALSE, fillOpacity = 0.7,
    color = ~speed_palette(speed),
    popup = ~paste0("MMSI: ", mmsi, "<br>Speed: ", round(speed, 1), " kn")
  ) %>%
  addLegend("bottomright", pal = speed_palette, values = ~speed,
            title = "Speed (knots)", opacity = 1)

# selfcontained = TRUE bundles all JS/CSS dependencies into a single .html
# file, so NGINX only needs to serve this one file with no other assets.
saveWidget(map_static, file = "sample_points.html", selfcontained = TRUE)

cat("Saved sample_points.html to the project root.\n",
    "Copy/move this file into the directory that Task 6.2 mounts into the\n",
    "NGINX document root (see README and docker-compose.yaml).\n")
