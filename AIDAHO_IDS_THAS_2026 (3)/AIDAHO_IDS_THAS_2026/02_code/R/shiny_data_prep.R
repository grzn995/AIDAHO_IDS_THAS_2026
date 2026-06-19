# ==============================================================================
# shiny_data_prep.R
#
# Task 7.3: Preprocess the data needed by the Shiny app (Option B) so the
# deployed app reads a static file instead of repeatedly querying the
# AIS API on every user interaction.
#
# Produces: 01_data/shiny_app_data.csv
#   columns: hour, minute, collection_type, ship_type, n_records
#
# This pre-aggregated table is small enough to ship with the app and lets
# the Shiny server simply filter/sum in memory.
# ==============================================================================

source("02_code/functions/api_helpers.R")
library(dplyr)

TARGET_DATE <- "2024-04-23"  # adjust if a different reference date is required

ship_type_lookup <- ais_get("ais_static", list(select = "mmsi,ship_type"))

day_blocks <- list(
  c("00:00:00", "06:00:00"),
  c("06:00:00", "12:00:00"),
  c("12:00:00", "18:00:00"),
  c("18:00:00", "23:59:59")
)

block_list <- list()

for (block in day_blocks) {
  block_start <- paste0(TARGET_DATE, "T", block[1], "Z")
  block_end   <- paste0(TARGET_DATE, "T", block[2], "Z")

  chunk <- ais_get(
    "ais_dynamic",
    list(
      select = "mmsi,msg_timestamp,collection_type",
      msg_timestamp = c(paste0("gte.", block_start), paste0("lt.", block_end)),
      limit = 200000
    )
  )
  if (nrow(chunk) > 0) block_list[[length(block_list) + 1]] <- chunk
}

raw <- do.call(rbind, block_list)

raw$timestamp <- as.POSIXct(raw$msg_timestamp, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
raw$hour   <- as.integer(format(raw$timestamp, "%H"))
raw$minute <- as.integer(format(raw$timestamp, "%M"))

shiny_data <- raw %>%
  left_join(ship_type_lookup, by = "mmsi") %>%
  mutate(ship_type = ifelse(is.na(ship_type), "Unknown", ship_type)) %>%
  group_by(hour, minute, collection_type, ship_type) %>%
  summarise(n_records = n(), .groups = "drop")

write.csv(shiny_data, "01_data/shiny_app_data.csv", row.names = FALSE)

cat("Saved pre-aggregated Shiny data:", nrow(shiny_data), "rows\n")
