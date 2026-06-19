# ==============================================================================
# ais_dynamic_sample_points.R
#
# Task 3: Generating Sample Data
#
# Part A: Interval-based (cluster) sample
#   - sampling frame = all 5-minute intervals on 2024-01-24
#   - randomly select 100 intervals, pull up to 100 records each,
#     stop once 10,000 records are accumulated
#   -> saved as 01_data/sample_intervals.csv
#
# Part B: Stratified sample
#   - strata = combinations of ship_type x hour-of-day on 2024-01-24
#   - sample size 10,000, allocated proportionally to stratum size
#   -> saved as 01_data/sample_stratified.csv
#
# Part C: Comparison plots (ship_type, speed, collection_type) between
#   both samples.
# ==============================================================================

source("02_code/functions/api_helpers.R")
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------
# Reproducibility: fix the random seed.
# This matters because both the choice of intervals (Part A) and the
# within-stratum subsampling (Part B) rely on R's random number generator.
# Without a fixed seed, re-running this script would draw a different
# sample of intervals/records each time, so neither the resulting .csv
# files nor any results computed downstream (Task 4 dashboard, Task 5
# comparisons) could be reproduced by a grader or by ourselves later.
# ------------------------------------------------------------------------
set.seed(2026)
#Declare target date and total sample and set to values accordingly
TARGET_DATE   <- as.Date("2024-01-24")
TOTAL_SAMPLE  <- 10000

# ==========================================================================
# PART A: Interval-based cluster sample
# ==========================================================================

# --- Step 1: build the sampling frame of all 5-minute intervals ----------

#declare the start and end of day for the targeted date declared above
day_start <- as.POSIXct(paste(TARGET_DATE, "00:00:00"), tz = "UTC")
day_end   <- as.POSIXct(paste(TARGET_DATE, "23:55:00"), tz = "UTC")


#initialize the intervals starting from day_start to day_end with 5 min steps
interval_starts <- seq(day_start, day_end, by = "5 min")  # 288 intervals
n_intervals_total <- length(interval_starts)
stopifnot(n_intervals_total == 288)

# --- Step 2: randomly draw 100 intervals without replacement -------------
N_INTERVALS_TO_DRAW <- 100
PER_INTERVAL_LIMIT  <- 100

# Sample INDICES rather than the POSIXct objects directly: applying sample()
# to a POSIXct vector can strip its time class on some R versions, which then
# breaks downstream time formatting. Indexing back into interval_starts keeps
# the proper POSIXct type
sampled_idx    <- sample(seq_len(n_intervals_total), size = N_INTERVALS_TO_DRAW,
                          replace = FALSE)
sampled_starts <- sort(interval_starts[sampled_idx])  # keeps POSIXct class

# --- Step 3: query each interval until 10,000 observations are reached ---
interval_sample_list <- list()
n_collected <- 0

# Iterate over indices (not over the POSIXct vector directly): looping with
# `for (x in posixct_vector)` can strip the time class from each element on
# some R versions, which then breaks time formatting inside the loop.
for (i in seq_along(sampled_starts)) {

  start_time <- sampled_starts[i]  # stays POSIXct via single-bracket indexing

  if (n_collected >= TOTAL_SAMPLE) break

  end_time <- start_time + 5 * 60  # 5 minutes later

  remaining_budget <- TOTAL_SAMPLE - n_collected
  this_limit <- min(PER_INTERVAL_LIMIT, remaining_budget)

  query <- list(
    select        = "msg_timestamp,mmsi,latitude,longitude,speed,course,heading,collection_type",
    msg_timestamp = c(
      paste0("gte.", format_iso_utc(start_time)),
      paste0("lt.",  format_iso_utc(end_time))
    ),
    limit = this_limit
  )

  chunk <- ais_get("ais_dynamic", query)

  if (nrow(chunk) > 0) {
    chunk$sampled_interval_start <- format_iso_utc(start_time)
    interval_sample_list[[length(interval_sample_list) + 1]] <- chunk
    n_collected <- n_collected + nrow(chunk)
  }
}

sample_intervals <- do.call(rbind, interval_sample_list)

# Defensive trim in case the last chunk overshot the 10,000 target
if (nrow(sample_intervals) > TOTAL_SAMPLE) {
  sample_intervals <- sample_intervals[seq_len(TOTAL_SAMPLE), ]
}

cat("Interval sample size:", nrow(sample_intervals), "\n")
cat("Number of intervals actually used:", length(unique(sample_intervals$sampled_interval_start)), "\n")

write.csv(sample_intervals, "01_data/sample_intervals.csv", row.names = FALSE)

# ==========================================================================
# PART B: Stratified sample (ship_type x hour-of-day)
# ==========================================================================

# --- Step 1: get stratum sizes (counts per ship_type x hour) -------------
# We split the day into 4 six-hour blocks when querying, to keep each
# request's response small, then combine the per-block results into one
# stratum table.

day_blocks <- list(
  c("00:00:00", "06:00:00"),
  c("06:00:00", "12:00:00"),
  c("12:00:00", "18:00:00"),
  c("18:00:00", "23:59:59")
)

# Helper: extend ais_dynamic with an hour-of-day bucket and ship_type via
# a join against ais_static is expensive at the database level for a full
# day, so instead we count records per ship_type x hour using a small number
# of targeted aggregate queries (one per hour), which keeps each request fast
# and avoids grouping over the full high-resolution timestamp.

# --- Step 1: count records per (ship_type, hour) ------------------------
# We first need the ship_type for each mmsi (small lookup table), then count
# dynamic records per hour and join. To keep server load low we issue one
# aggregate request per hour that returns the count of records grouped by
# mmsi for that hour, then map mmsi -> ship_type locally.

ship_type_lookup <- ais_get("ais_static", list(select = "mmsi,ship_type"))

block_stratum_list <- list()

for (hr in 0:23) {
  hour_start <- sprintf("%sT%02d:00:00Z", TARGET_DATE, hr)
  hour_end   <- if (hr == 23) {
    paste0(as.character(TARGET_DATE + 1), "T00:00:00Z")
  } else {
    sprintf("%sT%02d:00:00Z", TARGET_DATE, hr + 1)
  }

  # Count records per mmsi within this hour (server-side aggregate).
  # Grouping by mmsi alone yields at most a few thousand rows per hour.
  hour_data <- ais_get("ais_dynamic", list(
    select        = "mmsi,n:mmsi.count()",
    msg_timestamp = c(paste0("gte.", hour_start), paste0("lt.", hour_end))
  ))

  if (nrow(hour_data) > 0) {
    hour_data$hour <- hr
    block_stratum_list[[length(block_stratum_list) + 1]] <- hour_data
  }
}

mmsi_hour_counts <- do.call(rbind, block_stratum_list)

# --- Step 2: attach ship_type and aggregate to (ship_type, hour) strata --
strata_counts <- mmsi_hour_counts %>%
  left_join(ship_type_lookup, by = "mmsi") %>%
  mutate(ship_type = ifelse(is.na(ship_type), "Unknown", ship_type)) %>%
  group_by(ship_type, hour) %>%
  summarise(stratum_count = sum(n), .groups = "drop")

total_records <- sum(strata_counts$stratum_count)

# --- Step 3: proportional allocation of the 10,000-record sample ---------
# Allocation rule: each stratum receives round(proportion * 10,000)
# observations. Strata whose proportional allocation would round to 0 but
# that contain at least 1 record are bumped up to a minimum of 1 observation,
# so that very small strata are not silently dropped from the sample. After
# this floor adjustment we rescale slightly so the total stays at 10,000.

strata_counts <- strata_counts %>%
  mutate(
    proportion   = stratum_count / total_records,
    raw_alloc    = proportion * TOTAL_SAMPLE,
    allocation   = pmax(1, round(raw_alloc)),
    allocation   = pmin(allocation, stratum_count)  # cannot sample more than exists
  )

# Rescale so allocations sum to (approximately) TOTAL_SAMPLE: adjust the
# largest strata up/down by the rounding remainder.
alloc_diff <- TOTAL_SAMPLE - sum(strata_counts$allocation)
if (alloc_diff != 0) {
  ord <- order(-strata_counts$stratum_count)
  adjust_idx <- ord[seq_len(min(abs(alloc_diff), length(ord)))]
  strata_counts$allocation[adjust_idx] <- strata_counts$allocation[adjust_idx] +
    sign(alloc_diff)
}

cat("Total allocated sample size:", sum(strata_counts$allocation), "\n")

# --- Step 4: query each stratum, retrieving the allocated number of rows -
# True random row selection is not directly supported by PostgREST (there
# is no "ORDER BY random()" exposed through the REST filter syntax). As a
# practical and documented approximation, we fetch a pool of records for
# each HOUR using only a time filter (which keeps the request URL short and
# safe), attach ship_type locally via the lookup table, and then draw the
# allocated number of rows per (ship_type, hour) stratum as a reproducible
# random subsample in R under the fixed seed.
#
# NOTE: We deliberately do NOT filter by a long list of MMSIs in the URL.
# Doing so for common ship types (tens of thousands of MMSIs) produces an
# extremely long URL that the server rejects ("Failed sending data to the
# peer"). Filtering ship_type locally avoids this entirely.

# Pool size to fetch per hour: large enough to contain the per-hour
# allocations across all ship types, capped to protect the shared server.
HOUR_POOL_LIMIT <- 5000

stratified_sample_list <- list()

# Build a quick lookup from mmsi -> ship_type for local assignment
mmsi_to_type <- setNames(ship_type_lookup$ship_type, ship_type_lookup$mmsi)

for (hr in 0:23) {

  hour_start <- sprintf("%sT%02d:00:00Z", TARGET_DATE, hr)
  hour_end   <- if (hr == 23) {
    paste0(as.character(TARGET_DATE + 1), "T00:00:00Z")
  } else {
    sprintf("%sT%02d:00:00Z", TARGET_DATE, hr + 1)
  }

  # Fetch a pool of records for this hour (time filter only -> short URL).
  pool <- ais_get("ais_dynamic", list(
    select = "msg_timestamp,mmsi,latitude,longitude,speed,course,heading,collection_type",
    msg_timestamp = c(paste0("gte.", hour_start), paste0("lt.", hour_end)),
    limit = HOUR_POOL_LIMIT
  ))

  if (nrow(pool) == 0) next

  # Attach ship_type locally
  pool$ship_type <- mmsi_to_type[as.character(pool$mmsi)]
  pool$ship_type[is.na(pool$ship_type)] <- "Unknown"

  # For each stratum (ship_type) allocated to THIS hour, draw the allocated
  # number of rows from the pool.
  strata_this_hour <- strata_counts[strata_counts$hour == hr, ]

  for (j in seq_len(nrow(strata_this_hour))) {
    st   <- strata_this_hour$ship_type[j]
    need <- strata_this_hour$allocation[j]

    pool_st <- pool[pool$ship_type == st, ]
    if (nrow(pool_st) == 0) next

    take <- min(need, nrow(pool_st))
    chunk_sampled <- pool_st[sample(seq_len(nrow(pool_st)), take, replace = FALSE), ]
    chunk_sampled$stratum_hour <- hr
    stratified_sample_list[[length(stratified_sample_list) + 1]] <- chunk_sampled
  }
}

sample_stratified <- do.call(rbind, stratified_sample_list)

cat("Stratified sample size:", nrow(sample_stratified), "\n")

write.csv(sample_stratified, "01_data/sample_stratified.csv", row.names = FALSE)

# ==========================================================================
# PART C: Comparison plots between the two samples
# ==========================================================================

sample_intervals$sample_source  <- "Interval sample"
sample_stratified$sample_source <- "Stratified sample"

# Drop columns that exist only in the stratified sample (ship_type and
# stratum_hour were added during stratified sampling) so both data frames
# share the same base columns; ship_type is re-attached below for BOTH
# samples in one consistent step.
drop_cols <- c("ship_type", "stratum_hour")
sample_stratified_base <- sample_stratified[, !(names(sample_stratified) %in% drop_cols)]

common_cols <- intersect(names(sample_intervals), names(sample_stratified_base))
combined <- rbind(
  sample_intervals[, common_cols],
  sample_stratified_base[, common_cols]
)

# --- ship_type: attach it to BOTH samples via the lookup -----------------
combined <- combined %>%
  left_join(ship_type_lookup, by = "mmsi") %>%
  mutate(ship_type = ifelse(is.na(ship_type), "Unknown", ship_type))

# Plot 1: ship_type distribution
p_ship_type <- ggplot(combined, aes(x = ship_type, fill = sample_source)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(
    title = "Ship type distribution: interval vs. stratified sample",
    x = "Ship type", y = "Count", fill = "Sample"
  ) +
  theme_minimal(base_size = 11)

ggsave("03_report/graphs/compare_ship_type.png", p_ship_type, width = 7, height = 5, dpi = 300)

# Plot 2: speed distribution
p_speed <- ggplot(combined, aes(x = speed, fill = sample_source)) +
  geom_density(alpha = 0.4) +
  xlim(0, 30) +
  labs(
    title = "Speed distribution: interval vs. stratified sample",
    x = "Speed (knots)", y = "Density", fill = "Sample"
  ) +
  theme_minimal(base_size = 11)

ggsave("03_report/graphs/compare_speed.png", p_speed, width = 7, height = 5, dpi = 300)

# Plot 3: collection_type distribution
p_collection <- ggplot(combined, aes(x = collection_type, fill = sample_source)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Collection type distribution: interval vs. stratified sample",
    x = "Collection type", y = "Count", fill = "Sample"
  ) +
  theme_minimal(base_size = 11)

ggsave("03_report/graphs/compare_collection_type.png", p_collection, width = 7, height = 5, dpi = 300)

cat("Saved comparison plots to 03_report/graphs/\n")
