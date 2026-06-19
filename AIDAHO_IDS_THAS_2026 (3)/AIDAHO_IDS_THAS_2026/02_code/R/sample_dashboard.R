# ==============================================================================
# ais_dynamic_individual_paths.R
#
# Task 4.2: Query, process, and visualize individual vessel tracks.
#
#   (a) MMSI 2579999       - data quality inspection
#   (b) MMSI 563040400     - movement description over a chosen window
#   (c) MMSI 211430830     - Vienna-Linz route, ship locks (descriptive)
#   (d) detect_lock_events()  - rule-based ship-lock detection function
#   (e) leaflet plot of the vessel's trajectory with detected lock events
# ==============================================================================

source("02_code/functions/api_helpers.R")
library(leaflet)
library(dplyr)
library(geosphere)   # for distHaversine() used in the lock-detection function

# ------------------------------------------------------------------------
# (a) MMSI = 2579999 - data quality check
# ------------------------------------------------------------------------
# MMSI numbers should be 9 digits and follow ITU coding rules (the first
# three digits are a Maritime Identification Digit, MID, that identifies
# the vessel's flag state). 2579999 is only 7 digits long, and ranges
# below 100,000,000 do not correspond to a valid ship MID. Such short or
# malformed MMSIs typically indicate a misconfigured AIS transponder
# (e.g. operator entered the wrong identifier), a test/dummy device, or a
# non-vessel transmitter (such as certain AIS-enabled buoys or improperly
# configured devices).
#
# Online lookup (MarineTraffic/MyShipTracking, June 2026) shows MMSI
# 2579999 registered as an unnamed "Base Station" rather than a vessel,
# and the number additionally appears on a public list of MMSIs
# associated with AIS spoofing/anomalies (Global Fishing Watch vessel-list
# repository). Likely explanations: a misconfigured or default AIS unit,
# a coastal base station rather than a ship, or deliberately spoofed/
# test traffic. Document this as a concrete data-quality caveat in the
# report's "Coverage and Limitations" section.

mmsi_check <- ais_get(
  "ais_static",
  list(select = "*", mmsi = "eq.2579999")
)
print(mmsi_check)

mmsi_check_dynamic_sample <- ais_get(
  "ais_dynamic",
  list(select = "msg_timestamp,latitude,longitude,speed,collection_type",
       mmsi = "eq.2579999", limit = 50)
)
print(mmsi_check_dynamic_sample)

# ------------------------------------------------------------------------
# (b) MMSI = 563040400 - movement over a chosen time window
# ------------------------------------------------------------------------

# NOTE: the originally assumed window (2021-05-01 to 2021-05-08) was wrong --
# this MMSI has no data in May 2021. A diagnostic query against the live API
# (ais_aggregate with min()/max()/count() on msg_timestamp, filtered to this
# MMSI) showed the vessel's full ais_dynamic history runs from
# 2021-01-01T06:35:09Z to 2021-01-05T05:53:42Z (4,313 records in total).
# We therefore use that full window here, which comfortably fits under the
# limit = 5000 cap below, so the entire available track is retrieved.
window_start <- "2021-01-01T00:00:00Z"
window_end   <- "2021-01-06T00:00:00Z"

track_563040400 <- ais_get(
  "ais_dynamic",
  list(
    select = "msg_timestamp,latitude,longitude,speed,course,heading,collection_type",
    mmsi   = "eq.563040400",
    msg_timestamp = c(paste0("gte.", window_start), paste0("lt.", window_end)),
    order  = "msg_timestamp.asc",
    limit  = 5000
  )
)
cat("Records retrieved for MMSI 563040400:", nrow(track_563040400), "\n")

# Defensive check: if the query returned zero rows, ais_get() returns a
# completely empty data.frame() (no columns at all -- see api_helpers.R).
# Passing that into leaflet(...) %>% addPolylines(lng = ~longitude, ...)
# fails with "object 'longitude' not found", because the ~longitude formula
# has no column to resolve against. We check for this explicitly and stop
# with an informative message instead, so the real cause (no data for this
# MMSI/time window, rather than a bug in the mapping code) is obvious.
if (nrow(track_563040400) == 0) {
  stop(
    "No ais_dynamic records found for MMSI 563040400 in the window ",
    window_start, " to ", window_end, ".\n",
    "Possible causes: (1) this MMSI has no data in this date range in your ",
    "database instance, (2) a typo in the MMSI or timestamps, or (3) the API ",
    "request failed silently. Try a wider window first, e.g. query without a ",
    "time filter (with a small limit) to check whether this MMSI exists at all:\n",
    '  ais_get("ais_dynamic", list(select = "msg_timestamp", mmsi = "eq.563040400", limit = 5))'
  )
}

map_563040400 <- leaflet(track_563040400) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(lng = ~longitude, lat = ~latitude, color = "steelblue", weight = 2) %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE,
    fillOpacity = 0.6, fillColor = "steelblue",
    clusterOptions = markerClusterOptions()
  )

print(map_563040400)

# ------------------------------------------------------------------------
# (c) MMSI = 211430830 - Vienna-Linz route, 2021-01-17 to 2021-01-20
# ------------------------------------------------------------------------

vienna_linz_window_start <- "2021-01-17T00:00:00Z"
vienna_linz_window_end   <- "2021-01-20T00:00:00Z"

track_211430830 <- ais_get(
  "ais_dynamic",
  list(
    select = "msg_timestamp,latitude,longitude,speed,course,heading",
    mmsi   = "eq.211430830",
    msg_timestamp = c(
      paste0("gte.", vienna_linz_window_start),
      paste0("lt.",  vienna_linz_window_end)
    ),
    order = "msg_timestamp.asc",
    limit = 5000
  )
)
cat("Records retrieved for MMSI 211430830:", nrow(track_211430830), "\n")

# Same defensive check as for MMSI 563040400 above.
if (nrow(track_211430830) == 0) {
  stop(
    "No ais_dynamic records found for MMSI 211430830 in the window ",
    vienna_linz_window_start, " to ", vienna_linz_window_end, ".\n",
    "Check the MMSI/time window, or test with a small unfiltered query first:\n",
    '  ais_get("ais_dynamic", list(select = "msg_timestamp", mmsi = "eq.211430830", limit = 5))'
  )
}

map_211430830_raw <- leaflet(track_211430830) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(lng = ~longitude, lat = ~latitude, color = "darkgreen", weight = 2) %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE,
    fillOpacity = 0.6, fillColor = "darkgreen",
    clusterOptions = markerClusterOptions()
  )
print(map_211430830_raw)

# Expected qualitative signature of a ship lock in AIS data (used to guide
# both the manual description in the report and the detection rule below):
#   - speed drops to near zero for several minutes (vessel waits / is
#     raised or lowered),
#   - location stays essentially fixed (small spatial footprint),
#   - duration is typically in the range of ~10-40 minutes for a single
#     chamber passage,
#   - AIS observation density at the lock is often higher than on the open
#     river, because slow-moving/stationary vessels send AIS position
#     reports at a higher frequency than fast-moving ones (the AIS
#     reporting interval depends on speed and course-change rate).

# ------------------------------------------------------------------------
# (d) Rule-based ship-lock detection function
# ------------------------------------------------------------------------
#
# Decision logic (see report for the accompanying flowchart):
#   1. Sort observations by msg_timestamp.
#   2. Flag a point as a "candidate lock point" if speed < SPEED_THRESHOLD.
#   3. Group consecutive candidate points into one event if the time gap
#      to the previous candidate point is <= MAX_TIME_GAP_MIN minutes AND
#      the spatial distance to the previous candidate point is
#      <= MAX_DIST_M metres. (Both conditions must hold; this avoids
#      merging two genuinely different stops, e.g. a lock stop followed
#      much later by an anchoring stop elsewhere, into a single event.)
#   4. Discard groups that are too short to plausibly represent a lock
#      passage: a group is kept if EITHER its duration is at least
#      MIN_DURATION_MIN minutes OR it contains at least MIN_OBSERVATIONS
#      AIS records (see note on duration_minutes below).
#   5. For each remaining group, compute start_time, end_time,
#      duration_minutes, an approximate location (mean lat/lon of the
#      group), and n_observations.
#
# Thresholds (justified in the report):
#   SPEED_THRESHOLD   = 1 knot   (vessels under power on open water rarely
#                                  sustain speeds this low; locks require
#                                  vessels to slow to a near stop)
#   MAX_TIME_GAP_MIN  = 15 min   (AIS reporting intervals lengthen at low
#                                  speed, but rarely exceed this within one
#                                  continuous stop)
#   MAX_DIST_M        = 300 m    (a single lock chamber plus approach is
#                                  typically well under this radius)
#   MIN_DURATION_MIN  = 5 min    (shorter stops are unlikely to be a full
#                                  lock passage, which generally takes at
#                                  least several minutes to complete)
#   MIN_OBSERVATIONS  = 4        (complementary criterion to MIN_DURATION_MIN.
#                                  In this dataset, several genuine stops
#                                  consist of multiple AIS records that all
#                                  share an identical msg_timestamp at
#                                  to-the-second resolution, so
#                                  duration_minutes evaluates to exactly 0
#                                  for those events even though many distinct
#                                  observations were recorded at the same
#                                  location. Accepting a group when it has
#                                  enough independent observations -- even if
#                                  its computed duration is 0 -- avoids
#                                  discarding real stops because of this
#                                  timestamp-resolution artefact.)

detect_lock_events <- function(track,
                               speed_threshold   = 1,
                               max_time_gap_min   = 15,
                               max_dist_m         = 300,
                               min_duration_min   = 5,
                               min_observations   = 4) {
  
  track <- track[order(track$msg_timestamp), ]
  track$msg_timestamp <- as.POSIXct(track$msg_timestamp, tz = "UTC")
  
  is_candidate <- track$speed < speed_threshold & !is.na(track$speed)
  
  # Assign a group id to consecutive candidate points that satisfy both
  # the time-gap and distance conditions relative to the previous
  # candidate point.
  group_id <- rep(NA_integer_, nrow(track))
  current_group <- 0L
  prev_idx <- NA_integer_
  
  for (i in seq_len(nrow(track))) {
    if (!is_candidate[i]) next
    
    if (is.na(prev_idx)) {
      current_group <- current_group + 1L
      group_id[i] <- current_group
    } else {
      time_gap_min <- as.numeric(
        difftime(track$msg_timestamp[i], track$msg_timestamp[prev_idx], units = "mins")
      )
      dist_m <- geosphere::distHaversine(
        c(track$longitude[prev_idx], track$latitude[prev_idx]),
        c(track$longitude[i],        track$latitude[i])
      )
      
      if (time_gap_min <= max_time_gap_min && dist_m <= max_dist_m) {
        group_id[i] <- current_group       # continue current event
      } else {
        current_group <- current_group + 1L
        group_id[i] <- current_group       # start a new event
      }
    }
    prev_idx <- i
  }
  
  candidates <- track[!is.na(group_id), ]
  candidates$group_id <- group_id[!is.na(group_id)]
  
  if (nrow(candidates) == 0) {
    return(data.frame(
      start_time = as.POSIXct(character()), end_time = as.POSIXct(character()),
      duration_minutes = numeric(), latitude = numeric(), longitude = numeric(),
      n_observations = integer()
    ))
  }
  
  # Filtering rule: an event is kept if it satisfies the duration threshold
  # OR the observation-count threshold (rather than duration alone).
  #
  # Rationale: in this dataset, several genuine low-speed stops consist of
  # multiple AIS records that all share the SAME msg_timestamp (to-the-second
  # resolution), so duration_minutes evaluates to exactly 0 for those events
  # even though 6-45 separate observations were recorded at essentially the
  # same location. A duration-only filter would discard these as noise,
  # even though a real vessel does not transmit dozens of position reports
  # without elapsed time -- the repeated timestamp is a data-resolution
  # artefact, not evidence against a real stop. Requiring at least
  # min_observations independent AIS messages at (approximately) the same
  # place is a robust complementary signal of a genuine stop, so we accept
  # an event if EITHER condition holds.
  events <- candidates %>%
    group_by(group_id) %>%
    summarise(
      start_time       = min(msg_timestamp),
      end_time          = max(msg_timestamp),
      duration_minutes = as.numeric(difftime(end_time, start_time, units = "mins")),
      latitude         = mean(latitude),
      longitude        = mean(longitude),
      n_observations   = n(),
      .groups = "drop"
    ) %>%
    filter(duration_minutes >= min_duration_min | n_observations >= min_observations) %>%
    select(start_time, end_time, duration_minutes, latitude, longitude, n_observations)
  
  as.data.frame(events)
}

lock_events_211430830 <- detect_lock_events(track_211430830)
print(lock_events_211430830)

write.csv(lock_events_211430830, "01_data/lock_events_211430830.csv", row.names = FALSE)

# Limitations to discuss in the report:
#  - fixed thresholds are not adaptive to vessel type (a barge and a
#    passenger ferry may behave differently at low speed);
#  - GPS/AIS position noise near locks (concrete structures, reduced
#    satellite visibility) can occasionally inflate the apparent distance
#    between consecutive points, splitting one real event into two;
#  - the rule cannot distinguish a lock stop from other reasons for
#    stopping (e.g. waiting at a bridge, anchoring, technical fault)
#    without external knowledge of lock locations;
#  - an unsupervised approach (e.g. DBSCAN clustering on
#    longitude/latitude/time jointly, or a Hidden Markov Model over
#    speed states) could detect "stop" regimes without manually chosen
#    thresholds, and could be cross-validated against a public list of
#    known lock coordinates along the Danube.

# ------------------------------------------------------------------------
# (e) Trajectory plot with detected lock events highlighted
# ------------------------------------------------------------------------

map_with_locks <- leaflet(track_211430830) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(lng = ~longitude, lat = ~latitude, color = "darkgreen",
               weight = 2, opacity = 0.7, group = "Trajectory") %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE,
    fillOpacity = 0.4, fillColor = "darkgreen", group = "Trajectory"
  )

if (nrow(lock_events_211430830) > 0) {
  map_with_locks <- map_with_locks %>%
    addCircleMarkers(
      data = lock_events_211430830,
      lng = ~longitude, lat = ~latitude,
      radius = 8, color = "red", stroke = TRUE, fillOpacity = 0.8,
      group = "Detected lock events",
      popup = ~paste0(
        "Lock event<br>Start: ", start_time,
        "<br>Duration: ", round(duration_minutes, 1), " min",
        "<br>Observations: ", n_observations
      )
    )
}

map_with_locks <- map_with_locks %>%
  addLayersControl(
    overlayGroups = c("Trajectory", "Detected lock events"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(map_with_locks)

htmlwidgets::saveWidget(map_with_locks, "03_report/graphs/vessel_211430830_locks.html",
                        selfcontained = TRUE)