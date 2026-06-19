source("02_code/functions/api_helpers.R")

root_resp <- httr::GET(AIS_BASE_URL, httr::accept("application/openapi+json"))
root_json <- jsonlite::fromJSON(httr::content(root_resp, as = "text", encoding = "UTF-8"))
available_resources <- names(root_json$paths)
print(available_resources)
# Static table: total rows and distinct mmsi (should be identical, since
# mmsi is the primary key of ais_static -> one row per vessel)
static_overview <- ais_aggregate(
  "ais_static",
  select = "rows:count(),distinct_mmsi:mmsi.count()"
)


dynamic_overview <- ais_aggregate(
  "ais_dynamic",
  select = "rows:count(),distinct_mmsi:mmsi.count()"
)

distinct_vessels_dynamic <- ais_get(
  "ais_dynamic",
  list(select = "mmsi", limit = 1)  # placeholder; see note below
)

distinct_vessels_dynamic_grouped <- ais_get(
  "ais_dynamic",
  list(select = "mmsi,n:mmsi.count()")
)
n_distinct_vessels_dynamic <- nrow(distinct_vessels_dynamic_grouped)



ship_type_counts <- ais_get(
  "ais_static",
  list(select = "ship_type,n:ship_type.count()")
)
ship_type_counts <- ship_type_counts[order(-ship_type_counts$n), ]
top3_ship_types <- head(ship_type_counts, 3)
print(top3_ship_types)


top3_ship_types <- ais_get(
  "ais_static",
  list(select = "ship_type,n:ship_type.count()", order = "n.desc", limit = 3)
)
print(top3_ship_types)



# --- draught ---
draught_summary <- ais_aggregate(
  "ais_static",
  select  = "n_valid:draught.count(),min:draught.min(),max:draught.max(),mean:draught.avg()",
  filters = list(draught = "neq.0")   # 0 gilt laut Aufgabe als "unknown", nicht als echter Wert
)
print(draught_summary)

# --- length ---
length_summary <- ais_aggregate(
  "ais_static",
  select  = "n_valid:length.count(),min:length.min(),max:length.max(),mean:length.avg()",
  filters = list(length = "neq.0")
)
print(length_summary)

# --- width ---
width_summary <- ais_aggregate(
  "ais_static",
  select  = "n_valid:width.count(),min:width.min(),max:width.max(),mean:width.avg()",
  filters = list(width = "neq.0")
)
print(width_summary)




#NA FUNCS

draught_na <- ais_aggregate(
  "ais_static",
  select  = "n_na:draught.count()",
  filters = list(draught = "is.null")
)



length_na <- ais_aggregate(
      "ais_static",
      select  = "n_na:length.count()",
      filters = list(length = "is.null") )

width_na <- ais_aggregate(
  "ais_static",
  select  = "n_na:width.count()",
  filters = list(width = "is.null")
)



print(width_na)



print(length_na)

print(draught_na)


##Vessel sorting by flag


n_benelux <- ais_aggregate(
  "ais_static",
  select  = "count()",
  filters = list(flag = "in.(BE,NL,LU)")
)
print(n_benelux)


# All german cargo vessels
n_german_cargo <- ais_aggregate(
  "ais_static",
  select  = "count()",
  filters = list(flag = "eq.DE", ship_type = "eq.Cargo")
)
print(n_german_cargo)

# German cargo vessels longer than 150m
n_german_cargo_long <- ais_aggregate(
  "ais_static",
  select  = "count()",
  filters = list(flag = "eq.DE", ship_type = "eq.Cargo", length = "gt.150")
)
print(n_german_cargo_long)


#Vessels with express in name
n_express <- ais_aggregate(
  "ais_static",
  select  = "count()",
  filters = list(name = "ilike.*EXPRESS*")
)
print(n_express)


first5min_filters <- list(
  msg_timestamp = c("gte.2022-01-01T00:00:00Z", "lt.2022-01-01T00:05:00Z")
)

#first 5 min Records
n_first5min_records <- ais_aggregate(
  "ais_dynamic",
  select  = "count()",
  filters = first5min_filters
)

print(n_first5min_records)

# Anzahl distinct Vessels (über group-by mmsi, dann Zeilen zählen)
first5min_vessels <- ais_get(
  "ais_dynamic",
  c(list(select = "mmsi,n:mmsi.count()"), first5min_filters)
)
n_first5min_distinct_vessels <- nrow(first5min_vessels)

print(n_first5min_distinct_vessels)



window_filters <- list(
  msg_timestamp = c("gte.2021-05-04T14:00:00Z", "lt.2021-05-04T14:30:00Z"),
  speed         = "gt.12"
)

# Distinct fast vessels
fast_vessels <- ais_get(
  "ais_dynamic",
  c(list(select = "mmsi"), window_filters)
)
fast_vessels_unique <- unique(fast_vessels$mmsi)
n_fast_vessels <- length(fast_vessels_unique)
print(n_fast_vessels)

# of those the cargo ones (Join through ais_static via 'in'-Filter)
if (n_fast_vessels > 0) {
  mmsi_list_str <- paste(fast_vessels_unique, collapse = ",")
  fast_cargo <- ais_aggregate(
    "ais_static",
    select  = "count()",
    filters = list(
      mmsi      = paste0("in.(", mmsi_list_str, ")"),
      ship_type = "eq.Cargos"
    )
  )
  print(fast_cargo)
}

