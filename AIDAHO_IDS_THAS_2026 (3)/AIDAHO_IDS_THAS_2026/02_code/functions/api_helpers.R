# ==============================================================================
# api_helpers.R
#
# Helper functions for querying the AIDAHO AIS PostgREST API.
# Sourced by the analysis scripts in 02_code/R/.
#
# Base endpoint: https://aidaho-edu.uni-hohenheim.de/aisdb/
# ==============================================================================

library(httr)
library(jsonlite)

AIS_BASE_URL <- "https://aidaho-edu.uni-hohenheim.de/aisdb"

#' Send a GET request to the AIS PostgREST API and parse the JSON response.
#'
#' @param resource Name of the table/view to query (e.g. "ais_static").
#' @param query A named list of PostgREST query parameters, e.g.
#'   list(select = "mmsi,name", limit = 100, flag = "eq.DE").
#' @param as_df If TRUE (default), the result is returned as a data.frame.
#'   If FALSE, the parsed list is returned (useful for single aggregate rows).
#'
#' @return A data.frame (or list) with the query result.
ais_get <- function(resource, query = list(), as_df = TRUE) {

  # Build the query string manually so that a single parameter can carry
  # MULTIPLE values (e.g. msg_timestamp = c("gte.X", "lt.Y") must become
  # msg_timestamp=gte.X&msg_timestamp=lt.Y). httr::modify_url cannot express
  # repeated keys, so we URL-encode and assemble the query ourselves.
  base_url <- file.path(AIS_BASE_URL, resource)

  if (length(query) > 0) {
    # PostgREST uses certain characters syntactically in filter values
    # (e.g. '*' wildcards in ilike, parentheses and commas in in.(...),
    # '.' between operator and value, ':' in aggregate aliases). These must
    # NOT be percent-encoded, or the filter breaks. We encode spaces and a
    # few genuinely unsafe characters but leave PostgREST syntax intact.
    encode_value <- function(v) {
      v <- as.character(v)
      # Remove any whitespace (spaces, newlines, tabs) that may appear when a
      # long select/filter string is split across multiple lines in the code.
      # Whitespace is never semantically meaningful inside PostgREST
      # select/filter values, so this is safe and avoids broken URLs.
      v <- gsub("[[:space:]]+", "", v)
      v <- gsub("#", "%23", v, fixed = TRUE)
      v <- gsub("&", "%26", v, fixed = TRUE)
      v <- gsub("\\+", "%2B", v)
      v
    }
    pairs <- unlist(lapply(names(query), function(key) {
      values <- query[[key]]
      vapply(
        values,
        function(v) paste0(key, "=", encode_value(v)),
        character(1)
      )
    }), use.names = FALSE)
    url <- paste0(base_url, "?", paste(pairs, collapse = "&"))
  } else {
    url <- base_url
  }

  resp <- httr::GET(url)

  if (httr::status_code(resp) >= 300) {
    stop(
      "AIS API request failed with status ", httr::status_code(resp),
      "\nURL: ", url,
      "\nResponse: ", httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }

  raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")

  if (nchar(raw_text) == 0) {
    if (as_df) return(data.frame())
    return(list())
  }

  parsed <- jsonlite::fromJSON(raw_text, flatten = TRUE)

  if (as_df) {
    return(as.data.frame(parsed, stringsAsFactors = FALSE))
  }
  parsed
}

#' Convenience wrapper to get a single aggregate value (e.g. a count) from
#' a PostgREST aggregate query.
#'
#' @param resource Table/view name.
#' @param select A PostgREST aggregate select string, e.g. "count()" or
#'   "n_distinct:mmsi.count(distinct)".
#' @param filters Named list of additional filter parameters.
#'
#' @return The parsed aggregate result as a data.frame (usually one row).
ais_aggregate <- function(resource, select, filters = list()) {
  query <- c(list(select = select), filters)
  ais_get(resource, query)
}

#' Build a PostgREST "limit"/"offset" paginated request and combine all pages.
#' Useful when a single stratum or filter still returns more rows than is
#' comfortable in one request.
#'
#' @param resource Table/view name.
#' @param query Named list of query parameters (without limit/offset).
#' @param page_size Number of rows requested per page.
#' @param max_rows Safety cap on total rows retrieved.
ais_get_paginated <- function(resource, query = list(), page_size = 1000,
                               max_rows = 100000) {
  all_pages <- list()
  offset <- 0

  repeat {
    page_query <- c(query, list(limit = page_size, offset = offset))
    page <- ais_get(resource, page_query)

    if (nrow(page) == 0) break

    all_pages[[length(all_pages) + 1]] <- page
    offset <- offset + page_size

    if (offset >= max_rows) {
      warning("Reached max_rows safety cap (", max_rows, "). Stopping pagination.")
      break
    }
    if (nrow(page) < page_size) break  # last page was not full -> done
  }

  if (length(all_pages) == 0) return(data.frame())
  do.call(rbind, all_pages)
}

#' Format a POSIXct timestamp as an ISO-8601 UTC string suitable for
#' PostgREST timestamp filters (e.g. "2024-01-24T00:00:00Z").
format_iso_utc <- function(timestamp) {
  # Defensive: if the time class was lost upstream (e.g. a numeric value),
  # coerce it back to POSIXct (numeric POSIXct values are seconds since the
  # Unix epoch in UTC) before formatting.
  if (!inherits(timestamp, "POSIXct")) {
    timestamp <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
  }
  format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

