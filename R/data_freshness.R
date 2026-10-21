#' Data freshness badge inputs (STAC datetime + release updated_at)
#'
#' Rank **7**: surface STAC item datetime, key emma_envdata release
#' `updated_at` timestamps, and (at render time) Quarto build time.

#' Fetch GitHub release metadata (updated_at / published_at) for one tag.
get_release_updated_at <- function(tag, repo = ENVDATA_REPO) {
  token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
  headers <- c(
    Accept = "application/vnd.github+json",
    `User-Agent` = "eco-emma/emma_report"
  )
  if (nzchar(token)) {
    headers <- c(headers, Authorization = paste("Bearer", token))
  }
  url <- sprintf("https://api.github.com/repos/%s/releases/tags/%s", repo, tag)
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf), add = TRUE)
  ok <- tryCatch(
    {
      utils::download.file(url, tf, quiet = TRUE, headers = headers)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!isTRUE(ok) || !file.exists(tf) || file.info(tf)$size < 2L) {
    return(list(tag = tag, updated_at = NA_character_, published_at = NA_character_))
  }
  rel <- tryCatch(
    jsonlite::fromJSON(tf, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(rel)) {
    return(list(tag = tag, updated_at = NA_character_, published_at = NA_character_))
  }
  list(
    tag = tag,
    updated_at = as.character(rel$updated_at %||% NA_character_),
    published_at = as.character(rel$published_at %||% NA_character_)
  )
}

#' Latest VI STAC item datetime + id (soft-fail → NA).
get_latest_vi_stac_datetime <- function() {
  out <- list(
    item_id = NA_character_,
    datetime = NA_character_,
    href = NA_character_
  )
  item <- tryCatch(get_latest_vi_stac_item(), error = function(e) NULL)
  if (is.null(item)) {
    return(out)
  }
  props <- item$properties %||% list()
  out$item_id <- as.character(item$id %||% NA_character_)
  out$datetime <- as.character(props$datetime %||% NA_character_)
  self_links <- Filter(
    function(l) identical(l$rel, "self"),
    item$links %||% list()
  )
  if (length(self_links) >= 1L) {
    out$href <- as.character(self_links[[1]]$href %||% NA_character_)
  }
  out
}

#' Assemble freshness metadata for Quarto footers.
#'
#' @param tags Character vector of emma_envdata release tags to probe.
compute_data_freshness <- function(tags = c(
                                     "stac",
                                     "stac-vi",
                                     "fireage",
                                     "static_data",
                                     "vi_viirs_raster",
                                     "vi_modis_terra_raster",
                                     "weather_data",
                                     "inat_data"
                                   )) {
  stac <- get_latest_vi_stac_datetime()
  releases <- lapply(tags, get_release_updated_at)
  releases_df <- dplyr::bind_rows(lapply(releases, function(x) {
    data.frame(
      tag = x$tag,
      updated_at = x$updated_at,
      published_at = x$published_at,
      stringsAsFactors = FALSE
    )
  }))
  # Prefer the newest release updated_at among probed tags as a single signal.
  parsed <- suppressWarnings(as.POSIXct(releases_df$updated_at, tz = "UTC"))
  newest <- if (any(!is.na(parsed))) {
    releases_df$tag[[which.max(parsed)]]
  } else {
    NA_character_
  }
  newest_at <- if (any(!is.na(parsed))) {
    format(max(parsed, na.rm = TRUE), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    NA_character_
  }

  list(
    stac_item_id = stac$item_id,
    stac_datetime = stac$datetime,
    stac_href = stac$href,
    releases = releases_df,
    newest_release_tag = newest,
    newest_release_updated_at = newest_at,
    catalog_url = ENVDATA_STAC_CATALOG_URL
  )
}

#' Minimal escape for attribute/text values we inject into the badge HTML.
.emma_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Format a short freshness badge (Markdown + HTML wrapper).
#'
#' @param data_freshness List from [compute_data_freshness()]
#' @param build_time POSIXct Quarto build time (defaults to now UTC)
format_freshness_badge_md <- function(data_freshness,
                                      build_time = as.POSIXlt(Sys.time(), tz = "UTC")) {
  stac_dt <- data_freshness$stac_datetime
  if (length(stac_dt) != 1L || is.na(stac_dt) || !nzchar(stac_dt)) {
    stac_txt <- "unavailable"
  } else {
    stac_day <- substr(stac_dt, 1L, 10L)
    stac_txt <- sprintf(
      '<span title="%s">%s</span>',
      .emma_escape(stac_dt),
      .emma_escape(stac_day)
    )
  }
  rel_at <- data_freshness$newest_release_updated_at
  rel_tag <- data_freshness$newest_release_tag
  if (length(rel_at) != 1L || is.na(rel_at) || !nzchar(rel_at)) {
    rel_txt <- "unavailable"
  } else {
    rel_day <- substr(rel_at, 1L, 10L)
    rel_txt <- sprintf(
      '<span title="%s @ %s">%s</span>',
      .emma_escape(rel_tag),
      .emma_escape(rel_at),
      .emma_escape(rel_day)
    )
  }
  build_txt <- format(build_time, "%Y-%m-%d %H:%M UTC")

  paste0(
    '<div class="emma-freshness-badge" markdown="1">\n\n',
    "**Data freshness** — STAC VI datetime: ", stac_txt,
    " · envdata release updated: ", rel_txt,
    " · page built: ", .emma_escape(build_txt),
    " · [STAC catalog](", data_freshness$catalog_url, ")",
    " · [Data page](data.html)\n\n",
    "</div>\n"
  )
}

#' Park-page variant (relative link to Data page).
format_freshness_badge_park_md <- function(data_freshness,
                                           build_time = as.POSIXlt(Sys.time(), tz = "UTC")) {
  md <- format_freshness_badge_md(data_freshness, build_time = build_time)
  gsub("\\]\\(data\\.html\\)", "](../data.html)", md, perl = TRUE)
}

#' Calendar day (YYYY-MM-DD) from a release `updated_at`, or "—" when missing.
release_updated_day <- function(data_freshness, tag) {
  rel <- data_freshness$releases
  if (is.null(rel) || !is.data.frame(rel) || nrow(rel) < 1L) {
    return("—")
  }
  row <- rel[rel$tag == tag, , drop = FALSE]
  if (nrow(row) != 1L) {
    return("—")
  }
  at <- row$updated_at[[1L]]
  if (length(at) != 1L || is.na(at) || !nzchar(at)) {
    return("—")
  }
  substr(as.character(at), 1L, 10L)
}

#' Format a date-like value as YYYY-MM-DD, or "—" when missing.
.emma_asof_day <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return("—")
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.POSIXlt(x, tz = "UTC"), "%Y-%m-%d"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  s <- as.character(x)
  if (!nzchar(s) || identical(s, "NA")) {
    return("—")
  }
  # Already a day or ISO datetime
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) {
    return(substr(s, 1L, 10L))
  }
  s
}

#' Compact HTML table cell (escaped text).
.emma_td <- function(x) {
  paste0("<td>", .emma_escape(x), "</td>")
}

#' One compact **Data status** footer (domain scope).
#'
#' Merges former coverage + freshness strips. As-of dates that used to be a
#' separate Home/park table belong here (and/or the Data Sources table).
#'
#' @param coverage_metrics List from [compute_coverage_metrics()]
#' @param data_freshness List from [compute_data_freshness()]
#' @param evi_as_of Date (or coercible) for fused EVI composite
#' @param weather_as_of Date / character latest weather observation day
#' @param build_time POSIXct Quarto build time (UTC)
#' @param data_href Link to Data page (coverage definitions)
format_data_status_md <- function(coverage_metrics,
                                  data_freshness,
                                  evi_as_of = coverage_metrics$evi_as_of,
                                  weather_as_of = NULL,
                                  build_time = as.POSIXlt(Sys.time(), tz = "UTC"),
                                  data_href = "data.html") {
  m <- coverage_metrics
  evi_day <- .emma_asof_day(evi_as_of)
  stac_day <- .emma_asof_day(data_freshness$stac_datetime)
  fire_upd <- release_updated_day(data_freshness, "fireage")
  weather_day <- .emma_asof_day(weather_as_of)
  build_day <- format(build_time, "%Y-%m-%d %H:%M UTC")

  evi_cov <- paste0(fmt_km2(m$valid_evi$km2), " km²")
  fire_cov <- paste0(
    "known ", fmt_km2(m$fire_age$known_km2),
    " / censored ", fmt_km2(m$fire_age$censored_km2), " km²"
  )
  area_cov <- paste0(
    "domain ", fmt_km2(m$domain$km2),
    " · parks-union ", fmt_km2(m$parks_union$km2), " km²"
  )

  rows <- paste0(
    "<tr>",
    .emma_td("Fused EVI"), .emma_td(evi_day), .emma_td(stac_day), .emma_td(evi_cov),
    "</tr>\n",
    "<tr>",
    .emma_td("Fire age"), .emma_td("—"), .emma_td(fire_upd), .emma_td(fire_cov),
    "</tr>\n",
    "<tr>",
    .emma_td("Weather"), .emma_td(weather_day), .emma_td("—"), .emma_td("—"),
    "</tr>\n",
    "<tr>",
    .emma_td("Page"), .emma_td("—"), .emma_td(build_day), .emma_td("—"),
    "</tr>\n",
    "<tr>",
    .emma_td("Domain area"), .emma_td("—"), .emma_td("—"), .emma_td(area_cov),
    "</tr>\n"
  )

  cov_anchor <- paste0(data_href, "#coverage-assessment-metrics")
  paste0(
    '<div class="emma-data-status" markdown="1">\n\n',
    "## Data status\n\n",
    '<table class="emma-data-status-table">\n',
    "<thead><tr>",
    "<th>Product</th><th>As-of</th><th>Updated</th><th>Coverage</th>",
    "</tr></thead>\n<tbody>\n",
    rows,
    "</tbody></table>\n\n",
    "Product dates and coverage for maps on this page. ",
    "Sources and definitions: [Data](", data_href, ") · ",
    "[Coverage metrics](", cov_anchor, ").\n\n",
    "</div>\n"
  )
}

#' Park-page **Data status** footer (park coverage + station weather).
format_data_status_park_md <- function(coverage_metrics,
                                       data_freshness,
                                       park_name,
                                       evi_as_of = coverage_metrics$evi_as_of,
                                       weather_as_of = NULL,
                                       build_time = as.POSIXlt(Sys.time(), tz = "UTC"),
                                       data_href = "../data.html") {
  m <- coverage_metrics
  parks <- m$parks
  row <- parks[parks$name == park_name, , drop = FALSE]

  evi_day <- .emma_asof_day(evi_as_of)
  stac_day <- .emma_asof_day(data_freshness$stac_datetime)
  fire_upd <- release_updated_day(data_freshness, "fireage")
  weather_day <- .emma_asof_day(weather_as_of)
  build_day <- format(build_time, "%Y-%m-%d %H:%M UTC")

  if (nrow(row) == 1L) {
    evi_cov <- paste0(
      fmt_km2(row$valid_evi_km2), " km² (",
      sprintf("%.0f", row$valid_evi_pct), "%)"
    )
    fire_cov <- paste0(
      "known ", fmt_km2(row$fire_known_km2),
      " / censored ", fmt_km2(row$fire_censored_km2), " km²"
    )
    area_cov <- paste0(fmt_km2(row$denom_km2), " km² (park cells)")
  } else {
    evi_cov <- "—"
    fire_cov <- "—"
    area_cov <- "unavailable"
  }

  rows <- paste0(
    "<tr>",
    .emma_td("Fused EVI"), .emma_td(evi_day), .emma_td(stac_day), .emma_td(evi_cov),
    "</tr>\n",
    "<tr>",
    .emma_td("Fire age"), .emma_td("—"), .emma_td(fire_upd), .emma_td(fire_cov),
    "</tr>\n",
    "<tr>",
    .emma_td("Weather"), .emma_td(weather_day), .emma_td("—"), .emma_td("—"),
    "</tr>\n",
    "<tr>",
    .emma_td("Page"), .emma_td("—"), .emma_td(build_day), .emma_td("—"),
    "</tr>\n",
    "<tr>",
    .emma_td("Park area"), .emma_td("—"), .emma_td("—"), .emma_td(area_cov),
    "</tr>\n"
  )

  cov_anchor <- paste0(data_href, "#coverage-assessment-metrics")
  paste0(
    '<div class="emma-data-status emma-data-status-park" markdown="1">\n\n',
    "## Data status\n\n",
    '<table class="emma-data-status-table">\n',
    "<thead><tr>",
    "<th>Product</th><th>As-of</th><th>Updated</th><th>Coverage</th>",
    "</tr></thead>\n<tbody>\n",
    rows,
    "</tbody></table>\n\n",
    "Product dates and coverage for maps on this page. ",
    "Sources and definitions: [Data](", data_href, ") · ",
    "[Coverage metrics](", cov_anchor, ").\n\n",
    "</div>\n"
  )
}
