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
  if (is.null(rel) || !is.list(rel)) {
    return(list(tag = tag, updated_at = NA_character_, published_at = NA_character_))
  }
  list(
    tag = tag,
    updated_at = as.character(rel$updated_at %||% NA_character_),
    published_at = as.character(rel$published_at %||% NA_character_)
  )
}

#' Empty freshness payload (soft-fail default).
.empty_data_freshness <- function() {
  list(
    stac_item_id = NA_character_,
    stac_datetime = NA_character_,
    stac_href = NA_character_,
    releases = data.frame(
      tag = character(),
      updated_at = character(),
      published_at = character(),
      stringsAsFactors = FALSE
    ),
    newest_release_tag = NA_character_,
    newest_release_updated_at = NA_character_,
    catalog_url = ENVDATA_STAC_CATALOG_URL
  )
}

#' Latest VI STAC item datetime + id (soft-fail → NA).
#'
#' Never indexes empty `Filter(...)` / malformed `links` (list or data.frame).
get_latest_vi_stac_datetime <- function() {
  out <- list(
    item_id = NA_character_,
    datetime = NA_character_,
    href = NA_character_
  )
  tryCatch(
    {
      item <- tryCatch(get_latest_vi_stac_item(), error = function(e) NULL)
      if (is.null(item) || !is.list(item)) {
        return(out)
      }
      props <- item$properties %||% list()
      if (!is.list(props)) {
        props <- list()
      }
      out$item_id <- as.character(item$id %||% NA_character_)
      out$datetime <- as.character(props$datetime %||% NA_character_)
      self_links <- .stac_links_with_rel(item$links, "self")
      if (length(self_links) >= 1L) {
        out$href <- as.character(self_links[[1]]$href %||% NA_character_)
      }
      out
    },
    error = function(e) out
  )
}

#' Pick tag / timestamp of the newest non-NA POSIXct among release rows.
.newest_release_signal <- function(releases_df) {
  empty <- list(tag = NA_character_, updated_at = NA_character_)
  if (!is.data.frame(releases_df) || nrow(releases_df) < 1L) {
    return(empty)
  }
  parsed <- suppressWarnings(
    as.POSIXct(
      releases_df$updated_at,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  )
  # Fallback when format= misses (e.g. fractional seconds); still avoid hard fail.
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(releases_df$updated_at, tz = "UTC"))
  }
  ok <- which(!is.na(parsed))
  if (length(ok) < 1L) {
    return(empty)
  }
  i <- ok[[which.max(parsed[ok])]]
  if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(releases_df)) {
    return(empty)
  }
  tag <- releases_df$tag[[i]]
  list(
    tag = if (length(tag) == 1L) as.character(tag) else NA_character_,
    updated_at = format(parsed[[i]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Assemble freshness metadata for Quarto footers.
#'
#' Soft-fails to NA fields on network / STAC / release malformation — never
#' throws subscript-out-of-bounds into the targets pipeline.
#'
#' @param tags Character vector of emma_envdata release tags to probe.
compute_data_freshness <- function(tags = c(
                                     "stac",
                                     "stac-vi",
                                     "fireage",
                                     "static_data",
                                     "vi_viirs_raster",
                                     "vi_modis_terra_raster"
                                   )) {
  tryCatch(
    {
      stac <- get_latest_vi_stac_datetime()
      releases <- lapply(as.character(tags %||% character()), get_release_updated_at)
      releases_df <- dplyr::bind_rows(lapply(releases, function(x) {
        data.frame(
          tag = as.character(x$tag %||% NA_character_),
          updated_at = as.character(x$updated_at %||% NA_character_),
          published_at = as.character(x$published_at %||% NA_character_),
          stringsAsFactors = FALSE
        )
      }))
      if (!is.data.frame(releases_df) || nrow(releases_df) < 1L) {
        releases_df <- .empty_data_freshness()$releases
      }
      newest <- .newest_release_signal(releases_df)

      list(
        stac_item_id = stac$item_id %||% NA_character_,
        stac_datetime = stac$datetime %||% NA_character_,
        stac_href = stac$href %||% NA_character_,
        releases = releases_df,
        newest_release_tag = newest$tag,
        newest_release_updated_at = newest$updated_at,
        catalog_url = ENVDATA_STAC_CATALOG_URL
      )
    },
    error = function(e) .empty_data_freshness()
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
