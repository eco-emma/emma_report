#!/usr/bin/env Rscript
# Lightweight checks for coverage / freshness formatters (no network, no geotargets).
# Run: Rscript scripts/test_coverage_format.R

source("R/envdata_contract.R")
source("R/coverage_metrics.R")
source("R/data_freshness.R")

stopifnot(identical(fmt_km2(12345.6), "12,346"))
stopifnot(identical(fmt_km2(NA_real_), "—"))
# Vectorized path used by dplyr transmute on park rows (regression for all-dashes bug)
stopifnot(identical(fmt_km2(c(225.2, NA_real_, 1000)), c("225", "—", "1,000")))
stopifnot(identical(fmt_km2(numeric(0)), character(0)))

fake <- list(
  cell_km2 = 0.25,
  evi_as_of = as.Date("2026-09-21"),
  fire_mask_note = "mask note.",
  domain = list(km2 = 1000),
  parks_union = list(km2 = 200),
  valid_evi = list(km2 = 150),
  fire_age = list(known_km2 = 40, censored_km2 = 160),
  parks = data.frame(
    name = "Table Mountain National Park",
    denom_km2 = 225.2,
    valid_evi_km2 = 200,
    valid_evi_pct = 88.8,
    fire_known_km2 = 100,
    fire_censored_km2 = 125.2,
    stringsAsFactors = FALSE
  )
)

md <- format_coverage_domain_md(fake)
stopifnot(grepl("\\*\\*Coverage\\*\\*", md))
stopifnot(grepl("data.html#coverage-assessment-metrics", md))
stopifnot(!grepl("hero", md, ignore.case = TRUE))

pmd <- format_coverage_park_md(fake, "Table Mountain National Park")
stopifnot(grepl("VIIRS cells intersecting the park", pmd))
stopifnot(grepl("park area", pmd))
stopifnot(grepl("\\.\\./data.html", pmd))

# Simulate Data-page park table formatting (must not all be em-dash)
parks2 <- rbind(
  fake$parks,
  data.frame(
    name = "Cederberg Nature Reserve Complex",
    denom_km2 = 1000,
    valid_evi_km2 = 900,
    valid_evi_pct = 90,
    fire_known_km2 = 400,
    fire_censored_km2 = 600,
    stringsAsFactors = FALSE
  )
)
formatted <- fmt_km2(parks2$denom_km2)
stopifnot(identical(formatted, c("225", "1,000")))
stopifnot(!any(formatted == "—"))

fresh <- list(
  stac_item_id = "vi_20260921",
  stac_datetime = "2026-09-21T00:00:00Z",
  newest_release_tag = "fireage",
  newest_release_updated_at = "2026-10-16T19:52:30Z",
  catalog_url = ENVDATA_STAC_CATALOG_URL
)
b <- format_freshness_badge_md(fresh, build_time = as.POSIXlt("2026-10-19 12:00:00", tz = "UTC"))
stopifnot(grepl("Data freshness", b))
stopifnot(grepl("2026-09-21", b))
stopifnot(grepl("page built", b))

pb <- format_freshness_badge_park_md(fresh)
stopifnot(grepl("\\.\\./data.html", pb))

# --- STAC links hardening (no network): empty / data.frame / no-self never OOB ---
stopifnot(length(.stac_normalize_links(NULL)) == 0L)
stopifnot(length(.stac_normalize_links(list())) == 0L)
stopifnot(length(.stac_links_with_rel(list(), "self")) == 0L)

# List-of-lists shape (simplifyVector = FALSE), no self link — the #49/#50 crash.
links_list <- list(
  list(rel = "collection", href = "https://example.invalid/collection.json"),
  list(rel = "root", href = "https://example.invalid/catalog.json"),
  list(rel = "parent", href = "https://example.invalid/collection.json")
)
stopifnot(length(.stac_links_with_rel(links_list, "self")) == 0L)
stopifnot(length(.stac_links_with_rel(links_list, "collection")) == 1L)

# data.frame shape (simplifyVector = TRUE) — Filter-on-columns footgun.
links_df <- data.frame(
  rel = c("collection", "item", "item"),
  href = c(
    "https://example.invalid/collection.json",
    "https://example.invalid/vi_20200101.json",
    "https://example.invalid/vi_20260921.json"
  ),
  stringsAsFactors = FALSE
)
items <- .stac_links_with_rel(links_df, "item")
stopifnot(length(items) == 2L)
stopifnot(identical(items[[2]]$href, "https://example.invalid/vi_20260921.json"))

# Newest-release picker: all-NA / empty never subscript-OOB.
sig_empty <- .newest_release_signal(data.frame(
  tag = character(),
  updated_at = character(),
  published_at = character(),
  stringsAsFactors = FALSE
))
stopifnot(is.na(sig_empty$tag), is.na(sig_empty$updated_at))

sig_na <- .newest_release_signal(data.frame(
  tag = c("stac", "fireage"),
  updated_at = c(NA_character_, NA_character_),
  published_at = c(NA_character_, NA_character_),
  stringsAsFactors = FALSE
))
stopifnot(is.na(sig_na$tag), is.na(sig_na$updated_at))

sig_ok <- .newest_release_signal(data.frame(
  tag = c("stac", "static_data"),
  updated_at = c("2026-10-07T20:05:06Z", "2026-10-17T02:41:29Z"),
  published_at = c(NA_character_, NA_character_),
  stringsAsFactors = FALSE
))
stopifnot(identical(sig_ok$tag, "static_data"))
stopifnot(identical(sig_ok$updated_at, "2026-10-17T02:41:29Z"))

message("All coverage/freshness formatter tests passed.")
