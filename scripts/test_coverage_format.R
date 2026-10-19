#!/usr/bin/env Rscript
# Lightweight checks for coverage / freshness formatters (no network, no geotargets).
# Run: Rscript scripts/test_coverage_format.R

source("R/envdata_contract.R")
source("R/coverage_metrics.R")
source("R/data_freshness.R")

stopifnot(identical(fmt_km2(12345.6), "12,346"))
stopifnot(identical(fmt_km2(NA_real_), "—"))

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
stopifnot(grepl("domain cells intersecting park", pmd))
stopifnot(grepl("\\.\\./data.html", pmd))

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

message("All coverage/freshness formatter tests passed.")
