#!/usr/bin/env Rscript
# Lightweight checks for coverage / freshness / Data status formatters
# (no network, no geotargets).
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

# Legacy strips still format (soft-compat) but pages use Data status
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

stopifnot(identical(
  parse_burn_yyyymm(c("burn_modis_202609.tif", "burn_viirs_202601.tif", "nope.tif")),
  as.Date(c("2026-09-01", "2026-01-01", NA))
))

fresh <- list(
  stac_item_id = "vi_20260921",
  stac_datetime = "2026-09-21T00:00:00Z",
  newest_release_tag = "fireage",
  newest_release_updated_at = "2026-10-16T19:52:30Z",
  burn_as_of = as.Date("2026-09-01"),
  catalog_url = ENVDATA_STAC_CATALOG_URL,
  releases = data.frame(
    tag = c("fireage", "weather_data", "inat_data"),
    updated_at = c(
      "2026-10-16T19:52:30Z",
      "2026-10-10T12:00:00Z",
      NA_character_
    ),
    published_at = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
)
b <- format_freshness_badge_md(fresh, build_time = as.POSIXlt("2026-10-19 12:00:00", tz = "UTC"))
stopifnot(grepl("Data freshness", b))
stopifnot(grepl("2026-09-21", b))
stopifnot(grepl("page built", b))

pb <- format_freshness_badge_park_md(fresh)
stopifnot(grepl("\\.\\./data.html", pb))

stopifnot(identical(release_updated_day(fresh, "fireage"), "2026-10-16"))
stopifnot(identical(release_updated_day(fresh, "inat_data"), "—"))
stopifnot(identical(release_updated_day(fresh, "missing"), "—"))

# Merged Data status footer (domain)
ds <- format_data_status_md(
  fake,
  fresh,
  evi_as_of = as.Date("2026-09-21"),
  weather_as_of = as.Date("2026-10-01"),
  build_time = as.POSIXlt("2026-10-19 12:00:00", tz = "UTC"),
  data_href = "data.html"
)
stopifnot(grepl("emma-data-status", ds))
stopifnot(grepl("Data status", ds))
stopifnot(grepl("Fused EVI", ds))
stopifnot(grepl("2026-09-21", ds))
stopifnot(grepl("2026-10-01", ds))
# Weather Updated from weather_data release day
stopifnot(grepl("2026-10-10", ds))
# Fire As-of from AppEEARS burn month (burn_as_of)
stopifnot(grepl("2026-09-01", ds))
stopifnot(grepl("known 40", ds))
# Page row holds domain / parks-union coverage; no separate Domain area row
stopifnot(grepl("domain 1,000", ds))
stopifnot(grepl("parks-union 200", ds))
stopifnot(!grepl("Domain area", ds))
stopifnot(grepl("data.html#coverage-assessment-metrics", ds))
stopifnot(!grepl("emma-coverage-strip", ds))
stopifnot(!grepl("emma-freshness-badge", ds))
# Soft-fail: missing STAC still renders a row
fresh_na <- fresh
fresh_na$stac_datetime <- NA_character_
fresh_na$burn_as_of <- as.Date(NA)
ds_na <- format_data_status_md(fake, fresh_na, weather_as_of = NULL)
stopifnot(grepl("Fused EVI", ds_na))
stopifnot(grepl("Weather", ds_na))
stopifnot(grepl("Fire age", ds_na))

# Park variant — Page row carries park-cell coverage (no separate Park area row)
dsp <- format_data_status_park_md(
  fake,
  fresh,
  park_name = "Table Mountain National Park",
  weather_as_of = "2026-09-30",
  data_href = "../data.html"
)
stopifnot(grepl("emma-data-status-park", dsp))
stopifnot(!grepl("Park area", dsp))
stopifnot(grepl("225 km", dsp))
stopifnot(grepl("park cells", dsp))
stopifnot(grepl("\\.\\./data.html", dsp))
stopifnot(grepl("89%", dsp))  # sprintf("%.0f", 88.8)
stopifnot(grepl("2026-09-01", dsp))

message("All coverage/freshness/Data status formatter tests passed.")
