#' Current envdata accessors for the report pipeline
#'
#' These functions consume the eco-emma/emma_envdata public release contract
#' (see R/envdata_contract.R). Vegetation product is EVI throughout targets and report templates.

get_temp_directory <- function(temp_directory = "data/temp") {
  if (!dir.exists(file.path(temp_directory))) {
    dir.create(file.path(temp_directory), recursive = TRUE)
  }
  temp_directory
}

#' List envdata release assets used by the report pipeline
#'
#' Returns a piggyback-style data frame (file_name, tag, ...) across the
#' active public tags. Prefer STAC helpers for "latest VI" discovery.
get_env_files <- function(repo = ENVDATA_REPO,
                          tags = NULL) {
  if (is.null(tags)) {
    # Only the public data tags the report pipeline reads
    tags <- c(
      ENVDATA_RELEASE_TAGS$static,
      ENVDATA_RELEASE_TAGS$vi_modis_terra,
      ENVDATA_RELEASE_TAGS$vi_modis_aqua,
      ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      ENVDATA_RELEASE_TAGS$vi_viirs,
      ENVDATA_RELEASE_TAGS$fireage,
      ENVDATA_RELEASE_TAGS$stac_vi,
      ENVDATA_RELEASE_TAGS$stac_burn
    )
  }
  rows <- lapply(tags, function(tag) {
    tryCatch(
      list_release_assets(repo = repo, tag = tag),
      error = function(e) {
        message("Skipping tag ", tag, ": ", conditionMessage(e))
        NULL
      }
    )
  })
  dplyr::bind_rows(rows)
}

get_report_files <- function(repo = REPORT_REPO, create_park_tag = TRUE) {
  # List across known report tags; fall back to empty frame
  tags <- tryCatch({
    # releases listing via API
    token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
    headers <- c(Accept = "application/vnd.github+json", `User-Agent` = "eco-emma/emma_report")
    if (nzchar(token)) headers <- c(headers, Authorization = paste("Bearer", token))
    tf <- tempfile(fileext = ".json")
    utils::download.file(
      sprintf("https://api.github.com/repos/%s/releases?per_page=100", repo),
      tf, quiet = TRUE, headers = headers
    )
    rels <- jsonlite::fromJSON(tf, simplifyVector = TRUE)
    unlink(tf)
    unique(as.character(rels$tag_name))
  }, error = function(e) character())

  rows <- lapply(tags, function(tag) {
    tryCatch(list_release_assets(repo = repo, tag = tag), error = function(e) NULL)
  })
  report_files <- dplyr::bind_rows(rows)
  park_data_tag <- "park_data"
  if (create_park_tag && (nrow(report_files) == 0L || !park_data_tag %in% report_files$tag)) {
    tryCatch(
      piggyback::pb_release_create(repo, tag = park_data_tag),
      error = function(e) message("Could not create park_data release: ", conditionMessage(e))
    )
  }
  report_files
}

#' Years since fire from fireage_current.tif (fireage release)
#'
#' Band 1 of fireage_current.tif is fire_age_days. Converted to years for the
#' existing report symbology (column/layer name "Years").
get_years_since_fire.tif <- function(env_files,
                                     temp_directory,
                                     most_recent_fire_date = NULL,
                                     piggyback_push = FALSE) {
  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$fireage,
    file = "fireage_current.tif",
    dest_dir = temp_directory,
    overwrite = TRUE
  )
  fireage <- terra::rast(path)
  age_days <- fireage[[1]]
  years_since_fire.tif <- age_days / 365.25
  names(years_since_fire.tif) <- "Years"
  years_since_fire.tif[years_since_fire.tif < 0] <- NA

  if (isTRUE(piggyback_push)) {
    out <- file.path(temp_directory, "years_since_fire.tif")
    terra::writeRaster(years_since_fire.tif, out, overwrite = TRUE)
    robust_pb_upload(
      file = out,
      repo = REPORT_REPO,
      tag = "current",
      max_attempts = 10,
      sleep_time = 10,
      temp_directory = temp_directory,
      overwrite = TRUE
    )
  }

  years_since_fire.tif
}

#' Crop years-since-fire to remnants and return WGS84 polygons for leaflet
generate_fires_vector <- function(years_since_fire.tif) {
  remnants_path <- "data/misc/remnants.tif"
  if (!file.exists(remnants_path)) {
    write_remnants_from_domain(dest = remnants_path)
  }
  remnants <- terra::rast(remnants_path)
  years_since_fire.tif <- terra::mask(years_since_fire.tif, remnants)

  fires_wgs <- terra::as.polygons(x = years_since_fire.tif) |>
    sf::st_as_sf() |>
    dplyr::rename(Years = 1) |>
    sf::st_transform(crs = sf::st_crs(4326))

  fires_wgs
}

#' Most recent MODIS Terra VI COG metadata (via STAC)
#'
#' Returns a one-row tibble with file_name, tag, href, file_date for
#' compatibility with existing targets.
get_most_recent_evi_file <- function(env_files = NULL) {
  item <- get_latest_vi_stac_item()
  assets <- item$assets
  if (is.null(assets$modis_terra_vi)) {
    stop("Latest VI STAC item lacks modis_terra_vi asset")
  }
  href <- assets$modis_terra_vi$href
  file_name <- basename(href)
  tibble::tibble(
    file_name = file_name,
    tag = ENVDATA_RELEASE_TAGS$vi_modis_terra,
    href = href,
    file_date = parse_envdata_yyyymmdd(file_name)
  )
}

#' Download and scale the most recent Terra EVI COG to approximate [-1, 1] units
#' (integer storage on release assets is ×100).
get_most_recent_evi.tif <- function(most_recent_evi_file, temp_directory) {
  path <- download_envdata_asset(
    tag = most_recent_evi_file$tag[[1]],
    file = most_recent_evi_file$file_name[[1]],
    dest_dir = temp_directory,
    overwrite = TRUE
  )
  r <- terra::rast(path)
  # Band 1 = evi (×100 integer); band 2 = doy
  evi <- if (terra::nlyr(r) >= 1L) r[[1]] else r
  evi <- scale_evi_integer(evi)
  evi[evi > 1] <- 1
  evi[evi < -1] <- -1
  # Mask zeros / nodata placeholders
  evi <- terra::mask(evi, mask = evi, maskvalue = 0)
  names(evi) <- "evi"
  evi
}

get_most_recent_evi_date <- function(most_recent_evi_file) {
  if ("file_date" %in% names(most_recent_evi_file) &&
      !is.na(most_recent_evi_file$file_date[[1]])) {
    return(most_recent_evi_file$file_date[[1]])
  }
  parse_envdata_yyyymmdd(most_recent_evi_file$file_name[[1]])
}

#' Monthly mean EVI minus current observation (same units)
get_monthly_delta_evi.tif <- function(most_recent_evi.tif, monthly_mean_evi.tif) {
  monthly_delta_evi.tif <- most_recent_evi.tif - monthly_mean_evi.tif

  if (terra::crs(most_recent_evi.tif, proj = TRUE) !=
      terra::crs(monthly_mean_evi.tif, proj = TRUE)) {
    stop("VI CRS mismatch")
  }
  if (terra::ext(most_recent_evi.tif) != terra::ext(monthly_mean_evi.tif)) {
    stop("VI extent mismatch")
  }
  if (terra::crs(monthly_delta_evi.tif, proj = TRUE) !=
      terra::crs(most_recent_evi.tif, proj = TRUE)) {
    terra::crs(monthly_delta_evi.tif) <- terra::crs(most_recent_evi.tif)
  }
  monthly_delta_evi.tif
}
