#' emma_envdata public contract helpers for emma_report
#'
#' Downstream consumers should read from eco-emma/emma_envdata GitHub Releases
#' (and the STAC catalog), not the legacy AdamWilsonLab `current` /
#' `processed_static` / old MODIS VI release tags.
#'
#' Join key for tabular products: `pid` (int32). Vegetation product is EVI
#' stored as integer × 100 (scale 0.01) in both COG band 1 and parquet `value`.

ENVDATA_REPO <- "eco-emma/emma_envdata"
REPORT_REPO <- "eco-emma/emma_report"
MODEL_REPO <- "eco-emma/emma_model"

ENVDATA_RELEASE_TAGS <- list(
  static              = "static_data",
  # Report SoT downloads (Phase C / R1–R3). Envdata writers publish these;
  # report never live-harvests parks / GSOD / iNat on GHA.
  weather             = "weather_data",
  inat                = "inat_data",
  # Envdata-only input tag (flat shapefile assets). Not consumed by report.
  manual_data         = "manual-data",
  vi_modis_terra      = "vi_modis_terra_raster",
  vi_modis_aqua       = "vi_modis_aqua_raster",
  vi_viirs            = "vi_viirs_raster",
  vi_modis_parquet    = "vi_modis_parquet",
  vi_viirs_parquet    = "vi_viirs_parquet",
  burn_modis_raster   = "burn_modis_raster",
  burn_viirs_raster   = "burn_viirs_raster",
  burn_modis_parquet  = "burn_modis_parquet",
  burn_viirs_parquet  = "burn_viirs_parquet",
  fireage             = "fireage",
  stac                = "stac",
  stac_vi             = "stac-vi",
  stac_burn           = "stac-burn",
  stac_static         = "stac-static"
)

#' Pinned consumer assets (§0.2 of the CI full plan)
#'
#' Missing tag/asset → fail loud (download_envdata_asset stops).
#' Present + zero rows → succeed; Quarto empty guards handle display.
ENVDATA_REPORT_ASSETS <- list(
  parks = list(
    tag = "static_data",
    files = c(
      "protected_areas.gpkg",
      "domain_outline.gpkg",
      "parks_buffer_10km.gpkg"
    ),
    required_cols = c("name", "type", "area_ha")
  ),
  weather = list(
    tag = "weather_data",
    files = c("weather.parquet", "stations.gpkg"),
    weather_cols = c("STNID", "YEARMODA", "TEMP", "PRCP", "NAME"),
    station_cols = c("STNID", "NAME")
  ),
  inat = list(
    tag = "inat_data",
    files = c("inat_invasives.gpkg"),
    required_cols = c(
      "scientific_name", "observed_on", "longitude", "latitude", "invasive"
    )
  )
)

ENVDATA_STAC_CATALOG_URL <-
  "https://github.com/eco-emma/emma_envdata/releases/download/stac/catalog.json"

#' Build a GitHub release asset URL
envdata_release_url <- function(tag, file, repo = ENVDATA_REPO) {
  sprintf("https://github.com/%s/releases/download/%s/%s", repo, tag, URLencode(file, reserved = TRUE))
}

#' List GitHub release assets via the REST API (paginated)
#'
#' Replaces piggyback::pb_list for large releases (e.g. fireage has 900+ assets)
#' where piggyback fails with "values must be length 1".
#'
#' @return data.frame with columns file_name, tag, size, timestamp (ISO), repo
list_release_assets <- function(repo = ENVDATA_REPO,
                                tag,
                                max_pages = 50L,
                                verbose = FALSE) {
  token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
  # GitHub API requires a User-Agent; anonymous requests without one often 403.
  headers <- c(
    Accept = "application/vnd.github+json",
    `User-Agent` = "eco-emma/emma_report"
  )
  if (nzchar(token)) {
    headers <- c(headers, Authorization = paste("Bearer", token))
  }

  empty <- data.frame(
    file_name = character(),
    tag = character(),
    size = integer(),
    timestamp = character(),
    repo = character(),
    stringsAsFactors = FALSE
  )

  # Fast path: gh CLI with server-side pagination + jq (avoids piggyback limits)
  if (nzchar(Sys.which("gh"))) {
    # Quote carefully — some R/system2 paths invoke sh and break on `|` / `?`.
    gh_sh <- function(args) {
      cmd <- paste(c("gh", vapply(args, shQuote, character(1))), collapse = " ")
      tryCatch(system(cmd, intern = TRUE), error = function(e) character())
    }
    rel_id <- suppressWarnings(as.integer(
      gh_sh(c("api", sprintf("repos/%s/releases/tags/%s", repo, tag), "--jq", ".id"))[1]
    ))
    if (!is.na(rel_id) && rel_id > 0L) {
      jq_filter <- ".[] | [.name, (.size|tostring), (.updated_at // .created_at)] | @tsv"
      lines <- gh_sh(c(
        "api", "--paginate",
        sprintf("repos/%s/releases/%d/assets?per_page=100", repo, rel_id),
        "--jq", jq_filter
      ))
      lines <- lines[nzchar(lines)]
      if (length(lines) > 0L) {
        parts <- strsplit(lines, "\t", fixed = TRUE)
        ok <- lengths(parts) >= 3L
        parts <- parts[ok]
        return(data.frame(
          file_name = vapply(parts, `[[`, character(1), 1L),
          tag = tag,
          size = as.integer(vapply(parts, function(x) x[2], character(1))),
          timestamp = vapply(parts, function(x) x[3], character(1)),
          repo = repo,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Fallback: download.file pagination (needs User-Agent / token headers)
  release_tf <- tempfile(fileext = ".json")
  on.exit(unlink(release_tf), add = TRUE)
  release_url <- sprintf("https://api.github.com/repos/%s/releases/tags/%s", repo, tag)
  tryCatch(
    utils::download.file(release_url, release_tf, quiet = !verbose, headers = headers),
    error = function(e) stop("Failed to fetch release ", repo, "@", tag, ": ", conditionMessage(e))
  )
  release <- jsonlite::fromJSON(release_tf, simplifyVector = TRUE)
  release_id <- release$id
  if (is.null(release_id)) stop("Release ", repo, "@", tag, " has no id")

  file_name <- character()
  size <- integer()
  timestamp <- character()
  page <- 1L
  repeat {
    page_tf <- tempfile(fileext = ".json")
    page_url <- sprintf(
      "https://api.github.com/repos/%s/releases/%s/assets?per_page=100&page=%d",
      repo, release_id, page
    )
    utils::download.file(page_url, page_tf, quiet = !verbose, headers = headers)
    page_assets <- jsonlite::fromJSON(page_tf, simplifyVector = TRUE)
    unlink(page_tf)
    if (is.null(page_assets) || NROW(page_assets) == 0L) break
    nms <- as.character(page_assets$name)
    file_name <- c(file_name, nms)
    size <- c(size, as.integer(page_assets$size))
    timestamp <- c(timestamp, as.character(page_assets$updated_at))
    if (length(nms) < 100L || page >= max_pages) break
    page <- page + 1L
  }
  if (length(file_name) == 0L) return(empty)
  data.frame(
    file_name = file_name,
    tag = tag,
    size = size,
    timestamp = timestamp,
    repo = repo,
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Download one release asset with retries (replaces legacy robust_pb_download for envdata)
download_envdata_asset <- function(tag,
                                   file,
                                   dest_dir,
                                   repo = ENVDATA_REPO,
                                   overwrite = TRUE,
                                   max_attempts = 10,
                                   sleep_time = 10,
                                   verbose = TRUE) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  destfile <- file.path(dest_dir, basename(file))
  if (file.exists(destfile) && !overwrite) {
    return(destfile)
  }
  url <- envdata_release_url(tag = tag, file = basename(file), repo = repo)
  if (verbose) {
    message("Downloading ", basename(file), " from ", repo, "@", tag)
  }
  attempts <- 0L
  while (attempts < max_attempts) {
    attempts <- attempts + 1L
    status <- tryCatch(
      {
        if (.Platform$OS.type == "windows") {
          download.file(url, destfile, mode = "wb", quiet = !verbose)
        } else {
          download.file(url, destfile, quiet = !verbose)
        }
        0L
      },
      error = function(e) {
        message("Download attempt ", attempts, " failed: ", conditionMessage(e))
        1L
      },
      warning = function(w) {
        message("Download attempt ", attempts, " warning: ", conditionMessage(w))
        1L
      }
    )
    if (identical(status, 0L) && file.exists(destfile) && file.info(destfile)$size > 0) {
      return(invisible(destfile))
    }
    Sys.sleep(sleep_time)
  }
  stop("Failed to download ", url, " after ", max_attempts, " attempts")
}

#' Compatibility shim for existing call sites that still use robust_pb_download()
#' Prefer download_envdata_asset() for new code.
robust_pb_download <- function(file = NULL,
                               dest,
                               repo = ENVDATA_REPO,
                               tag,
                               overwrite = TRUE,
                               max_attempts = 10,
                               sleep_time = 10,
                               ...) {
  if (is.null(file)) {
    # Download all assets for a tag via piggyback when available
    if (!requireNamespace("piggyback", quietly = TRUE)) {
      stop("file=NULL download requires piggyback")
    }
    piggyback::pb_download(
      repo = repo,
      tag = tag,
      dest = dest,
      overwrite = overwrite,
      show_progress = FALSE
    )
    return(invisible(dest))
  }
  files <- file
  out <- character(length(files))
  for (i in seq_along(files)) {
    out[[i]] <- download_envdata_asset(
      tag = tag,
      file = files[[i]],
      dest_dir = dest,
      repo = repo,
      overwrite = overwrite,
      max_attempts = max_attempts,
      sleep_time = sleep_time
    )
  }
  invisible(out)
}

#' Compatibility shim for report/model release uploads
robust_pb_upload <- function(file,
                             repo = REPORT_REPO,
                             tag,
                             max_attempts = 10,
                             sleep_time = 1,
                             temp_directory = "data/temp/pb_test",
                             overwrite = TRUE,
                             ...) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("robust_pb_upload requires piggyback")
  }
  assets <- tryCatch(piggyback::pb_list(repo = repo), error = function(e) NULL)
  if (!is.null(assets) && !tag %in% assets$tag) {
    tryCatch(piggyback::pb_new_release(repo = repo, tag = tag), error = function(e) NULL)
  }
  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }
  files <- file
  for (f in files) {
    attempts <- 0L
    ok <- FALSE
    while (attempts < max_attempts && !ok) {
      attempts <- attempts + 1L
      tryCatch(
        piggyback::pb_upload(file = f, repo = repo, tag = tag, overwrite = overwrite),
        error = function(e) message("Upload attempt ", attempts, " failed: ", conditionMessage(e))
      )
      listed <- tryCatch(
        piggyback::pb_list(repo = repo, tag = tag),
        error = function(e) data.frame(file_name = character())
      )
      ok <- basename(f) %in% listed$file_name
      if (!ok) Sys.sleep(sleep_time)
    }
    if (!ok) stop("Failed to upload ", f, " to ", repo, "@", tag)
  }
  invisible(TRUE)
}

#' Read JSON from a URL
read_json_url <- function(url) {
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf), add = TRUE)
  download.file(url, tf, quiet = TRUE)
  jsonlite::fromJSON(tf, simplifyVector = FALSE)
}

#' Latest VI STAC item (MODIS+VIIRS composite) from the published catalog
get_latest_vi_stac_item <- function(collection_url = NULL) {
  if (is.null(collection_url)) {
    collection_url <- envdata_release_url(ENVDATA_RELEASE_TAGS$stac_vi, "vi_collection.json")
  }
  collection <- read_json_url(collection_url)
  item_links <- Filter(function(l) identical(l$rel, "item"), collection$links)
  if (length(item_links) == 0L) {
    stop("No VI STAC items found at ", collection_url)
  }
  hrefs <- vapply(item_links, function(l) l$href, character(1))
  # Item IDs / filenames are vi_YYYYMMDD.json — last lexical href is latest when sorted
  latest_href <- sort(hrefs)[length(hrefs)]
  read_json_url(latest_href)
}

#' Parse YYYYMMDD from a VI / fireage filename (vectorized for dplyr::mutate)
parse_envdata_yyyymmdd <- function(file_name) {
  file_name <- as.character(file_name)
  n <- length(file_name)
  out <- rep(as.Date(NA), n)
  if (!n) {
    return(out)
  }
  # Do not use regmatches(): it drops non-matches and shortens the vector.
  # The old m[[1]] path returned a length-1 Date, so mutate() recycled the
  # first filename's date onto every row and month filters found nothing.
  m <- regexpr("20[0-9]{6}", file_name)
  ok <- !is.na(m) & m > 0L
  if (any(ok)) {
    ml <- attr(m, "match.length")[ok]
    extracted <- substring(file_name[ok], m[ok], m[ok] + ml - 1L)
    out[ok] <- as.Date(extracted, format = "%Y%m%d")
  }
  out
}

#' Download domain.tif and return a SpatRaster
get_domain_raster <- function(dest_dir = "data/temp",
                              overwrite = FALSE) {
  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$static,
    file = "domain.tif",
    dest_dir = dest_dir,
    overwrite = overwrite
  )
  terra::rast(path)
}

#' Domain outline polygon (from domain mask band) as sf
get_domain_polygon <- function(dest_dir = "data/temp", overwrite = FALSE) {
  domain <- get_domain_raster(dest_dir = dest_dir, overwrite = overwrite)
  mask_name <- if ("domain" %in% names(domain)) "domain" else names(domain)[[1]]
  poly <- terra::as.polygons(domain[[mask_name]], values = TRUE, na.rm = TRUE) |>
    sf::st_as_sf()
  poly
}

#' Write remnants band from domain.tif to data/misc/remnants.tif
write_remnants_from_domain <- function(dest = "data/misc/remnants.tif",
                                       temp_dir = "data/temp") {
  domain <- get_domain_raster(dest_dir = temp_dir, overwrite = FALSE)
  if (!"remnants" %in% names(domain)) {
    stop("domain.tif is missing a 'remnants' band; available: ",
         paste(names(domain), collapse = ", "))
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(
    domain[["remnants"]],
    filename = dest,
    overwrite = TRUE,
    filetype = "COG",
    gdal = c("COMPRESS=LZW")
  )
  invisible(dest)
}

#' Scale EVI integer (×100) to reflectance-like units
scale_evi_integer <- function(x) {
  x / 100
}
