#' Sync a working set of emma_envdata release assets into a local folder
#'
#' Replaces the legacy `tag = "current"` bulk download. Pulls static domain
#' products plus the latest multi-sensor VI COGs (STAC) and fireage snapshot.
sync_envdata <- function(repo = ENVDATA_REPO,
                         path = "data/envdata/") {

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  static_files <- c(
    "domain.tif",
    "domain.parquet",
    "static_covariates.parquet"
  )
  for (f in static_files) {
    download_envdata_asset(
      tag = ENVDATA_RELEASE_TAGS$static,
      file = f,
      dest_dir = path,
      repo = repo,
      overwrite = FALSE
    )
  }

  download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$fireage,
    file = "fireage_current.tif",
    dest_dir = path,
    repo = repo,
    overwrite = TRUE
  )

  latest <- get_most_recent_evi_file()
  for (i in seq_len(nrow(latest))) {
    download_envdata_asset(
      tag = latest$tag[[i]],
      file = latest$file_name[[i]],
      dest_dir = path,
      repo = repo,
      overwrite = TRUE
    )
  }

  invisible(path)
}
