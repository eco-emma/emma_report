#' Tidy static covariates from the new static_covariates.parquet product
#'
#' Legacy input was stable_data.gz.parquet with cellID + remnant_distance.tif.
#' New schema uses pid + remnants_distance on static_data/static_covariates.parquet.
tidy_static_data <- function(envdata, region, remnant_distance, sample_proportion) {

  cov_path <- file.path(envdata, "static_covariates.parquet")
  if (!file.exists(cov_path)) {
    # Allow envdata to be either a directory or the parquet path itself
    cov_path <- if (grepl("static_covariates\\.parquet$", envdata)) {
      envdata
    } else {
      stop("Expected static_covariates.parquet under ", envdata)
    }
  }

  data <- arrow::open_dataset(sources = cov_path)

  region_bbox <- sf::st_bbox(region, crs = sf::st_crs(4326)) |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = "EPSG:9221") |>
    sf::st_bbox()

  td <- data |>
    dplyr::collect()

  # Coordinates may be columns or embedded in geoparquet geometry
  if (!all(c("x", "y") %in% names(td)) && inherits(td, "sf")) {
    xy <- sf::st_coordinates(td)
    td$x <- xy[, 1]
    td$y <- xy[, 2]
  }
  if (!"pid" %in% names(td) && "cellID" %in% names(td)) {
    td$pid <- td$cellID
  }

  td <- td |>
    dplyr::mutate(
      cellID = .data$pid,
      fynbos = dplyr::case_when(
        .data$remnants > 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      model_domain = dplyr::case_when(
        .data$remnants_distance >= remnant_distance &
          .data$x > region_bbox$xmin & .data$x < region_bbox$xmax &
          .data$y > region_bbox$ymin & .data$y < region_bbox$ymax ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::group_by(.data$model_domain) |>
    dplyr::mutate(
      training = sample(
        c(TRUE, FALSE),
        size = dplyr::n(),
        replace = TRUE,
        prob = c(sample_proportion, 1 - sample_proportion)
      )
    ) |>
    dplyr::mutate(sample = ifelse(.data$model_domain, .data$training, FALSE)) |>
    dplyr::ungroup()

  td
}
