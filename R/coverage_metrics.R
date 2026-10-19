#' Coverage / assessment area metrics for the EMMA report site
#'
#' Computes quiet-footer KPIs (decisions **2D**, **3B**, **4B**, **10B**, **12A**):
#' domain km², parks-union km², valid latest fused EVI km², and fire-age
#' known + censored km² after masking cells where all fireage bands are NA.
#' Park rows use the same definitions with denominator = domain cells
#' intersecting the park polygon (`touches = TRUE`).

#' Cell area in km² for the 500 m EMMA domain grid (EPSG:9221).
emma_cell_km2 <- function(r = NULL) {
  if (!is.null(r) && inherits(r, "SpatRaster")) {
    ha <- tryCatch(
      as.numeric(terra::cellSize(r, unit = "ha")[1]),
      error = function(e) NA_real_
    )
    if (is.finite(ha) && ha > 0) {
      return(ha / 100)
    }
    res_m <- terra::res(r)
    if (length(res_m) >= 2L && all(is.finite(res_m))) {
      return(abs(res_m[[1]] * res_m[[2]]) / 1e6)
    }
  }
  0.25
}

#' Logical domain mask from domain.tif (band `domain` == 1).
domain_cell_mask <- function(domain_rast) {
  stopifnot(inherits(domain_rast, "SpatRaster"))
  band <- if ("domain" %in% names(domain_rast)) {
    domain_rast[["domain"]]
  } else {
    domain_rast[[min(2L, terra::nlyr(domain_rast))]]
  }
  vals <- terra::values(band, mat = FALSE)
  !is.na(vals) & vals == 1
}

#' Fire-age known / censored masks after all-NA band mask (**3B**).
#'
#' Cells where `fire_age_days`, `last_burn_date`, and `is_censored` are all
#' nodata/NA are dropped. Remaining cells with `is_censored == 1` (or age NA)
#' count as censored; `is_censored == 0` with a finite age count as known.
fire_age_coverage_masks <- function(fireage_rast, domain_mask) {
  stopifnot(inherits(fireage_rast, "SpatRaster"))
  nms <- names(fireage_rast)
  age <- if ("fire_age_days" %in% nms) {
    fireage_rast[["fire_age_days"]]
  } else {
    fireage_rast[[1]]
  }
  last <- if ("last_burn_date" %in% nms) {
    fireage_rast[["last_burn_date"]]
  } else if (terra::nlyr(fireage_rast) >= 2L) {
    fireage_rast[[2]]
  } else {
    NULL
  }
  cens <- if ("is_censored" %in% nms) {
    fireage_rast[["is_censored"]]
  } else if (terra::nlyr(fireage_rast) >= 3L) {
    fireage_rast[[3]]
  } else {
    NULL
  }

  age_v <- terra::values(age, mat = FALSE)
  last_v <- if (is.null(last)) {
    rep(NA_real_, length(age_v))
  } else {
    terra::values(last, mat = FALSE)
  }
  cens_v <- if (is.null(cens)) {
    rep(NA_real_, length(age_v))
  } else {
    terra::values(cens, mat = FALSE)
  }

  all_na <- is.na(age_v) & is.na(last_v) & is.na(cens_v)
  keep <- as.logical(domain_mask) & !all_na
  if (!is.null(cens)) {
    known <- keep & !is.na(cens_v) & cens_v == 0
    censored <- keep & !is.na(cens_v) & cens_v == 1
  } else {
    # Fallback when is_censored band missing: finite age = known, else censored.
    known <- keep & !is.na(age_v)
    censored <- keep & is.na(age_v)
  }
  list(keep = keep, known = known, censored = censored)
}

#' Rasterize park polygons onto the domain grid (domain cells intersecting park).
park_domain_masks <- function(domain_rast, protected_areas) {
  stopifnot(inherits(domain_rast, "SpatRaster"))
  parks <- sf::st_as_sf(protected_areas)
  if (is.na(sf::st_crs(parks))) {
    stop("protected_areas has no CRS")
  }
  # Align CRS to domain for extract/rasterize
  domain_crs <- terra::crs(domain_rast)
  if (!is.na(domain_crs) && !identical(sf::st_crs(parks), sf::st_crs(domain_crs))) {
    parks <- sf::st_transform(parks, crs = domain_crs)
  }
  pid_layer <- if ("pid" %in% names(domain_rast)) {
    domain_rast[["pid"]]
  } else {
    domain_rast[[1]]
  }
  domain_mask <- domain_cell_mask(domain_rast)
  ncell <- terra::ncell(domain_rast)

  out <- vector("list", nrow(parks))
  names(out) <- as.character(parks$name)
  for (i in seq_len(nrow(parks))) {
    ex <- terra::extract(
      x = pid_layer,
      y = terra::vect(parks[i, , drop = FALSE]),
      cells = TRUE,
      touches = TRUE
    )
    cells <- unique(stats::na.omit(as.integer(ex$cell)))
    mask <- rep(FALSE, ncell)
    if (length(cells)) {
      mask[cells] <- TRUE
    }
    out[[i]] <- mask & domain_mask
  }
  out
}

#' Build coverage metrics list used by index / Data / park Quarto pages.
#'
#' @param most_recent_evi.tif SpatRaster fused latest EVI
#' @param most_recent_evi_date Date of the fused composite
#' @param protected_areas park polygons (SpatVector or sf)
#' @param temp_directory download cache for domain.tif / fireage_current.tif
compute_coverage_metrics <- function(most_recent_evi.tif,
                                     most_recent_evi_date,
                                     protected_areas,
                                     temp_directory = "data/temp/") {
  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }

  domain_path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$static,
    file = "domain.tif",
    dest_dir = temp_directory,
    overwrite = FALSE
  )
  fireage_path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$fireage,
    file = "fireage_current.tif",
    dest_dir = temp_directory,
    overwrite = FALSE
  )

  domain_rast <- terra::rast(domain_path)
  fireage_rast <- terra::rast(fireage_path)
  evi <- most_recent_evi.tif
  if (!inherits(evi, "SpatRaster")) {
    stop("most_recent_evi.tif must be a SpatRaster")
  }

  # Align EVI / fireage to domain grid when needed
  if (!terra::compareGeom(evi, domain_rast, stopOnError = FALSE)) {
    evi <- terra::resample(evi, domain_rast[[1]], method = "near")
  }
  if (!terra::compareGeom(fireage_rast, domain_rast, stopOnError = FALSE)) {
    fireage_rast <- terra::resample(fireage_rast, domain_rast[[1]], method = "near")
  }

  cell_km2 <- emma_cell_km2(domain_rast)
  domain_mask <- domain_cell_mask(domain_rast)
  evi_vals <- terra::values(evi[[1]], mat = FALSE)
  valid_evi <- domain_mask & !is.na(evi_vals)

  park_masks <- park_domain_masks(domain_rast, protected_areas)
  parks_union <- Reduce(`|`, park_masks, init = rep(FALSE, length(domain_mask)))

  fire <- fire_age_coverage_masks(fireage_rast, domain_mask)

  summarise_scope <- function(mask) {
    n_denom <- sum(mask, na.rm = TRUE)
    n_evi <- sum(mask & valid_evi, na.rm = TRUE)
    n_known <- sum(mask & fire$known, na.rm = TRUE)
    n_cens <- sum(mask & fire$censored, na.rm = TRUE)
    list(
      n_cells = as.integer(n_denom),
      km2 = n_denom * cell_km2,
      valid_evi_n_cells = as.integer(n_evi),
      valid_evi_km2 = n_evi * cell_km2,
      valid_evi_pct = if (n_denom > 0) 100 * n_evi / n_denom else NA_real_,
      fire_known_n_cells = as.integer(n_known),
      fire_known_km2 = n_known * cell_km2,
      fire_censored_n_cells = as.integer(n_cens),
      fire_censored_km2 = n_cens * cell_km2,
      fire_known_pct = if (n_denom > 0) 100 * n_known / n_denom else NA_real_,
      fire_censored_pct = if (n_denom > 0) 100 * n_cens / n_denom else NA_real_
    )
  }

  domain_stats <- summarise_scope(domain_mask)
  parks_union_stats <- summarise_scope(parks_union)

  park_rows <- lapply(names(park_masks), function(nm) {
    s <- summarise_scope(park_masks[[nm]])
    data.frame(
      name = nm,
      denom_n_cells = s$n_cells,
      denom_km2 = s$km2,
      valid_evi_km2 = s$valid_evi_km2,
      valid_evi_pct = s$valid_evi_pct,
      fire_known_km2 = s$fire_known_km2,
      fire_censored_km2 = s$fire_censored_km2,
      fire_known_pct = s$fire_known_pct,
      fire_censored_pct = s$fire_censored_pct,
      stringsAsFactors = FALSE
    )
  })
  parks_df <- if (length(park_rows)) {
    dplyr::bind_rows(park_rows)
  } else {
    data.frame(
      name = character(),
      denom_n_cells = integer(),
      denom_km2 = numeric(),
      valid_evi_km2 = numeric(),
      valid_evi_pct = numeric(),
      fire_known_km2 = numeric(),
      fire_censored_km2 = numeric(),
      fire_known_pct = numeric(),
      fire_censored_pct = numeric(),
      stringsAsFactors = FALSE
    )
  }

  list(
    cell_km2 = cell_km2,
    evi_as_of = as.Date(most_recent_evi_date),
    fire_mask_note = paste(
      "Fire-age known/censored exclude cells where all fireage bands",
      "(age, last burn, is_censored) are NA; censored = never-burned /",
      "right-censored (`is_censored = 1`)."
    ),
    domain = domain_stats,
    parks_union = parks_union_stats,
    valid_evi = list(
      n_cells = domain_stats$valid_evi_n_cells,
      km2 = domain_stats$valid_evi_km2,
      pct_of_domain = domain_stats$valid_evi_pct
    ),
    fire_age = list(
      known_km2 = domain_stats$fire_known_km2,
      censored_km2 = domain_stats$fire_censored_km2,
      known_pct = domain_stats$fire_known_pct,
      censored_pct = domain_stats$fire_censored_pct
    ),
    parks = parks_df
  )
}

#' Format km² for quiet assessment strips
fmt_km2 <- function(x, digits = 0) {
  if (length(x) != 1L || !is.finite(x)) {
    return("—")
  }
  format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Markdown for the quiet domain coverage strip (index / Data page)
format_coverage_domain_md <- function(coverage_metrics) {
  m <- coverage_metrics
  paste0(
    '<div class="emma-coverage-strip" markdown="1">\n\n',
    "**Coverage** — domain ",
    fmt_km2(m$domain$km2), " km² · parks-union ",
    fmt_km2(m$parks_union$km2), " km² · valid latest fused EVI ",
    fmt_km2(m$valid_evi$km2), " km²",
    if (!is.null(m$evi_as_of) && !is.na(m$evi_as_of)) {
      paste0(" (EVI as of ", format(m$evi_as_of, "%Y-%m-%d"), ")")
    } else {
      ""
    },
    " · fire-age known ",
    fmt_km2(m$fire_age$known_km2), " km² / censored ",
    fmt_km2(m$fire_age$censored_km2), " km². ",
    "*", m$fire_mask_note, "* ",
    "[Definitions & downloads](data.html#coverage-assessment-metrics).\n\n",
    "</div>\n"
  )
}

#' Markdown for a single park coverage panel
format_coverage_park_md <- function(coverage_metrics, park_name) {
  parks <- coverage_metrics$parks
  row <- parks[parks$name == park_name, , drop = FALSE]
  if (nrow(row) != 1L) {
    return(paste0(
      '<div class="emma-coverage-strip emma-coverage-park" markdown="1">\n\n',
      "**Coverage** — park metrics unavailable for ", park_name, ".\n\n</div>\n"
    ))
  }
  paste0(
    '<div class="emma-coverage-strip emma-coverage-park" markdown="1">\n\n',
    "**Coverage (park)** — denominator ",
    fmt_km2(row$denom_km2), " km² (domain cells intersecting park) · ",
    "valid fused EVI ", fmt_km2(row$valid_evi_km2), " km² (",
    sprintf("%.0f", row$valid_evi_pct), "%) · ",
    "fire-age known ", fmt_km2(row$fire_known_km2), " km² / censored ",
    fmt_km2(row$fire_censored_km2), " km². ",
    "*", coverage_metrics$fire_mask_note, "* ",
    "[Data sources & downloads →](../data.html)\n\n",
    "</div>\n"
  )
}
