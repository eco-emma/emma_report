library(rinat)

# Note: this function requires a manual download of observations iNaturalist observations to "prime the pump"
# Once this manual download is done, the species list is used to get updated records from iNat

inat_empty_sf <- function() {
  sf::st_sf(
    scientific_name = character(),
    observed_on = as.Date(character()),
    longitude = numeric(),
    latitude = numeric(),
    invasive = logical(),
    geometry = sf::st_sfc(crs = "WGS84")
  )
}

#' Build invasive iNat records from the CSV seed (cheap bbox filter).
#' Used for EMMA_CI_LIGHT, GitHub Actions, and as a live-API fallback.
inat_from_csv_seed <- function(inat_proj_data,
                               invasive_species,
                               oldest_date,
                               protected_areas) {
  inv <- inat_proj_data %>%
    filter(
      .data$scientific_name %in% invasive_species,
      .data$observed_on > oldest_date,
      !is.na(.data$longitude),
      !is.na(.data$latitude)
    )
  if (!nrow(inv)) {
    return(inat_empty_sf())
  }
  inv_sf <- inv %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      remove = FALSE,
      crs = "WGS84"
    )
  parks_bb <- protected_areas %>%
    st_transform("WGS84") %>%
    st_bbox() %>%
    st_as_sfc()
  inv_sf <- inv_sf[lengths(st_intersects(inv_sf, parks_bb)) > 0L, ]
  inv_sf %>%
    mutate(invasive = TRUE) %>%
    dplyr::select(
      "scientific_name",
      "observed_on",
      "longitude",
      "latitude",
      "invasive",
      "geometry"
    )
}

#' @param inat_data_location file path to an inat bulk download containing all species of interest
#' @param temp_directory a temporary directory to save things.  will be deleted.
#' @param max_attempts maximum number of attempts to download/upload before giving up
#' @param sleep_time time to pause between upload/download attempts
#' @param oldest_date Date as character (yyyy-mm-dd format). Any records older than this will be discarded
# function to pull invasive species records from inaturalist
get_inat_data <- function(inat_data_location = "data/manual_downloads/inat_project/observations-405358.csv",
                          invasive_age_months = 3,
                          invasive_taxa = c("Acacia", "Pinus", "Hakea", "Eucalyptus", "Leptospermum"),
                          temp_directory = "data/temp/inat/",
                          max_attemps = 10,
                          sleep_time = 10,
                          oldest_date = "2020-01-01",
                          protected_areas = protected_areas,
                          park_buffer = 10000,
                          verbose = TRUE) {

  protected_areas <- st_as_sf(protected_areas)

  if (dir.exists(file.path(temp_directory))) {
    unlink(file.path(temp_directory), recursive = TRUE, force = TRUE)
  }
  if (!dir.exists(file.path(temp_directory))) {
    dir.create(file.path(temp_directory), recursive = TRUE)
  }

  if (verbose) {
    message("Getting original inat data")
  }
  inat_proj_data <- read.csv(inat_data_location)
  species_list <- inat_proj_data %>%
    pull(scientific_name) %>%
    unique()
  invasive_species <- grep(
    species_list,
    pattern = paste(invasive_taxa, collapse = "|"),
    value = TRUE
  )

  # Light PR CI, or any GitHub Actions job: skip live API + park-union.
  # Full-mode Actions previously OOM-killed callr during st_union/buffer (~25m).
  use_csv_only <- identical(Sys.getenv("EMMA_CI_LIGHT"), "1") ||
    identical(Sys.getenv("GITHUB_ACTIONS"), "true")

  if (use_csv_only) {
    message(
      "Using CSV seed for inat_data (",
      length(invasive_species),
      " invasive taxa); skipping live iNaturalist API / park union"
    )
    inv_sf <- inat_from_csv_seed(
      inat_proj_data = inat_proj_data,
      invasive_species = invasive_species,
      oldest_date = oldest_date,
      protected_areas = protected_areas
    )
    if (!nrow(inv_sf)) {
      warning(
        "CSV seed had no invasive rows; returning empty sf.",
        call. = FALSE
      )
      return(inat_empty_sf())
    }
    message("inat_data rows from CSV seed: ", nrow(inv_sf))
    return(inv_sf)
  }

  if (verbose) {
    message("Getting domain")
  }
  domain <- get_domain_polygon(dest_dir = temp_directory, overwrite = TRUE) %>%
    st_transform("WGS84")

  # Cheap spatial filter (bbox), not st_union/buffer of all parks — that OOMs on GHA.
  if (verbose) {
    message("Building park/domain bbox filter")
  }
  parks_and_domain <- protected_areas %>%
    st_transform("WGS84") %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_union(st_as_sfc(st_bbox(domain)))

  full_records <- NULL

  if (verbose) {
    message("Starting for loop for downloading new inat data")
  }

  for (i in seq_along(invasive_species)) {
    message("Getting data for species ", i, " of ", length(invasive_species))

    data_i <- tryCatch(
      get_inat_obs(
        taxon_name = invasive_species[i],
        bounds = domain,
        maxresults = 2000
      ),
      error = function(e) e
    )

    # rinat often returns NULL when the API is down — not an error.
    if (inherits(data_i, "error") ||
        is.null(data_i) ||
        !inherits(data_i, "data.frame") ||
        nrow(data_i) == 0L) {
      next
    }

    data_i <- data_i %>%
      filter(.data$observed_on > oldest_date)

    data_i <- data_i %>%
      st_as_sf(
        coords = c("longitude", "latitude"),
        remove = FALSE
      )
    st_crs(data_i) <- st_crs("WGS84")

    data_i <- data_i %>%
      st_filter(parks_and_domain)

    if (nrow(data_i) == 0L) {
      next
    }

    full_records <- full_records %>%
      bind_rows(data_i)
    gc()
  }

  if (is.null(full_records) ||
      !inherits(full_records, "data.frame") ||
      nrow(full_records) == 0L) {
    warning(
      "No iNaturalist records from live API; falling back to CSV seed.",
      call. = FALSE
    )
    return(
      inat_from_csv_seed(
        inat_proj_data = inat_proj_data,
        invasive_species = invasive_species,
        oldest_date = oldest_date,
        protected_areas = protected_areas
      )
    )
  }

  full_records %>%
    mutate(invasive = TRUE)
}
