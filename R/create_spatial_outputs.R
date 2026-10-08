# Spatial Predictions

create_spatial_outputs <- function(envdata, envvars, model_results, data_training) {

  # Normalize pixel id: envdata uses pid; older tables used cellID
  if (!"pid" %in% names(envdata) && "cellID" %in% names(envdata)) {
    envdata$pid <- envdata$cellID
  }
  if (!"pid" %in% names(data_training) && "cellID" %in% names(data_training)) {
    data_training$pid <- data_training$cellID
  }

  td <- envdata %>%
    left_join(dplyr::select(data_training, pid, dplyr::any_of("cellID")), by = "pid")

  xmat <- as.matrix(td[, envvars])

  betas <- model_results %>%
    filter(type == "beta") %>%
    mutate(term = sub("_.*$", "", parameter)) %>%
    dplyr::select(term, median, xname)

  td$gamma <- as.vector(xmat %*% filter(betas, term == "gamma")$median)
  td$lambda <- as.vector(xmat %*% filter(betas, term == "lambda")$median)
  td$A <- as.vector(xmat %*% filter(betas, term == "A")$median)
  td$RT <- td$lambda * log(200 * td$gamma)

  # Align predictions to the published domain pid grid (static_data/domain.tif)
  domain_path <- "data/domain.tif"
  if (!file.exists(domain_path)) {
    domain_path <- download_envdata_asset(
      tag = ENVDATA_RELEASE_TAGS$static,
      file = "domain.tif",
      dest_dir = "data",
      overwrite = FALSE
    )
  }
  domain_terra <- terra::rast(domain_path)
  pid_layer <- if ("pid" %in% names(domain_terra)) {
    domain_terra[["pid"]]
  } else {
    domain_terra[[1]]
  }

  pid_vals <- as.vector(terra::values(pid_layer))
  vmat <- data.frame(pid = pid_vals) %>%
    left_join(dplyr::select(td, pid, gamma, lambda, A, RT), by = "pid") %>%
    dplyr::select(-pid)

  layers <- lapply(seq_len(ncol(vmat)), function(i) {
    layer <- pid_layer
    terra::values(layer) <- vmat[[i]]
    names(layer) <- colnames(vmat)[i]
    layer
  })
  rasters <- do.call(c, layers)
  rasters
}
