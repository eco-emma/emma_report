#' @description Function to scrape our envdata repo and download the remnant data layers
get_remnants_polygons <- function() {
  # get a list of files

  github_files("AdamWilsonLab/emma_envdata") %>%
    as.data.frame() %>%
    rename(file = ".") %>%
    filter(
      grepl(pattern = "Remnant", x = file),
      grepl(pattern = ".", x = file, fixed = TRUE)
    ) -> remnant_files

  # create the directory if needed

  if (!dir.exists(dirname(remnant_files[1, ]))) {
    dir.create(dirname(remnant_files[1, ]), recursive = TRUE)
  }

  # download the files

  for (i in 1:nrow(remnant_files)) {
    download.file(
      url = file.path("https://github.com/AdamWilsonLab/emma_envdata/raw/main/", remnant_files[i, ]),
      destfile = remnant_files[i, ]
    )
  }

  # load the shapefile

  remnant_files %>%
    filter(grepl(pattern = "shp$", x = file)) %>%
    pull(file) %>%
    sf::read_sf()
}

# Code from https://rdrr.io/github/ScottishCovidResponse/SCRCdataAPI/src/R/github_files.R

github_files <- function(repo) {
  req <- httr::GET(paste("https://api.github.com/repos", repo,
    "git/trees/main?recursive=1",
    sep = "/"
  ))

  if (req$status_code == 404) {
    req <- httr::GET(paste("https://api.github.com/repos", repo,
      "git/trees/master?recursive=1",
      sep = "/"
    ))
  }

  httr::stop_for_status(req)

  unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
}
