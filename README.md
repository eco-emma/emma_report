# EMMA Reports

Draft reports can be found [here](https://adamwilsonlab.github.io/emma_report/)

# EMMA workflow overview

The EMMA workflow consists of four modules, each with a separate Github repo:

1) The Environmental Data module (https://github.com/eco-emma/emma_envdata)

2) The Modelling and Change Detection module (https://github.com/eco-emma/emma_model)

3) The Change Classification module (https://github.com/AdamWilsonLab/emma_change_classification)

4) The Reporting module (https://github.com/eco-emma/emma_report)


## Data

Weather data are stored via emma_report releases. Vegetation index, fire age, and static layers are pulled from [emma_envdata](https://github.com/eco-emma/emma_envdata) GitHub Releases using the public contract in `R/envdata_contract.R`.

### emma_envdata contract (current)

| Product | Release tag | Key assets |
|---|---|---|
| Static grid / covariates | `static_data` | `domain.tif` (bands: pid, domain, remnants, remnants_distance), `domain.parquet`, `static_covariates.parquet` |
| MODIS Terra / Aqua VI COGs | `vi_modis_terra_raster`, `vi_modis_aqua_raster` | `vi_modis_{terra\|aqua}_YYYYMMDD.tif` (bands: evi×100, doy) |
| VIIRS VI COGs | `vi_viirs_raster` | `vi_viirs_{snpp\|noaa20}_YYYYMMDD.tif` |
| VI parquet | `vi_modis_parquet` | `vi_modis_YYYYMMDD.parquet` (`pid`, `date`, `sensor`, `value`) |
| Fire age | `fireage` | `fireage_current.tif`, `fireage_{modis\|viirs}_YYYYMMDD.parquet` |
| STAC | `stac`, `stac-vi`, `stac-burn`, `stac-static` | `catalog.json` + collection/item JSON |

STAC root: https://github.com/eco-emma/emma_envdata/releases/download/stac/catalog.json

Notes for report readers:

- Tabular join key is **`pid`** (legacy `cellID` is aliased where needed).
- Vegetation product is **EVI** (targets, paths, and report labels use `evi`).
- EVI integer storage uses scale **0.01** (divide by 100), matching emma_envdata writers.
- Remnants are read from `domain.tif` band `remnants` (no standalone `remnants.tif` on `static_data`).

## Releases

* GSOD contains weather data from the [Global Surface Summary of the Day](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00516/html), which are extracted using [GSODR](https://cran.r-project.org/package=GSODR). The file "gsod_stations" contains metadata on the stations within the focal domain, and the other files contain data for each weather station in the area. Files are in Apache Arrow format.

* NOAA contains weather data from [Integrated Surface Database](https://www.ncei.noaa.gov/pub/data/noaa/isd-format-document.pdf), which are extracted using [rnoaa](https://cran.r-project.org/package=rnoaa). The file "noaa_stations" contains metadata on the stations within the focal domain, and the other files contain data for each weather station in the area.  Files are in Apache Arrow format.

## Changelog

* Oct 2026
  - Migrated envdata consumption to the eco-emma public release/STAC contract (`static_data`, `vi_*`, `fireage`, `stac*`).
  - Replaced legacy AdamWilsonLab tags (`current`, `processed_static`, old MODIS VI tags, `processed_most_recent_burn_dates`, `raw_static`) and `cellID` / `stable_data.gz.parquet` assumptions.
  - Renamed all report product identifiers from the old vegetation-index naming to `evi` (targets, Quarto, paths, docs).
  - Added `R/envdata_contract.R` as the single source of release-tag and download helpers for the report pipeline.
* May 14, 2024
  - Added a new section to the README to describe the data used by the workflow and where it is stored.  Added a new section to describe the releases and what they contain.  Added a new section to describe the changelog.
* May 13, 2024 - Significant updates to the report website including:
  - Use `tar_terra_rast` and `tar_terra_vect` for spatial targets objects. This has the advantage of writing a .tif (or whatever) format for the target output itself, which eliminates the need to push the data to a release for 'publishing.'
  - Website Updates
    - Switch from separate html pages to a quarto website with multiple linked pages
    - added tar_quarto to build the project as part of the targets workflow
    - Updated the main index.html page to include a summary of the project and links to the other pages
    - Added a new page for the project summary with abstract, team, etc.
    - retired the emma.eco google sites page and replaced with this generated one.
    - Park-level reports updates
      - added a new target `report_qmd_dir` that generates all the park-level .qmd files prior to running the website build (so they are included in the site architecture)
      - condensed and enriched the time-since-fire plots
      - condensed the weather data into a three-panel linked dygraph (temp, precip, spi)
  - The Google Earth api changed a few months ago and broke the automatic updates in emma_envdata.
