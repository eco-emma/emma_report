# EMMA Reports

Draft reports can be found [here](https://adamwilsonlab.github.io/emma_report/)

## Runtime: always use the emma Docker image

Every pipeline / CI / interactive run uses
[`adamwilsonlab/emma`](https://hub.docker.com/r/adamwilsonlab/emma)
(built from [`eco-emma/emma_docker`](https://github.com/eco-emma/emma_docker)).
There is **no host-side R or `renv::restore()`** path for this workflow.

| Where | Command |
|---|---|
| Laptop | `docker run --rm -v "$PWD":/work -w /work -e GITHUB_PAT adamwilsonlab/emma:latest Rscript -e 'targets::tar_make()'` |
| GitHub Actions | `container: adamwilsonlab/emma:latest` in `targets-data.yml` / `targets-website.yml` |
| HPC (CCR) | `apptainer exec … AdamWilsonLab-emma_docker-latest.sif Rscript -e 'targets::tar_make()'` (see emma_docker README) |

Package versions are pinned in the emma_docker **Dockerfile** (P3M snapshot).
To bump packages, change that Dockerfile, rebuild/push the image, then re-run.

## CI: data vs website split (Phase D / E)

Free `ubuntu-latest` only; both jobs use `adamwilsonlab/emma:latest`.

| Workflow | Role | Timeout |
|---|---|---|
| [`targets-data.yml`](.github/workflows/targets-data.yml) | `tar_make` **except** `website`; thin `*-runs` upload | 90m |
| [`targets-website.yml`](.github/workflows/targets-website.yml) | Restore `_targets`; **full** Quarto `website`; deploy **eco-emma** Pages | 120m |

- Schedule: **06:00 UTC** daily on the data workflow (`0 6 * * *`); website follows via `workflow_run` after a successful data run.
- **Phase E:** push/schedule run real Quarto (`EMMA_CI_LIGHT=0`). Job success means `website: full` (park HTML), not a stub. PRs keep `EMMA_CI_LIGHT=1` (light skip only).
- Pages deploy runs when the website job produced **non-stub** `docs/index.html` on `main`.

### GitHub Pages (eco-emma) — Adam ops

Switch Pages source to Actions (legacy **`main-runs` / `docs`** is incompatible with thin `*-runs`):

1. Repo **Settings → Pages → Build and deployment → Source: GitHub Actions** (not “Deploy from a branch”).
2. Confirm the **`github-pages`** environment exists (created automatically on first `deploy-pages` run, or create it under Settings → Environments).
3. Keep custom domain **`emma.eco`** / HTTPS as today; no change to org host (**eco-emma**, not a personal pages org).
4. No new secret is required for Pages when using `actions/deploy-pages` (`GITHUB_TOKEN` + `pages: write` / `id-token: write` on the deploy job).
5. Repo secret **`GH_PAT`** must have `contents:write` on `eco-emma/emma_report` so thin `*-runs` uploads succeed (soft-fail today).

Do **not** publish the site via `main-runs` after D0 thin upload (that tip has no `docs/`).

## CI `*-runs` cache (Phase D0 / R4b)

GitHub Actions pushes a **`<branch>-runs`** branch after each successful **data** job.
That ref is a **targets store cache only**:

| Allowed on `*-runs` tip | Forbidden (never force-add) |
|---|---|
| `_targets/` (regular git blobs, **not** Git LFS) | `docs/`, `reports/`, `.quarto/`, `site_libs/` |
| `.gitattributes` (no LFS filters) | `*.html.md`, ephemeral `data/temp/` |

Source checkout uses `fetch-depth: 1`; restore copies `_targets/` only.
Website / Pages deploy is **not** via `main-runs`.

**Why not Git LFS:** org LFS budget exhaustion blocks uploads of `_targets/objects/**`. Thin tips store objects as normal git blobs so orphan-reset and CI upload can push without an LFS transfer. Raising/clearing the eco-emma LFS budget is still needed if you want GitHub to drop *old* unreferenced LFS blobs from billing — that is separate from a successful thin force-push.

**Ops follow-up (reclaim ~5 GB history):** after thin-upload lands on `main`, orphan-reset bloated runs refs (force-push of the runs branch only — does not rewrite `main`):

```bash
DRY_RUN=1 ./scripts/orphan-reset-runs-branch.sh main-runs
CONFIRM=yes ./scripts/orphan-reset-runs-branch.sh main-runs
CONFIRM=yes ./scripts/orphan-reset-runs-branch.sh --delete-stale targets-runs
CONFIRM=yes ./scripts/orphan-reset-runs-branch.sh --delete-stale 2025_website-runs
```

`GIT_LFS_SKIP_SMUDGE=1` is set by the script; `git-lfs` is optional. If tip objects are still LFS pointers (smudge failed), the script **aborts** (refuses to force-push pointer text as blobs). Heal with a successful `targets-data` upload of real objects, then orphan-reset that clean tip. CI restore also deletes unresolved pointer files so `tar_make` can rebuild.

Runs upload is **soft-fail** (`continue-on-error: true`): data-job ✅ tracks `tar_make`, not runs-branch auth. If the summary shows `runs-upload: failed (soft)`, rotate `GH_PAT`.

# EMMA workflow overview

The EMMA workflow consists of four modules, each with a separate Github repo:

1) The Environmental Data module (https://github.com/eco-emma/emma_envdata)

2) The Modelling and Change Detection module (https://github.com/eco-emma/emma_model)

3) The Change Classification module (https://github.com/AdamWilsonLab/emma_change_classification)

4) The Reporting module (https://github.com/eco-emma/emma_report)


## Data

Parks, weather, iNaturalist invasives, vegetation index, fire age, and static grid
layers are pulled from [emma_envdata](https://github.com/eco-emma/emma_envdata)
GitHub Releases via `R/envdata_contract.R`. Report CI is **download-only** for
parks / weather / iNat — no live GSOD, rinat, or shapefile dissolve on GHA.

### emma_envdata contract (current)

| Product | Release tag | Key assets |
|---|---|---|
| Static grid / covariates | `static_data` | `domain.tif` (bands: pid, domain, remnants, remnants_distance), `domain.parquet`, `static_covariates.parquet` |
| **Parks (report SoT)** | `static_data` | `protected_areas.gpkg` (`name`, `type`, `area_ha`; EPSG:9221); also `domain_outline.gpkg`, `parks_buffer_10km.gpkg` |
| **Weather (report SoT)** | `weather_data` | `weather.parquet` (`STNID`, `YEARMODA`, `TEMP` °C, `PRCP` mm, `NAME`); `stations.gpkg` |
| **iNat invasives (report SoT)** | `inat_data` | `inat_invasives.gpkg` (WGS84: `scientific_name`, `observed_on`, `longitude`, `latitude`, `invasive`, geometry) |
| Park shapefile **inputs** (envdata writers only) | `manual-data` | Flat SACAD/SAPAD/CapeNature assets (no nested folders) — not report SoT |
| MODIS Terra / Aqua VI COGs | `vi_modis_terra_raster`, `vi_modis_aqua_raster` | `vi_modis_{terra\|aqua}_YYYYMMDD.tif` (bands: evi×100, doy) |
| VIIRS VI COGs | `vi_viirs_raster` | `vi_viirs_{snpp\|noaa20}_YYYYMMDD.tif` |
| VI parquet | `vi_modis_parquet` | `vi_modis_YYYYMMDD.parquet` (`pid`, `date`, `sensor`, `value`) |
| Fire age | `fireage` | `fireage_current.tif`, `fireage_{modis\|viirs}_YYYYMMDD.parquet` |
| STAC | `stac`, `stac-vi`, `stac-burn`, `stac-static` | `catalog.json` + collection/item JSON |

STAC root: https://github.com/eco-emma/emma_envdata/releases/download/stac/catalog.json

**Missing vs empty vs stale** (report data job):

| Case | Behaviour |
|---|---|
| Tag/asset **missing** (404) | **Fail loud** — broken contract; no live fallback |
| Asset present, **zero rows** | Succeed; Quarto empty weather / empty iNat guards |
| Asset present, **stale** | Succeed (freshness = nightly envdata + report cron `0 6 * * *` UTC) |

`emma_report@park_data` is **retired** as parks SoT (no upload / tag create).

Notes for report readers:

- Tabular join key is **`pid`** (legacy `cellID` is aliased where needed).
- Vegetation product is **EVI** (targets, paths, and report labels use `evi`).
- EVI integer storage uses scale **0.01** (divide by 100), matching emma_envdata writers.
- Remnants are read from `domain.tif` band `remnants` (no standalone `remnants.tif` on `static_data`).
- Climate figures cite GSOD via emma_envdata `weather_data`.

## CI `*-runs` cache (Phase D0 / R4b)

GitHub Actions pushes a **`<branch>-runs`** branch after each successful pipeline.
That ref is a **targets store cache only**:

| Allowed on `*-runs` tip | Forbidden (never force-add) |
|---|---|
| `_targets/` (regular git blobs, **not** Git LFS) | `docs/`, `reports/`, `.quarto/`, `site_libs/` |
| `.gitattributes` (no LFS filters) | `*.html.md`, ephemeral `data/temp/` |

Source checkout uses `fetch-depth: 1`; restore copies `_targets/` only.
Website / Pages deploy is **not** via `main-runs` (see CI full plan Phase D).

**Why not Git LFS:** org LFS budget exhaustion blocks LFS uploads. Thin tips and
orphan-reset push `_targets` as normal git blobs. Raise/clear the eco-emma LFS
budget only if you need GitHub to GC *old* unreferenced LFS objects from billing.

**Ops follow-up (reclaim ~5 GB history):** after this thin-upload lands on
`main`, orphan-reset bloated runs refs (force-push of the runs branch only —
does not rewrite `main`):

```bash
# Dry-run first
DRY_RUN=1 ./scripts/orphan-reset-runs-branch.sh main-runs

# Then, with write access:
CONFIRM=yes ./scripts/orphan-reset-runs-branch.sh main-runs

# Optional: drop unused legacy runs branches
CONFIRM=yes ./scripts/orphan-reset-runs-branch.sh --delete-stale targets-runs
CONFIRM=yes ./scripts/orphan-reset-runs-branch.sh --delete-stale 2025_website-runs
```

Do **not** run the force-push from unattended CI; Adam (or a maintainer) runs
it once manually after confirming a recent tip has `_targets/`. Pull the fixed
script from `main` first (no LFS track on the orphan tip).

Runs upload is **soft-fail** on all events (`continue-on-error: true`): pipeline
✅ tracks `tar_make`, not runs-branch auth. If the summary shows
`runs-upload: failed (soft)`, rotate `GH_PAT` to a token with
`contents:write` on `eco-emma/emma_report` so the thin cache can be reused.

## Releases

* Weather / stations: published by emma_envdata on tag `weather_data` (GSOD via [GSODR](https://cran.r-project.org/package=GSODR)).
* Parks: published by emma_envdata on tag `static_data` (`protected_areas.gpkg`).
* iNat invasives: published by emma_envdata on tag `inat_data`.

## Changelog

* Oct 2026
  - Phase C / R1–R3: parks / weather / iNat download-only from `static_data` / `weather_data` / `inat_data`; fail loud if missing; retire `park_data` upload; schedule `0 6 * * *` UTC.
  - Migrated envdata consumption to the eco-emma public release/STAC contract (`static_data`, `vi_*`, `fireage`, `stac*`).
  - Replaced legacy AdamWilsonLab tags (`current`, `processed_static`, old MODIS VI tags, `processed_most_recent_burn_dates`, `raw_static`) and `cellID` / `stable_data.gz.parquet` assumptions.
  - Renamed all report product identifiers from the old vegetation-index naming to `evi` (targets, Quarto, paths, docs).
  - Added `R/envdata_contract.R` as the single source of release-tag and download helpers for the report pipeline.
  - Runtime contract: always execute inside `adamwilsonlab/emma` (no host `renv` / `update.packages`); removed GEE from CI.
  - Phase D: split CI into `targets-data` / `targets-website`; thin `*-runs`; Pages deploy path for eco-emma.
  - Phase E: remove `GITHUB_ACTIONS` Quarto stub; push/schedule render full park site; CI secret `GH_PAT` (not `GH_PAT_BSM`); Pages deploy when non-stub `docs/` on `main`.
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
  - Historical note: an older Google Earth Engine path once broke envdata updates; GEE/`rgee`/`earthengine` have been removed from this report workflow (envdata now uses AppEEARS + GitHub Releases).
