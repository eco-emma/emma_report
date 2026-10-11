# EMMA Report Copilot Instructions

## Project Overview

This is the EMMA (Environmental Monitoring & Modeling Application) **reporting**
module — a Quarto website and `targets` pipeline that assembles park-level and
project reports from `emma_envdata` and `emma_model` outputs.

**Architecture**: Part of a 4-module pipeline:

1. [emma_envdata](https://github.com/eco-emma/emma_envdata) — Environmental data preparation
2. [emma_model](https://github.com/eco-emma/emma_model) — Bayesian modeling & change detection
3. emma_change_classification — Classification
4. **emma_report** (this repo) — Reporting

## R style (required)

All R code **must** follow the [tidyverse style guide](https://style.tidyverse.org).

- Prefer tidyverse idioms and the native pipe `|>`.
- Format with [`styler`](https://styler.r-lib.org); do not change behavior when restyling.
- File-scoped rules: [`.github/instructions/r-style.instructions.md`](instructions/r-style.instructions.md).

## Core Workflow (targets)

Orchestrated by [_targets.R](_targets.R):

```r
targets::tar_make()
```

Typical stages include downloading model/envdata release artifacts, preparing
park polygons and fire/NDVI summaries, generating park-level `.qmd` pages, and
building the Quarto site.

## Key Files

- [_targets.R](_targets.R): Pipeline definition
- [R/](R/): Helper functions (report generation, data pulls, spatial outputs)
- [index.qmd](index.qmd), [about.qmd](about.qmd), [model_summary.qmd](model_summary.qmd): Site pages
- [_quarto.yml](_quarto.yml): Quarto project config
- [.github/workflows/targets.yaml](.github/workflows/targets.yaml): CI (`adamwilsonlab/emma`)

## When Making Changes

- New helpers go in `R/` and are sourced from `_targets.R`.
- Prefer release downloads via existing piggyback helpers; avoid interactive calls.
- Keep CI-friendly paths (no hardcoded local server mounts).
- After editing R, ensure tidyverse / styler formatting before committing.
