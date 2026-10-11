# Agent instructions — emma_report

## R style (required)

All R code written or edited by agents **must** follow the
[tidyverse style guide](https://style.tidyverse.org).

- Prefer tidyverse idioms (`dplyr`, `tidyr`, `purrr`, `ggplot2`, `readr`, `tibble`).
- Use the native pipe `|>` (not `%>%`).
- Formatting: 2-space indent, ~80-character lines, `snake_case` names, `<-` assignment.
- Restyle with [`styler`](https://styler.r-lib.org) (`styler::style_file()` /
  `styler::style_dir()`) when touching R sources; do not change behavior while restyling.
- Always-on details: [`.github/instructions/r-style.instructions.md`](.github/instructions/r-style.instructions.md).

## Project context

This is the `emma_report` Quarto / `targets` reporting module for EMMA
(Environmental Monitoring and Modelling for Africa). Key files:

- `_targets.R` — pipeline definition
- `R/` — pipeline and report helper functions
- `*.qmd` — Quarto site pages
- `.github/workflows/targets.yaml` — Actions workflow (`adamwilsonlab/emma` container)

Upstream modules: `emma_envdata` (data), `emma_model` (model outputs).
