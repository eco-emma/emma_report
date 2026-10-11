# Claude Code — Project Instructions

## R style (required)

Follow the [tidyverse style guide](https://style.tidyverse.org) for all R code.
Use [`styler`](https://styler.r-lib.org) to format sources. See
[`.github/instructions/r-style.instructions.md`](.github/instructions/r-style.instructions.md)
and [`AGENTS.md`](AGENTS.md).

## Project context

This is the `emma_report` R/targets + Quarto pipeline for the EMMA project. Key files:

- `_targets.R` — pipeline definition
- `R/` — all pipeline functions
- `*.qmd` / `_quarto.yml` — website
- `.github/workflows/targets.yaml` — Actions workflow
