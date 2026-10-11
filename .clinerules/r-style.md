# R Code Style

Applies to all `*.R` / `*.qmd` R sources in this project.

**Required:** Follow the [tidyverse style guide](https://style.tidyverse.org).
Format with [`styler`](https://styler.r-lib.org) (`style_file()` / `style_dir()`);
restyle without changing behavior.

## Tidyverse

- Use tidyverse idioms: `dplyr`, `tidyr`, `purrr`, `ggplot2`, `readr`, `tibble`.
- Use the native pipe `|>` (not `%>%`).
- Use `|> filter()`, `|> mutate()`, `|> summarise()` rather than base R subsetting
  or `apply` families, unless there is a significant performance reason.
- Avoid explicit `for` loops unless no vectorised or `purrr::map*` equivalent
  exists; prefer `purrr::map*` variants otherwise.

## Formatting

- 2-space indentation. Max line width 80 characters.
- One expression per line; do not chain unrelated operations onto one line.
- `snake_case` names; `<-` for assignment; spaces around operators / after commas.

## Script Style

- Write linear, top-to-bottom scripts that read like a narrative of the analysis.
- Do **not** wrap logic in a function unless the identical logic is genuinely reused
  in two or more places in the codebase.
- Prefer named intermediate objects over deeply nested pipes.

## Comments

- Comment the scientific rationale, not just the mechanics.
- Include units, CRS, and temporal coverage where relevant.
