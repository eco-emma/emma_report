---
applyTo: "**/*.{R,r,Rmd,qmd}"
---

## R Code Style

Follow the [tidyverse style guide](https://style.tidyverse.org) for all R code
in this repository. Use [`styler`](https://styler.r-lib.org)
(`styler::style_file()` / `styler::style_dir()`) to apply formatting; restyle
must not change behavior.

### Tidyverse
- Use tidyverse idioms: `dplyr`, `tidyr`, `purrr`, `ggplot2`, `readr`, `tibble`.
- Use the native pipe `|>` (not `%>%`).
- Use `|> filter()`, `|> mutate()`, `|> summarise()` rather than base R subsetting
  or `apply` families, unless there is a significant performance reason.
- Avoid explicit `for` loops unless no vectorised or `purrr::map*` equivalent
  exists (e.g., loops with stateful side-effects or where each iteration depends
  on the result of the previous one); prefer `purrr::map*` variants otherwise.

### Formatting (tidyverse / styler)
- 2-space indentation. Aim for a max line width of 80 characters.
- One expression per line; do not chain unrelated operations onto one line.
- Use `snake_case` for object and function names; use `<-` for assignment.
- Put spaces around most operators and after commas (per the style guide).
- Prefer existing project naming; when renaming for style only, keep behavior
  identical and update all call sites in the same change.

### Script style
- Write linear, top-to-bottom scripts that read like a narrative of the analysis.
- Do **not** wrap logic in a function unless the identical logic is genuinely reused
  in two or more places in the codebase.
- Avoid helper functions, utility wrappers, and abstraction layers that exist only
  to shorten a single call site — inline that code instead.
- Keep all parameters (file paths, thresholds, product names) as named objects near
  the top of the script so they are easy to find and change.
- Prefer named intermediate objects (`filtered_sites`, `monthly_means`) over deeply
  nested pipes — it aids debugging and readability.

### Comments
- Comment the scientific rationale, not just the mechanics
  (e.g., "# resample to match MODIS 500m grid" not "# resample raster").
- Include units, CRS, and temporal coverage in comments wherever they are relevant.
