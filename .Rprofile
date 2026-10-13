# Activate project renv (lockfile mirrored from eco-emma/emma_docker).
# Soft-fail so interactive/CI sessions still work if bootstrap mismatches the image R.
if (file.exists("renv/activate.R")) {
  tryCatch(
    source("renv/activate.R"),
    error = function(e) {
      message("renv activate skipped: ", conditionMessage(e))
    }
  )
}
