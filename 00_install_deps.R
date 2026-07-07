# 00_install_deps.R — install all R packages needed by the pipeline (01..17)
# Called automatically by run_pipeline.py before any pipeline step.

needed <- c(
  "polmineR", "data.table", "here", "fs",
  "tidyverse", "arrow", "glue",
  "manifestoR", "stringr", "readxl", "jsonlite",
  "ggplot2", "openxlsx"
)

missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) == 0L) {
  cat("All", length(needed), "required R packages are installed.\n")
} else {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0L) {
    stop("Failed to install: ", paste(still_missing, collapse = ", "))
  }
  cat("All packages installed successfully.\n")
}
