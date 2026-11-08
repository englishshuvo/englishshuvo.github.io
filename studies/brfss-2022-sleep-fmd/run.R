# One command, from the repository root:
#   Rscript studies/brfss-2022-sleep-fmd/run.R
# Stages: test, audit, estimate, all (default). all runs tests, then audit, then estimates.
# Respondent-level files stay under data/ and are not committed.

args <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args)) args[[1]] else "all"
if (!stage %in% c("test", "audit", "estimate", "all", "check")) {
  stop("Stage must be test, audit, estimate, all, or check.")
}

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
root <- normalizePath(dirname(file_arg))
Sys.setenv(BRFSS_STUDY_ROOT = root)

source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "recodes.R"))
source(file.path(root, "R", "estimands.R"))
source(file.path(root, "R", "download.R"))
source(file.path(root, "R", "audit.R"))
source(file.path(root, "R", "estimate.R"))
source(file.path(root, "R", "figures.R"))
source(file.path(root, "R", "report.R"))

need <- c("haven", "survey", "ggplot2", "jsonlite")
missing_pkg <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkg)) {
  stop(
    "Missing R packages: ", paste(missing_pkg, collapse = ", "),
    ". Install them, or restore the lockfile with renv::restore() from the study directory."
  )
}

run_unit_tests <- function() {
  files <- c(
    "tests/test_recodes.R",
    "tests/test_estimands.R",
    "tests/test_simulation.R"
  )
  for (f in files) {
    message("Running ", f)
    status <- system2("Rscript", file.path(root, f))
    if (!identical(status, 0L)) stop("Test file failed: ", f)
  }
}

if (stage == "check") {
  check_case_study(root)
  quit(status = 0)
}
if (stage %in% c("test", "all")) {
  run_unit_tests()
}
if (stage == "test") quit(status = 0)

raw_dir <- file.path(root, "data", "raw")
int_dir <- file.path(root, "data", "intermediate")
out_dir <- file.path(root, "output")
dir.create(int_dir, recursive = TRUE, showWarnings = FALSE)

zip_info <- download_llcp(file.path(raw_dir, "LLCP2022XPT.zip"))
xpt_info <- unzip_llcp(zip_info$path, raw_dir)

cache_path <- file.path(int_dir, "llcp2022-selected.rds")
cache_ok <- FALSE
if (file.exists(cache_path)) {
  cached <- readRDS(cache_path)
  cache_ok <- identical(cached$xpt_sha256, xpt_info$sha256)
  if (cache_ok) raw <- cached$raw
}
if (!cache_ok) {
  message("Reading selected columns from the transport file.")
  peek <- haven::read_xpt(xpt_info$path, n_max = 1)
  n_columns_full <- ncol(peek)
  rm(peek)
  raw <- as.data.frame(haven::read_xpt(
    xpt_info$path,
    col_select = tidyselect::all_of(c(ANALYSIS_COLUMNS, AUDIT_ONLY_COLUMNS))
  ))
  attr(raw, "n_columns_full") <- n_columns_full
  saveRDS(list(xpt_sha256 = xpt_info$sha256, raw = raw, n_columns_full = n_columns_full), cache_path)
} else {
  n_columns_full <- cached$n_columns_full
}

message("Auditing schema, recodes, and design. No association is estimated in this step.")
audit <- audit_microdata(raw)
audit$n_columns_in_transport_file <- n_columns_full

pkg_version <- function(pkg) as.character(utils::packageVersion(pkg))
provenance <- list(
  study = "BRFSS 2022 usual sleep and frequent mental distress",
  independent_sample_work = TRUE,
  landing_page = BRFSS_SOURCE$landing_url,
  data_url = BRFSS_SOURCE$data_url,
  codebook_url = BRFSS_SOURCE$codebook_url,
  design_guide_url = BRFSS_SOURCE$design_url,
  overview_url = BRFSS_SOURCE$overview_url,
  comparability_url = BRFSS_SOURCE$compare_url,
  calculated_variables_url = BRFSS_SOURCE$calculated_url,
  weighting_url = BRFSS_SOURCE$weighting_url,
  zip_sha256 = zip_info$sha256,
  zip_bytes = zip_info$bytes,
  zip_retrieval = zip_info$retrieval,
  verified_at_utc = zip_info$verified_at_utc,
  xpt_member_name = xpt_info$member_name_repr,
  xpt_sha256 = xpt_info$sha256,
  xpt_bytes = xpt_info$bytes,
  n_records = audit$n_records,
  n_columns_in_transport_file = n_columns_full,
  landing_page_notes = list(
    published_record_count = 445132,
    landing_page_zip_label = "64.3 MB",
    landing_page_variable_count_text = 326,
    observed_zip_bytes = zip_info$bytes,
    observed_variable_count = n_columns_full,
    interpretation = "The URL is the official link. The record count matches 445,132 and the codebook frequencies for the analysis items match. The page's size label and variable count do not match the object that URL returned on the retrieval date. Those labels are treated as stale. The inner member name has a trailing space and is renamed after extraction."
  ),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  packages = list(
    survey = pkg_version("survey"),
    haven = pkg_version("haven"),
    ggplot2 = pkg_version("ggplot2"),
    jsonlite = pkg_version("jsonlite")
  ),
  quarto_note = "Case study is rendered with the Quarto version pinned in .github/workflows/publish.yml (1.10.18) when that binary is available."
)

if (stage == "audit") {
  write_outputs(out_dir, provenance, audit, estimates = NULL)
  message("Audit complete. Estimates were not fit.")
  quit(status = 0)
}

message("Preparing the analysis frame. _AGE80, _SEX, and BIRTHSEX are dropped here.")
slim <- slim_for_analysis(raw)
rm(raw)
prep <- prepare_frame(slim)
rm(slim)

message("Fitting prespecified contrasts.")
estimates <- estimate_all(prep)
fig <- file.path(out_dir, "figure-primary.png")
write_primary_figure(estimates$primary, fig)
write_outputs(out_dir, provenance, audit, estimates)
message("Estimation complete. Outputs are in ", out_dir)
