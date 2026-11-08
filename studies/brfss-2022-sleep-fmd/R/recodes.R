# Recodes for the BRFSS 2022 sleep and frequent-distress study.
# Rules are the SAP. Do not infer categories from value labels at runtime.

source_study <- function(file) {
  root <- Sys.getenv("BRFSS_STUDY_ROOT", unset = "")
  if (!nzchar(root)) {
    # This file lives in <study>/R/.
    root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."), mustWork = FALSE)
  }
  source(file.path(root, "R", file), local = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Prefer an explicit root set by run.R or the test runner.
study_file <- function(name) {
  root <- Sys.getenv("BRFSS_STUDY_ROOT", unset = NA_character_)
  if (is.na(root) || !nzchar(root)) {
    stop("Set BRFSS_STUDY_ROOT to the study directory before sourcing recodes.")
  }
  file.path(root, name)
}

if (!exists("STATE_ALLOWLIST", inherits = TRUE)) {
  root_try <- Sys.getenv("BRFSS_STUDY_ROOT", unset = "")
  if (nzchar(root_try)) {
    source(file.path(root_try, "R", "constants.R"))
  }
}

as_code <- function(x) {
  # haven labelled columns become plain numbers. Tagged missing becomes NA.
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    x <- as.numeric(x)
  }
  as.numeric(x)
}

assert_integer_codes <- function(x, name) {
  z <- as_code(x)
  bad <- !is.na(z) & abs(z - round(z)) > 1e-6
  if (any(bad)) {
    stop(name, " has ", sum(bad), " non-integer values.")
  }
  out <- rep(NA_integer_, length(z))
  ok <- !is.na(z)
  out[ok] <- as.integer(round(z[ok]))
  out
}

in_states_dc <- function(state) {
  as_code(state) %in% STATE_ALLOWLIST
}

# Usual hours in a 24-hour period.
# short: 1-6 (<7). reference: 7-9. long: 10-24.
# 77, 99, and blank are missing. Anything else is missing and is counted upstream.
recode_sleep_group <- function(hours, reference = c("7to9", "7to8")) {
  reference <- match.arg(reference)
  h <- as_code(hours)
  out <- rep(NA_character_, length(h))
  out[h %in% 1:6] <- "short"
  if (reference == "7to9") {
    out[h %in% 7:9] <- "reference"
    out[h %in% 10:24] <- "long"
  } else {
    out[h %in% 7:8] <- "reference"
    out[h == 9] <- "hour9"
    out[h %in% 10:24] <- "long"
  }
  out
}

# TRUE when a valid hour is outside the prespecified plausible usual range.
# Hours that are already missing (77, 99, blank) are not "extreme"; they stay missing.
flag_implausible_sleep <- function(hours) {
  h <- as_code(hours)
  !is.na(h) & h %in% 1:24 & (h < SLEEP_PLAUSIBLE_MIN | h > SLEEP_PLAUSIBLE_MAX)
}

# Frequent mental distress: 14 or more days of poor mental health in the past 30.
# MENTHLTH 88 means zero days. 77, 99, and blank are missing.
# Returns 1, 0, or NA. Does not use _MENT14D.
recode_fmd <- function(menthlth) {
  m <- as_code(menthlth)
  out <- rep(NA_real_, length(m))
  out[m %in% 14:30] <- 1
  out[m == 88 | m %in% 1:13] <- 0
  out
}

# _MENT14D: 3 = 14 or more days, 1 = zero, 2 = 1 to 13, 9 = missing.
# This is only the crosswalk. The analysis outcome is recode_fmd().
recode_ment14d_fmd <- function(ment14d) {
  m <- as_code(ment14d)
  out <- rep(NA_real_, length(m))
  out[m == 3] <- 1
  out[m %in% c(1, 2)] <- 0
  out
}

fmd_crosswalk <- function(menthlth, ment14d) {
  derived <- recode_fmd(menthlth)
  official <- recode_ment14d_fmd(ment14d)
  both <- !is.na(derived) & !is.na(official)
  derived_na <- is.na(derived)
  official_na <- is.na(official)
  list(
    n = length(derived),
    n_both_observed = sum(both),
    n_disagree = sum(both & derived != official),
    n_derived_missing_official_observed = sum(derived_na & !official_na),
    n_derived_observed_official_missing = sum(!derived_na & official_na)
  )
}

# _AGEG5YR 1-13 are age groups. Code 14 is missing. Never fill from _AGE80.
recode_age_group <- function(ageg5yr) {
  a <- as_code(ageg5yr)
  out <- rep(NA_integer_, length(a))
  ok <- a %in% 1:13
  out[ok] <- as.integer(a[ok])
  out
}

# SEXVAR 1 male, 2 female. No other code is recoded from _SEX or BIRTHSEX.
recode_sex <- function(sexvar) {
  s <- as_code(sexvar)
  out <- rep(NA_integer_, length(s))
  ok <- s %in% c(1, 2)
  out[ok] <- as.integer(s[ok])
  out
}

prepare_frame <- function(raw, reference = c("7to9", "7to8"), drop_implausible = FALSE) {
  reference <- match.arg(reference)
  forbidden <- intersect(AUDIT_ONLY_COLUMNS, names(raw))
  if (length(forbidden)) {
    stop(
      "prepare_frame received audit-only columns (",
      paste(forbidden, collapse = ", "),
      "). Drop them before estimation."
    )
  }
  required <- c("SLEPTIM1", "MENTHLTH", "_AGEG5YR", "SEXVAR", "_STATE", "_LLCPWT", "_STSTR", "_PSU")
  missing <- setdiff(required, names(raw))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

  hours <- as_code(raw$SLEPTIM1)
  if (drop_implausible) {
    hours[flag_implausible_sleep(hours)] <- NA_real_
  }
  data.frame(
    state = assert_integer_codes(raw[["_STATE"]], "_STATE"),
    psu = assert_integer_codes(raw[["_PSU"]], "_PSU"),
    ststr = assert_integer_codes(raw[["_STSTR"]], "_STSTR"),
    wt = as_code(raw[["_LLCPWT"]]),
    hours = hours,
    sleep_group = recode_sleep_group(hours, reference = reference),
    fmd = recode_fmd(raw$MENTHLTH),
    age = recode_age_group(raw[["_AGEG5YR"]]),
    sex = recode_sex(raw$SEXVAR),
    in_geo = in_states_dc(raw[["_STATE"]]),
    stringsAsFactors = FALSE
  )
}
