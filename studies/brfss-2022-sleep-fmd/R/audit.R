# Schema, recode, and design audit. This file does not estimate the
# sleep-by-distress association.

frequency_check <- function(x, expected_named, label) {
  got <- vapply(names(expected_named), function(nm) {
    if (nm == "valid_1_24") return(sum(x %in% 1:24))
    if (nm == "days_1_30") return(sum(x %in% 1:30))
    if (nm == "dk_77") return(sum(x == 77, na.rm = TRUE))
    if (nm == "refused_99") return(sum(x == 99, na.rm = TRUE))
    if (nm == "none_88") return(sum(x == 88, na.rm = TRUE))
    if (nm == "blank") return(sum(is.na(x)))
    NA_integer_
  }, integer(1))
  mismatch <- got != expected_named
  if (any(mismatch)) {
    stop(
      label, " frequencies do not match the codebook.\n",
      paste(names(expected_named), "got", got, "expected", expected_named, collapse = "\n")
    )
  }
  as.list(got)
}

audit_microdata <- function(raw) {
  n <- nrow(raw)
  if (n != BRFSS_SOURCE$expected_records) {
    stop(
      "Record count is ", n, ", not the published ", BRFSS_SOURCE$expected_records,
      ". Estimation stopped."
    )
  }
  needed <- c(ANALYSIS_COLUMNS, AUDIT_ONLY_COLUMNS)
  missing <- setdiff(needed, names(raw))
  if (length(missing)) stop("Required columns absent: ", paste(missing, collapse = ", "))

  state <- assert_integer_codes(raw[["_STATE"]], "_STATE")
  psu <- assert_integer_codes(raw[["_PSU"]], "_PSU")
  ststr <- assert_integer_codes(raw[["_STSTR"]], "_STSTR")
  wt <- as_code(raw[["_LLCPWT"]])
  slept <- as_code(raw$SLEPTIM1)
  ment <- as_code(raw$MENTHLTH)
  ment14 <- as_code(raw[["_MENT14D"]])
  age <- as_code(raw[["_AGEG5YR"]])
  age80 <- as_code(raw[["_AGE80"]])
  sex <- as_code(raw$SEXVAR)
  sexcalc <- as_code(raw[["_SEX"]])
  birth <- as_code(raw$BIRTHSEX)

  if (any(!is.finite(wt)) || any(wt <= 0)) {
    stop("Every _LLCPWT must be finite and positive. Estimation stopped.")
  }
  if (any(!(slept %in% c(1:24, 77, 99) | is.na(slept)))) {
    stop("SLEPTIM1 contains codes outside 1-24, 77, 99, and blank.")
  }
  if (any(!(ment %in% c(1:30, 77, 88, 99) | is.na(ment)))) {
    stop("MENTHLTH contains codes outside 1-30, 77, 88, 99, and blank.")
  }

  slept_freq <- frequency_check(slept, CODEBOOK_SLEPTIM1, "SLEPTIM1")
  ment_freq <- frequency_check(ment, CODEBOOK_MENTHLTH, "MENTHLTH")
  sex_freq <- list(
    male = sum(sex == 1, na.rm = TRUE),
    female = sum(sex == 2, na.rm = TRUE),
    other = sum(!(sex %in% c(1, 2)) | is.na(sex))
  )
  if (sex_freq$male != CODEBOOK_SEXVAR[["male"]] || sex_freq$female != CODEBOOK_SEXVAR[["female"]] || sex_freq$other != 0) {
    stop("SEXVAR frequencies do not match the codebook, or a value other than 1 or 2 is present.")
  }

  ment14_tab <- table(factor(ment14, levels = c(1, 2, 3, 9)), useNA = "ifany")
  ment14_got <- as.integer(ment14_tab[c("1", "2", "3", "9")])
  if (!identical(ment14_got, unname(CODEBOOK_MENT14D)) || any(is.na(ment14))) {
    stop("_MENT14D frequencies do not match the codebook, or a code other than 1, 2, 3, 9 is present.")
  }
  cross <- fmd_crosswalk(ment, ment14)
  if (cross$n_disagree != 0 ||
      cross$n_derived_missing_official_observed != 0 ||
      cross$n_derived_observed_official_missing != 0) {
    stop(
      "Derived frequent distress disagrees with _MENT14D. ",
      "Disagreements: ", cross$n_disagree,
      ". Estimation stopped so the outcome rule can be amended before any contrast is fit."
    )
  }

  state_tab <- as.list(table(state))
  unexpected_geo <- sort(setdiff(unique(state), c(STATE_ALLOWLIST, unname(TERRITORY_FIPS))))
  if (length(unexpected_geo)) {
    stop("Unexpected _STATE codes: ", paste(unexpected_geo, collapse = ", "), ". They are not in the allow-list and were not the three known territories.")
  }
  geo <- state %in% STATE_ALLOWLIST
  strata_states <- tapply(state, ststr, function(x) length(unique(x)))
  if (any(strata_states > 1)) stop("At least one _STSTR maps to more than one state. Do not drop territories before building the design.")
  psu_strata <- tapply(ststr, psu, function(x) length(unique(x)))
  # Run lengths of (stratum, PSU). Avoids a strata-by-PSU matrix.
  ord <- order(ststr, psu)
  st_o <- ststr[ord]
  psu_o <- psu[ord]
  change <- c(TRUE, st_o[-1] != st_o[-length(st_o)] | psu_o[-1] != psu_o[-length(psu_o)])
  cluster_sizes <- diff(c(which(change), length(st_o) + 1L))
  psu_per <- tapply(psu, ststr, function(x) length(unique(x)))
  psu_per_geo <- tapply(psu[geo], ststr[geo], function(x) length(unique(x)))

  age14 <- sum(age == 14, na.rm = TRUE)
  age14_imputed <- sum(age == 14 & !is.na(age80), na.rm = TRUE)
  sex_disagree <- sum(sex != sexcalc, na.rm = TRUE)
  birth_differs <- sum(birth %in% c(1, 2) & birth != sex, na.rm = TRUE)

  list(
    n_records = n,
    n_columns_read = ncol(raw),
    codebook_frequencies = list(SLEPTIM1 = slept_freq, MENTHLTH = ment_freq, SEXVAR = sex_freq, `_MENT14D` = as.list(CODEBOOK_MENT14D)),
    fmd_crosswalk = cross,
    weights = list(
      n_missing = sum(!is.finite(wt)),
      n_nonpositive = sum(is.finite(wt) & wt <= 0),
      min = min(wt),
      max = max(wt),
      sum_all_areas = sum(wt),
      sum_states_dc = sum(wt[geo])
    ),
    geography = list(
      n_states_dc = sum(geo),
      n_excluded = sum(!geo),
      excluded_counts = list(
        Guam_66 = sum(state == 66),
        Puerto_Rico_72 = sum(state == 72),
        US_Virgin_Islands_78 = sum(state == 78)
      ),
      state_counts = state_tab
    ),
    design = list(
      n_strata_all = length(unique(ststr)),
      n_strata_states_dc = length(unique(ststr[geo])),
      strata_spanning_more_than_one_state = sum(strata_states > 1),
      n_psu_ids = length(unique(psu)),
      psu_ids_in_more_than_one_stratum = sum(psu_strata > 1),
      max_cluster_size = max(cluster_sizes),
      n_clusters_larger_than_one = sum(cluster_sizes > 1),
      singleton_strata_all = sum(psu_per == 1),
      singleton_strata_states_dc = sum(psu_per_geo == 1),
      min_psus_per_stratum_states_dc = min(psu_per_geo)
    ),
    age_sex_audit = list(
      n_age_group_missing_code_14 = age14,
      n_code_14_with_nonmissing_AGE80 = age14_imputed,
      AGE80_min = min(age80),
      AGE80_max = max(age80),
      AGE80_missing = sum(is.na(age80)),
      n_SEXVAR_disagree_SEX = sex_disagree,
      n_BIRTHSEX_answered = sum(birth %in% c(1, 2)),
      n_BIRTHSEX_differs_from_SEXVAR = birth_differs,
      note = "Estimation uses _AGEG5YR and SEXVAR. _AGE80 and _SEX are not analysis variables."
    ),
    sleep_item_in_states_dc = list(
      valid_1_24 = sum(slept[geo] %in% 1:24),
      dk_77 = sum(slept[geo] == 77, na.rm = TRUE),
      refused_99 = sum(slept[geo] == 99, na.rm = TRUE),
      blank = sum(is.na(slept[geo])),
      hours_below_3 = sum(slept[geo] %in% 1:2),
      hours_above_12 = sum(slept[geo] %in% 13:24)
    )
  )
}

# Columns handed to estimation. Audit-only variables are removed here.
slim_for_analysis <- function(raw) {
  keep <- raw[ANALYSIS_COLUMNS]
  for (nm in c("SLEPTIM1", "MENTHLTH", "_MENT14D", "_AGEG5YR", "SEXVAR", "_STATE", "_STSTR", "_PSU")) {
    keep[[nm]] <- as_code(keep[[nm]])
  }
  keep[["_LLCPWT"]] <- as_code(keep[["_LLCPWT"]])
  keep
}
