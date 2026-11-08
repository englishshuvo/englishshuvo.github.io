# Recode tests. These use the codebook rules, not the BRFSS microdata.
if (!nzchar(Sys.getenv("BRFSS_STUDY_ROOT", unset = ""))) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "constants.R"))) {
    Sys.setenv(BRFSS_STUDY_ROOT = cand)
  } else {
    Sys.setenv(BRFSS_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("BRFSS_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "recodes.R"))
source(file.path(root, "tests", "helper.R"))

message("Recode tests. Not BRFSS estimates.")

expect_equal(length(STATE_ALLOWLIST), 51L, "allow-list has 51 areas")
expect_true(11 %in% STATE_ALLOWLIST, "DC is included")
expect_true(!any(c(66, 72, 78) %in% STATE_ALLOWLIST), "territories are not in the allow-list")
expect_true(all(in_states_dc(c(1, 11, 56))), "states and DC pass the geography check")
expect_true(!any(in_states_dc(c(66, 72, 78, NA))), "territories and missing FIPS fail the geography check")

hours <- c(1, 6, 7, 8, 9, 10, 24, 77, 99, NA, 0, 25)
g <- recode_sleep_group(hours)
expect_equal(
  g,
  c("short", "short", "reference", "reference", "reference", "long", "long", NA, NA, NA, NA, NA),
  "7 to 9 hour reference groups"
)
g78 <- recode_sleep_group(hours, reference = "7to8")
expect_equal(
  g78,
  c("short", "short", "reference", "reference", "hour9", "long", "long", NA, NA, NA, NA, NA),
  "7 to 8 hour reference moves hour 9 out of the reference"
)

expect_equal(
  flag_implausible_sleep(c(1, 2, 3, 12, 13, 24, 77, NA)),
  c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
  "implausible flag is only for valid hours below 3 or above 12"
)

ment <- c(88, 1, 13, 14, 30, 77, 99, NA, 0, 31)
expect_equal(
  recode_fmd(ment),
  c(0, 0, 0, 1, 1, NA, NA, NA, NA, NA),
  "frequent distress is 14 to 30 days, and 88 is zero"
)
expect_equal(
  recode_ment14d_fmd(c(1, 2, 3, 9, NA, 4)),
  c(0, 0, 1, NA, NA, NA),
  "_MENT14D crosswalk uses 3 as yes and 1 or 2 as no"
)

# A hand-built file in which the CDC SAS rule and our rule agree, including the else branch.
ment_ok <- c(88, 5, 14, 77, 99, NA, 30, 1)
ment14_ok <- c(1, 2, 3, 9, 9, 9, 3, 2)
cw <- fmd_crosswalk(ment_ok, ment14_ok)
expect_equal(cw$n_disagree, 0, "crosswalk agrees when _MENT14D follows the calculated-variable SAS")
expect_equal(cw$n_derived_missing_official_observed, 0, "no one-sided missingness when codes match")

# Disagreement is detected rather than papered over.
cw_bad <- fmd_crosswalk(c(14), c(1))
expect_equal(cw_bad$n_disagree, 1, "a mismatched _MENT14D is counted")

expect_equal(recode_age_group(c(1, 13, 14, NA, 0, 15)), c(1L, 13L, NA, NA, NA, NA), "age code 14 is missing")
expect_equal(recode_sex(c(1, 2, 7, 9, NA)), c(1L, 2L, NA, NA, NA), "sex keeps only SEXVAR 1 and 2")

# prepare_frame refuses the imputed age and the calculated sex variable.
raw_bad <- data.frame(
  SLEPTIM1 = 8, MENTHLTH = 88, `_AGEG5YR` = 1, SEXVAR = 1,
  `_STATE` = 11, `_LLCPWT` = 1, `_STSTR` = 1, `_PSU` = 1,
  `_AGE80` = 20, check.names = FALSE
)
expect_true(
  inherits(try(prepare_frame(raw_bad), silent = TRUE), "try-error"),
  "prepare_frame rejects _AGE80"
)

raw_ok <- data.frame(
  SLEPTIM1 = c(5, 8, 12, 77),
  MENTHLTH = c(20, 88, 2, 14),
  `_AGEG5YR` = c(3, 14, 13, 1),
  SEXVAR = c(1, 2, 1, 2),
  `_STATE` = c(1, 66, 11, 1),
  `_LLCPWT` = c(10, 10, 5, 1),
  `_STSTR` = c(10, 66, 11, 10),
  `_PSU` = c(1, 1, 2, 3),
  check.names = FALSE
)
prep <- prepare_frame(raw_ok)
expect_equal(prep$sleep_group, c("short", "reference", "long", NA), "prepare_frame sleep groups")
expect_equal(prep$fmd, c(1, 0, 0, 1), "prepare_frame outcome, including a row with missing sleep")
expect_equal(prep$age, c(3L, NA, 13L, 1L), "prepare_frame age")
expect_equal(prep$in_geo, c(TRUE, FALSE, TRUE, TRUE), "prepare_frame geography")

prep_x <- prepare_frame(raw_ok, drop_implausible = TRUE)
# 5 hours is plausible, so the short row stays. Add an extreme hour.
raw_ok$SLEPTIM1[1] <- 2
prep_x <- prepare_frame(raw_ok, drop_implausible = TRUE)
expect_true(is.na(prep_x$sleep_group[1]), "extreme hour is dropped only when the sensitivity asks for it")
prep_keep <- prepare_frame(raw_ok, drop_implausible = FALSE)
expect_equal(prep_keep$sleep_group[1], "short", "primary recode keeps a 2-hour report in the short group")

finish_tests()
