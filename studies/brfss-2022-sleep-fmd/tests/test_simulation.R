# SIMULATION. Not a BRFSS result.
# Repeated stratified samples from a finite population with a known prevalence difference.
# The test checks that the design-based interval from prevalence_contrast covers that
# finite-population difference at roughly the nominal rate, and that the estimator is
# approximately unbiased. A misspecified variance (independence, ignored strata) fails this.

args_root <- Sys.getenv("BRFSS_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "estimands.R"))) {
    Sys.setenv(BRFSS_STUDY_ROOT = cand)
  } else {
    Sys.setenv(BRFSS_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("BRFSS_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "estimands.R"))
source(file.path(root, "tests", "helper.R"))
library(survey)

message("SIMULATION coverage check. These numbers are not BRFSS estimates.")

set.seed(202211)
n_strata <- 20
n_pop <- 200
pop <- data.frame(
  ststr = rep(seq_len(n_strata), each = n_pop),
  id = seq_len(n_strata * n_pop)
)
# Short sleep is more common in some strata. Outcome is higher for short sleep.
pop$u <- runif(nrow(pop))
pop$short <- as.integer(pop$u < 0.35)
pop$fmd <- as.integer(runif(nrow(pop)) < (0.12 + 0.10 * pop$short + 0.04 * (pop$ststr %% 5 == 0)))
pop$sleep_group <- ifelse(pop$short == 1, "short", "reference")

truth <- mean(pop$fmd[pop$sleep_group == "short"]) - mean(pop$fmd[pop$sleep_group == "reference"])

n_rep <- 80
n_draw <- 40
cover <- logical(n_rep)
ests <- numeric(n_rep)
options(survey.lonely.psu = "adjust")
for (i in seq_len(n_rep)) {
  take <- unlist(lapply(split(seq_len(nrow(pop)), pop$ststr), function(idx) sample(idx, n_draw)))
  samp <- pop[take, ]
  samp$wt <- n_pop / n_draw
  samp$psu <- seq_len(nrow(samp))
  samp$in_geo <- TRUE
  des <- svydesign(ids = ~psu, strata = ~ststr, weights = ~wt, data = samp, nest = TRUE)
  fit <- prevalence_contrast(des, "sleep_group", "fmd", extra_levels = character())
  ests[i] <- fit$difference$estimate
  cover[i] <- fit$difference$ci_low <= truth && fit$difference$ci_high >= truth
}

bias <- mean(ests) - truth
rate <- mean(cover)
message(sprintf("SIMULATION truth %.4f, mean estimate %.4f, bias %.4f, coverage %.3f over %d reps",
                truth, mean(ests), bias, rate, n_rep))

expect_true(abs(bias) < 0.02, "SIMULATION mean estimate is within 2 percentage points of the finite-population difference")
expect_true(rate >= 0.85 && rate <= 1, "SIMULATION 95% CI coverage is at least 85% in 80 replicates")

out_dir <- file.path(root, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(
    list(
      kind = "simulation_not_brfss",
      purpose = "Finite-population coverage of the primary prevalence-difference function.",
      truth_difference = truth,
      mean_estimate = mean(ests),
      bias = bias,
      coverage = rate,
      n_replicates = n_rep,
      nominal_level = 0.95
    ),
    file.path(out_dir, "simulation-check.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 8
  )
}

finish_tests()
