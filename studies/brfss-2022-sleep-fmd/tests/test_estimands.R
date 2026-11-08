# Estimand tests on fabricated surveys. None of these numbers are BRFSS results.
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

message("Estimand tests on fabricated samples. Not BRFSS estimates.")

# Two strata, repeated PSU ids, equal weights, known counts.
# Stratum 1: short outcomes 1,1,0 and reference outcomes 0,0.
# Stratum 2: short outcome 1 and reference outcomes 0,1.
# PSU id 1 is used in both strata, so nest=FALSE must fail.
fab <- data.frame(
  ststr = c(1, 1, 1, 1, 1, 2, 2, 2),
  psu = c(1, 2, 3, 4, 5, 1, 2, 3),
  wt = c(2, 2, 2, 2, 2, 4, 4, 4),
  sleep_group = c("short", "short", "short", "reference", "reference", "short", "reference", "reference"),
  fmd = c(1, 1, 0, 0, 0, 1, 0, 1),
  in_geo = TRUE
)
options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~psu, strata = ~ststr, weights = ~wt, data = fab, nest = TRUE)
nest_err <- try(
  svydesign(ids = ~psu, strata = ~ststr, weights = ~wt, data = fab, nest = FALSE),
  silent = TRUE
)
expect_true(inherits(nest_err, "try-error"), "repeated PSU ids without nest=TRUE are rejected")

# Weighted counts:
# short yes weight = 2+2+4 = 8, short total weight = 2+2+2+4 = 10, prev = 0.8
# reference yes weight = 4, reference total = 2+2+4+4 = 12, prev = 1/3
# difference = 0.8 - 1/3 = 0.4666...
got <- prevalence_contrast(des, "sleep_group", "fmd", extra_levels = character())
expect_equal(got$levels$short$estimate, 0.8, "short prevalence", tol = 1e-8)
expect_equal(got$levels$reference$estimate, 1 / 3, "reference prevalence", tol = 1e-8)
expect_equal(got$difference$estimate, 0.8 - 1 / 3, "prevalence difference", tol = 1e-8)
expect_equal(got$prevalence_ratio$estimate, 0.8 / (1 / 3), "prevalence ratio", tol = 1e-8)

# Covariance is not zero, so the difference variance is not the sum of the marginal variances.
v_s <- got$levels$short$se^2
v_r <- got$levels$reference$se^2
v_d <- got$difference$se^2
expect_true(abs(v_d - (v_s + v_r)) > 1e-8, "difference variance uses the covariance of the two prevalences")
expect_true(got$difference$ci_low < got$difference$estimate && got$difference$ci_high > got$difference$estimate,
            "difference CI covers the point estimate")

# Manski bounds. Add one missing outcome in each group.
fab_m <- fab
fab_m$fmd[1] <- NA # a short case, weight 2
fab_m$fmd[4] <- NA # a reference case, weight 2
des_m <- svydesign(ids = ~psu, strata = ~ststr, weights = ~wt, data = fab_m, nest = TRUE)
bounds <- outcome_missing_bounds(des_m, "sleep_group", "fmd")
# Short: weights of observed+missing = 10. Observed yes weight was 8, of which weight 2 is now missing.
# Remaining observed yes = 6, observed no = 2, missing = 2. Total 10.
# L = 6/10 = 0.6, U = 8/10 = 0.8
# Reference: total 12. Original yes weight 4, we blanked a no (weight 2).
# Observed yes = 4, observed no = 2+4 = 6, missing = 2. Total 12.
# L = 4/12, U = 6/12
expect_equal(bounds$level_a$lower$estimate, 0.6, "short lower bound", tol = 1e-8)
expect_equal(bounds$level_a$upper$estimate, 0.8, "short upper bound", tol = 1e-8)
expect_equal(bounds$level_b$lower$estimate, 4 / 12, "reference lower bound", tol = 1e-8)
expect_equal(bounds$level_b$upper$estimate, 6 / 12, "reference upper bound", tol = 1e-8)
expect_equal(
  bounds$difference$lower$estimate,
  0.6 - 6 / 12,
  "lower difference uses the assignment that shrinks the contrast",
  tol = 1e-8
)
expect_equal(
  bounds$difference$upper$estimate,
  0.8 - 4 / 12,
  "upper difference uses the assignment that widens the contrast",
  tol = 1e-8
)
# Width of a group bound equals the weighted share missing.
expect_equal(
  bounds$level_a$upper$estimate - bounds$level_a$lower$estimate,
  2 / 10,
  "bound width equals the missing weight share",
  tol = 1e-8
)

# No missing outcomes: bounds collapse to the complete-case prevalence.
bounds_full <- outcome_missing_bounds(des, "sleep_group", "fmd")
expect_equal(bounds_full$level_a$lower$estimate, 0.8, "lower bound equals the prevalence when nothing is missing", tol = 1e-8)
expect_equal(bounds_full$level_a$upper$estimate, 0.8, "upper bound equals the prevalence when nothing is missing", tol = 1e-8)

# Predictive margins.
# Saturated model (exposure only): margins match the group means, and the SE matches.
# The column has to live on the design, which holds its own copy of the data.
des$variables$short <- as.integer(des$variables$sleep_group == "short")
fit <- svyglm(fmd ~ short, design = des, family = quasibinomial())
mar <- predictive_margin_difference(fit, des$variables, weights(des), exposure = "short")
expect_equal(mar$exposed$estimate, 0.8, "margin at short sleep matches the group mean", tol = 1e-6)
expect_equal(mar$unexposed$estimate, 1 / 3, "margin at reference sleep matches the group mean", tol = 1e-6)
expect_equal(mar$difference$estimate, 0.8 - 1 / 3, "saturated margin difference matches the crude difference", tol = 1e-6)
expect_equal(mar$difference$se, got$difference$se, "saturated margin SE matches the design-based difference SE", tol = 1e-5)
expect_true(
  abs(mar$difference$se - mar$se_difference_if_independent) > 1e-8,
  "margin difference SE is not the independent sum of the two margin SEs"
)

# Confounding fixture: within each age the outcome rate does not depend on sleep,
# but short sleep is more common at the age with the higher outcome rate.
# The crude difference moves. The age-standardized difference stays near zero.
n <- 400
conf <- data.frame(
  ststr = rep(1:8, each = n / 8),
  psu = seq_len(n),
  age = rep(rep(1:2, each = 25), 8)
)
conf$wt <- ifelse(conf$age == 1, 5, 1)
conf$short <- 0L
conf$fmd <- 0
for (a in c(1, 2)) {
  idx <- which(conf$age == a)
  # Three quarters short at age 1, one quarter short at age 2.
  n_short <- if (a == 1) floor(0.75 * length(idx)) else floor(0.25 * length(idx))
  conf$short[idx[seq_len(n_short)]] <- 1L
  rate <- if (a == 1) 0.5 else 0.2
  for (s in c(0L, 1L)) {
    sub <- which(conf$age == a & conf$short == s)
    n1 <- round(rate * length(sub))
    conf$fmd[sub] <- 0
    if (n1 > 0) conf$fmd[sub[seq_len(n1)]] <- 1
  }
}
conf$age_f <- factor(conf$age)
conf$in_geo <- TRUE
conf$sleep_group <- ifelse(conf$short == 1, "short", "reference")
des_c <- svydesign(ids = ~psu, strata = ~ststr, weights = ~wt, data = conf, nest = TRUE)
crude <- prevalence_contrast(des_c, "sleep_group", "fmd", extra_levels = character())
fit_c <- svyglm(fmd ~ short + age_f, design = des_c, family = quasibinomial())
mar_c <- predictive_margin_difference(fit_c, des_c$variables, weights(des_c), "short")
d_short <- des_c$variables
d_short$short <- 1
manual_short <- sum(weights(des_c) * predict(fit_c, newdata = d_short, type = "response")) / sum(weights(des_c))
expect_equal(mar_c$exposed$estimate, as.numeric(manual_short), "margin matches the weighted mean of fitted probabilities", tol = 1e-8)
expect_true(abs(crude$difference$estimate) > 0.05, "crude difference is away from zero when age confounds")
expect_true(abs(mar_c$difference$estimate) < 0.02, "age-standardized difference stays near zero when sleep has no residual association")
expect_true(abs(crude$difference$estimate) > abs(mar_c$difference$estimate) + 0.03,
            "standardizing for age moves the difference relative to the crude contrast")

# Weights change the margin. Two rows, same covariates, different weights and outcomes.
tiny <- data.frame(short = c(0, 0), fmd = c(0, 1))
fit_w <- glm(fmd ~ 1, data = tiny, family = quasibinomial())
# Intercept-only MLE is the unweighted mean, 0.5. Margins must follow the supplied weights.
heavy <- predictive_margin_difference(fit_w, tiny, weights = c(1, 9), exposure = "short")
# Exposure coefficient is absent, so setting short to 0 or 1 does not change the linear predictor.
# Both margins equal the weighted mean of a constant prediction. The constant prediction is plogis(intercept).
expect_equal(heavy$exposed$estimate, heavy$unexposed$estimate, "a model without exposure has equal margins", tol = 1e-8)
expect_equal(
  heavy$exposed$estimate,
  plogis(coef(fit_w)[[1]]),
  "margin of an intercept-only model is the fitted probability",
  tol = 1e-8
)

# Lonely-PSU option is read at estimation time.
lone <- data.frame(
  ststr = c(1, 2, 2, 2),
  psu = c(1, 1, 2, 3),
  wt = 1,
  y = c(1, 0, 0, 1)
)
options(survey.lonely.psu = "adjust")
des_l <- svydesign(ids = ~psu, strata = ~ststr, weights = ~wt, data = lone, nest = TRUE)
se_adjust <- as.numeric(SE(svymean(~y, des_l)))
options(survey.lonely.psu = "remove")
se_remove <- as.numeric(SE(svymean(~y, des_l)))
options(survey.lonely.psu = "fail")
se_fail <- try(svymean(~y, des_l), silent = TRUE)
expect_true(abs(se_adjust - se_remove) > 1e-8, "adjust and remove give different SEs when a stratum has one PSU")
expect_true(inherits(se_fail, "try-error"), "the default fail option errors on a singleton stratum")
options(survey.lonely.psu = "adjust")

finish_tests()
