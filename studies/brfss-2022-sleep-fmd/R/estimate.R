# Association estimates. Called only after audit_microdata() has passed.

ESTIMATE_TIMINGS <- list()

timed <- function(label, expr) {
  message("Start: ", label)
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  message(sprintf("Done: %s (%.1f seconds)", label, elapsed))
  flush(stderr())
  ESTIMATE_TIMINGS[[label]] <<- elapsed
  value
}

checkpoint_result <- function(name, value) {
  root <- Sys.getenv("BRFSS_STUDY_ROOT", unset = "")
  if (!nzchar(root)) return(value)
  dir.create(file.path(root, "data", "intermediate"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, file.path(root, "data", "intermediate", paste0("ckpt-", name, ".rds")))
  value
}

sample_flow <- function(prep) {
  wt <- prep$wt
  geo <- prep$in_geo
  valid_sleep <- geo & !is.na(prep$sleep_group)
  contrast <- valid_sleep & prep$sleep_group %in% c("short", "reference")
  primary <- contrast & !is.na(prep$fmd)
  secondary <- primary & !is.na(prep$age) & !is.na(prep$sex)
  step <- function(name, keep, note) {
    data.frame(
      step = name,
      unweighted_n = sum(keep),
      weighted_n = sum(wt[keep]),
      note = note,
      stringsAsFactors = FALSE
    )
  }
  rows <- rbind(
    step("1. All LLCP2022 records", rep(TRUE, nrow(prep)), "Published file, all reporting areas."),
    step("2. 50 states and DC", geo, "Allow-list of state FIPS codes. DC is included. Guam, Puerto Rico, and the US Virgin Islands are removed."),
    step("3. Valid usual sleep, 1 to 24 hours", valid_sleep, "SLEPTIM1 codes 77, 99, and blank are missing. Long sleep stays in this row."),
    step("4. Contrast groups, under 7 hours or 7 to 9 hours", contrast, "Long sleep, 10 to 24 hours, is not in the primary contrast."),
    step("5. Observed mental-health days", primary, "Primary analytic sample. MENTHLTH 77, 99, and blank are missing."),
    step("6. Observed age group and sex", secondary, "Secondary sample. _AGEG5YR code 14 is missing. SEXVAR must be 1 or 2.")
  )
  rows$unweighted_excluded_from_previous <- c(NA, -diff(rows$unweighted_n))
  rows
}

missingness_table <- function(prep) {
  geo <- prep[prep$in_geo, , drop = FALSE]
  wt <- geo$wt
  is_group <- function(level) !is.na(geo$sleep_group) & geo$sleep_group == level
  row <- function(item, flag, denominator, population) {
    flag <- flag & !is.na(flag)
    denominator <- denominator & !is.na(denominator)
    data.frame(
      item = item,
      population = population,
      unweighted_n = sum(flag),
      unweighted_denominator = sum(denominator),
      weighted_share_of_population = sum(wt[flag]) / sum(wt[denominator]),
      stringsAsFactors = FALSE
    )
  }
  contrast <- is_group("short") | is_group("reference")
  primary <- contrast & !is.na(geo$fmd)
  rbind(
    row("Sleep missing (77, 99, or blank)", is.na(geo$sleep_group), rep(TRUE, nrow(geo)), "50 states and DC"),
    row("Sleep reported below 3 hours", geo$hours %in% 1:2, rep(TRUE, nrow(geo)), "50 states and DC"),
    row("Sleep reported above 12 hours", geo$hours %in% 13:24, rep(TRUE, nrow(geo)), "50 states and DC"),
    row("Outcome missing among short sleep", is_group("short") & is.na(geo$fmd), is_group("short"), "Valid short sleep"),
    row("Outcome missing among 7 to 9 hours", is_group("reference") & is.na(geo$fmd), is_group("reference"), "Valid 7 to 9 hour sleep"),
    row("Outcome missing among long sleep", is_group("long") & is.na(geo$fmd), is_group("long"), "Valid long sleep"),
    row("Age group missing", primary & is.na(geo$age), primary, "Primary contrast sample"),
    row("Sex missing", primary & is.na(geo$sex), primary, "Primary contrast sample")
  )
}

fit_secondary <- function(prep) {
  d <- prep[
    prep$in_geo %in% TRUE &
      prep$sleep_group %in% c("short", "reference") &
      !is.na(prep$fmd) & !is.na(prep$age) & !is.na(prep$sex),
    ,
    drop = FALSE
  ]
  d$short <- as.integer(d$sleep_group == "short")
  d$age_f <- factor(d$age, levels = 1:13)
  d$sex_f <- factor(d$sex, levels = c(1, 2))
  if (any(table(d$age_f) == 0) || any(table(d$sex_f) == 0)) {
    stop("A prespecified age or sex level is empty. The margin model was not changed to drop it.")
  }
  options(survey.lonely.psu = "adjust")
  des <- survey::svydesign(
    ids = ~psu, strata = ~ststr, weights = ~wt, data = d, nest = TRUE
  )
  n_single <- sum(tapply(d$psu, d$ststr, function(x) length(unique(x))) == 1)
  design_df <- survey::degf(des)
  mod <- survey::svyglm(
    fmd ~ short + age_f + sex_f,
    design = des,
    family = quasibinomial()
  )
  mar <- predictive_margin_difference(mod, des$variables, weights(des), "short")
  or_ci <- exp(stats::confint(mod)["short", ])
  list(
    n_unweighted = nrow(d),
    weighted_n = sum(d$wt),
    singleton_strata = n_single,
    design_df = design_df,
    margin_variance = "delta method on the survey coefficient covariance, conditional on the observed age and sex distribution as a fixed standard population",
    margins = mar,
    odds_ratio = list(
      estimate = unname(exp(stats::coef(mod)[["short"]])),
      ci_low = unname(or_ci[[1]]),
      ci_high = unname(or_ci[[2]]),
      role = "Coefficient from the working logistic model. Not the estimand. Not a prevalence ratio."
    ),
    age_coefficients_log_odds = as.list(stats::coef(mod))
  )
}

drop_model_pieces <- function(secondary) {
  # age_coefficients are small and auditable. The glm object is not saved.
  secondary
}

estimate_all <- function(prep) {
  ESTIMATE_TIMINGS <<- list()
  if (any(c("_AGE80", "_SEX", "BIRTHSEX") %in% names(prep))) {
    stop("Audit-only columns reached estimation.")
  }
  flow <- sample_flow(prep)
  miss <- missingness_table(prep)

  des <- timed("design, nested _PSU, lonely.psu=adjust", {
    make_geo_design(prep, lonely = "adjust", ids = "psu")
  })
  primary <- checkpoint_result("primary", timed("primary prevalence difference", {
    prevalence_contrast(des, "sleep_group", "fmd", extra_levels = "long")
  }))

  options(survey.lonely.psu = "average")
  lonely_average <- checkpoint_result("lonely-average", timed("singleton sensitivity, average", {
    prevalence_contrast(des, "sleep_group", "fmd", extra_levels = character())
  }))
  options(survey.lonely.psu = "remove")
  lonely_remove <- checkpoint_result("lonely-remove", timed("singleton sensitivity, remove", {
    prevalence_contrast(des, "sleep_group", "fmd", extra_levels = character())
  }))
  options(survey.lonely.psu = "adjust")

  independent <- timed("cross-check, ids=~1", {
    des_i <- make_geo_design(prep, lonely = "adjust", ids = "independent")
    prevalence_contrast(des_i, "sleep_group", "fmd", extra_levels = character())
  })

  subset_check <- timed("cross-check, subset then the same contrast", {
    # subset() is the base generic. survey registers subset.survey.design.
    sub <- subset(
      des,
      !is.na(sleep_group) & sleep_group %in% c("short", "reference") & !is.na(fmd)
    )
    est <- prevalence_contrast(sub, "sleep_group", "fmd", extra_levels = character())
    n_single <- sum(tapply(sub$variables$psu, sub$variables$ststr, function(x) length(unique(x))) == 1)
    list(
      estimate = est,
      n_rows = nrow(sub$variables),
      n_parent = nrow(des$variables),
      singleton_strata = n_single
    )
  })

  qc_diff <- abs(primary$difference$estimate - subset_check$estimate$difference$estimate)
  qc_se <- abs(primary$difference$se - subset_check$estimate$difference$se) / primary$difference$se
  if (qc_diff > 0.0001) {
    stop(sprintf("Subset cross-check differs by %.6f on the probability scale (gate 0.0001).", qc_diff))
  }
  if (qc_se > 0.05) {
    stop(sprintf("Subset cross-check SE differs by %.1f%% relative (gate 5%%).", 100 * qc_se))
  }

  bounds <- timed("outcome-missingness bounds", {
    outcome_missing_bounds(des, "sleep_group", "fmd")
  })

  extreme_prep <- prep
  extreme_hours <- extreme_prep$hours
  extreme_hours[flag_implausible_sleep(extreme_hours)] <- NA_real_
  extreme_prep$hours <- extreme_hours
  extreme_prep$sleep_group <- recode_sleep_group(extreme_hours, reference = "7to9")
  extreme <- timed("sensitivity, exclude hours below 3 or above 12", {
    des_e <- make_geo_design(extreme_prep, lonely = "adjust", ids = "psu")
    prevalence_contrast(des_e, "sleep_group", "fmd", extra_levels = character())
  })

  ref78_prep <- prep
  ref78_prep$sleep_group <- recode_sleep_group(ref78_prep$hours, reference = "7to8")
  ref78 <- timed("sensitivity, 7 to 8 hour reference", {
    des_r <- make_geo_design(ref78_prep, lonely = "adjust", ids = "psu")
    prevalence_contrast(des_r, "sleep_group", "fmd", extra_levels = character())
  })

  secondary <- timed("secondary age and sex margins", fit_secondary(prep))
  secondary_extreme <- timed("secondary margins, extreme-sleep exclusion", fit_secondary(extreme_prep))
  secondary_ref78 <- timed("secondary margins, 7 to 8 hour reference", fit_secondary(ref78_prep))

  list(
    flow = flow,
    missingness = miss,
    primary = primary,
    lonely = list(adjust = primary$difference, average = lonely_average$difference, remove = lonely_remove$difference),
    independent_ids = independent$difference,
    subset_check = list(
      difference = subset_check$estimate$difference,
      n_rows = subset_check$n_rows,
      n_parent = subset_check$n_parent,
      singleton_strata = subset_check$singleton_strata,
      absolute_difference_gap = qc_diff,
      relative_se_gap = qc_se
    ),
    bounds = bounds,
    extreme = extreme$difference,
    ref78 = ref78$difference,
    secondary = secondary,
    secondary_extreme = secondary_extreme,
    secondary_ref78 = secondary_ref78,
    df = primary$df,
    timings_sec = ESTIMATE_TIMINGS
  )
}
