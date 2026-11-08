# Design-based estimands. These functions do not read the BRFSS file.

ensure_survey <- function() {
  if (!requireNamespace("survey", quietly = TRUE)) stop("Package 'survey' is required.")
}

# Ratio estimator on the full design.
# `in_domain` is a named list of logical vectors, one per group, aligned to design rows.
# People outside a group contribute 0 to that group's numerator and denominator.
# `value` is the 0/1 (or bounded) outcome contribution; rows with NA value and
# in-domain TRUE are the caller's responsibility. This function treats NA value
# as "not in the denominator" only when `drop_na_value` is TRUE.
domain_ratios <- function(design, in_domain, value, drop_na_value = TRUE) {
  ensure_survey()
  if (!length(in_domain)) stop("in_domain is empty")
  n <- nrow(design$variables)
  value <- as.numeric(value)
  if (length(value) != n) stop("value length does not match the design")
  pieces <- character()
  vars <- design$variables
  for (nm in names(in_domain)) {
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", nm)) stop("Bad domain name: ", nm)
    inn <- in_domain[[nm]]
    if (length(inn) != n) stop("Domain ", nm, " has the wrong length")
    inn <- inn & !is.na(inn)
    if (drop_na_value) inn <- inn & !is.na(value)
    num <- ifelse(inn, value, 0)
    num[is.na(num)] <- 0
    vars[[paste0("num_", nm)]] <- num
    vars[[paste0("den_", nm)]] <- as.numeric(inn)
    pieces <- c(pieces, paste0("num_", nm), paste0("den_", nm))
  }
  design$variables <- vars
  est <- survey::svymean(stats::as.formula(paste("~", paste(pieces, collapse = " + "))), design)
  contrasts <- lapply(names(in_domain), function(nm) {
    parse(text = sprintf("num_%s/den_%s", nm, nm))[[1]]
  })
  names(contrasts) <- names(in_domain)
  rates <- survey::svycontrast(est, contrasts)
  rates
}

contrast_difference <- function(rates, a, b) {
  # rates is a svycontrast / svystat with components named a and b.
  survey::svycontrast(rates, bquote(.(as.name(a)) - .(as.name(b))))
}

unpack_stat <- function(stat, label) {
  est <- as.numeric(stat)
  # coef() for a one-row contrast; as.numeric works for both.
  if (length(est) != 1) est <- unname(as.numeric(coef(stat))[[label]])
  se <- as.numeric(survey::SE(stat))
  if (length(se) != 1) se <- unname(as.numeric(survey::SE(stat))[[label]])
  ci <- suppressMessages(stats::confint(stat))
  if (is.matrix(ci) && nrow(ci) > 1) {
    ci <- ci[label, , drop = TRUE]
  }
  ci <- as.numeric(ci)
  list(
    label = label,
    estimate = est,
    se = se,
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    estimate_pp = 100 * est,
    se_pp = 100 * se,
    ci_low_pp = 100 * ci[[1]],
    ci_high_pp = 100 * ci[[2]]
  )
}

# Primary contrast and, optionally, other descriptive group prevalences.
# `group` is a character vector on the design. `outcome` is 0/1/NA.
# The reported difference is level_a minus level_b (short minus reference).
prevalence_contrast <- function(design, group, outcome,
                                level_a = "short", level_b = "reference",
                                extra_levels = "long") {
  g <- design$variables[[group]]
  y <- design$variables[[outcome]]
  levels <- c(level_a, level_b, extra_levels)
  levels <- levels[levels %in% unique(g[!is.na(g)])]
  # Always request a and b even if empty, so a zero denominator fails loudly.
  levels <- unique(c(level_a, level_b, levels))
  in_domain <- lapply(levels, function(lv) !is.na(g) & g == lv)
  names(in_domain) <- levels
  rates <- domain_ratios(design, in_domain, y, drop_na_value = TRUE)
  diff <- survey::svycontrast(
    rates,
    list(difference = parse(text = sprintf("%s - %s", level_a, level_b))[[1]])
  )
  # Prevalence ratio, companion to the difference, same linearization.
  ratio <- survey::svycontrast(
    rates,
    list(ratio = parse(text = sprintf("%s / %s", level_a, level_b))[[1]])
  )
  V <- vcov(rates)
  list(
    levels = unpack_rates(rates),
    difference = unpack_stat(diff, "difference"),
    prevalence_ratio = unpack_stat(ratio, "ratio"),
    covariance = V,
    df = survey::degf(design),
    lonely_psu = getOption("survey.lonely.psu")
  )
}

unpack_rates <- function(rates) {
  cf <- coef(rates)
  se <- survey::SE(rates)
  ci <- suppressMessages(stats::confint(rates))
  out <- lapply(seq_along(cf), function(i) {
    list(
      group = names(cf)[[i]],
      estimate = unname(cf[[i]]),
      se = unname(se[[i]]),
      ci_low = unname(ci[i, 1]),
      ci_high = unname(ci[i, 2]),
      estimate_pp = 100 * unname(cf[[i]]),
      se_pp = 100 * unname(se[[i]]),
      ci_low_pp = 100 * unname(ci[i, 1]),
      ci_high_pp = 100 * unname(ci[i, 2])
    )
  })
  names(out) <- names(cf)
  out
}

# Manski bounds for the prevalence difference when the outcome is missing.
# No assumption about the missing answers other than that a prevalence is between 0 and 1.
# Lower difference: missing short-sleep outcomes set to 0 and missing reference outcomes set to 1.
# Upper difference: the opposite assignment.
# People with an undefined sleep group are not in either bound.
outcome_missing_bounds <- function(design, group, outcome,
                                   level_a = "short", level_b = "reference") {
  g <- design$variables[[group]]
  y <- design$variables[[outcome]]
  in_a <- !is.na(g) & g == level_a
  in_b <- !is.na(g) & g == level_b
  y_L <- ifelse(is.na(y), 0, y)
  y_U <- ifelse(is.na(y), 1, y)
  # Denominator is everyone in the sleep group, missing outcome included.
  # One joint total keeps the covariance of the lower and upper endpoints.
  vars <- design$variables
  vars$num_La <- ifelse(in_a, y_L, 0)
  vars$den_a <- as.numeric(in_a)
  vars$num_Ua <- ifelse(in_a, y_U, 0)
  vars$num_Lb <- ifelse(in_b, y_L, 0)
  vars$den_b <- as.numeric(in_b)
  vars$num_Ub <- ifelse(in_b, y_U, 0)
  design$variables <- vars
  est <- survey::svymean(~ num_La + den_a + num_Ua + num_Lb + den_b + num_Ub, design)
  lower <- survey::svycontrast(est, quote(num_La / den_a - num_Ub / den_b))
  upper <- survey::svycontrast(est, quote(num_Ua / den_a - num_Lb / den_b))
  la <- survey::svycontrast(est, quote(num_La / den_a))
  ua <- survey::svycontrast(est, quote(num_Ua / den_a))
  lb <- survey::svycontrast(est, quote(num_Lb / den_b))
  ub <- survey::svycontrast(est, quote(num_Ub / den_b))
  list(
    assumption = "manski_no_assumption_outcome_bounded_0_1",
    level_a = list(lower = unpack_stat(la, "num_La/den_a"), upper = unpack_stat(ua, "num_Ua/den_a")),
    level_b = list(lower = unpack_stat(lb, "num_Lb/den_b"), upper = unpack_stat(ub, "num_Ub/den_b")),
    difference = list(lower = unpack_stat(lower, "num_La/den_a - num_Ub/den_b"),
                      upper = unpack_stat(upper, "num_Ua/den_a - num_Lb/den_b"))
  )
}

# Weighted predictive margins from a binomial or quasibinomial fit.
# The standard population is the weighted rows in `data` (the shared complete-covariate sample).
# Variance is the delta method applied to vcov(model). Covariates are the fixed standard population.
# `exposure` is a numeric 0/1 column. Margins are reported at 1 and at 0, and as their difference.
predictive_margin_difference <- function(model, data, weights, exposure = "short") {
  if (!exposure %in% names(data)) stop("Exposure column missing: ", exposure)
  w <- as.numeric(weights)
  if (length(w) != nrow(data)) stop("weights length does not match data")
  if (any(!is.finite(w)) || any(w < 0)) stop("weights must be finite and non-negative")
  if (sum(w) <= 0) stop("weights sum to zero")
  tt <- delete.response(terms(model))
  beta <- coef(model)
  V <- vcov(model)
  one <- function(level) {
    d <- data
    d[[exposure]] <- level
    X <- model.matrix(tt, data = d)
    if (!identical(colnames(X), names(beta))) {
      stop(
        "Model matrix columns do not match coefficients.\n  matrix: ",
        paste(colnames(X), collapse = ", "),
        "\n  coef: ",
        paste(names(beta), collapse = ", ")
      )
    }
    eta <- as.numeric(X %*% beta)
    p <- stats::plogis(eta)
    mu <- sum(w * p) / sum(w)
    # d mu / d beta. Logistic derivative p(1-p).
    g <- colSums(w * (p * (1 - p)) * X) / sum(w)
    list(mu = mu, g = g, p = p)
  }
  exposed <- one(1)
  unexposed <- one(0)
  g_diff <- exposed$g - unexposed$g
  se <- function(g) sqrt(as.numeric(drop(t(g) %*% V %*% g)))
  pack <- function(mu, se_mu, label) {
    # Normal critical value. The survey t critical value is applied by the caller
    # when a design df is supplied via attr, but the probability-scale Wald
    # interval here uses the model df if present, else Inf (normal).
    df <- tryCatch(survey::degf(model), error = function(e) Inf)
    if (!is.finite(df) || df <= 0) df <- Inf
    crit <- if (is.finite(df)) stats::qt(0.975, df) else stats::qnorm(0.975)
    list(
      label = label,
      estimate = mu,
      se = se_mu,
      df = df,
      ci_low = mu - crit * se_mu,
      ci_high = mu + crit * se_mu,
      estimate_pp = 100 * mu,
      se_pp = 100 * se_mu,
      ci_low_pp = 100 * (mu - crit * se_mu),
      ci_high_pp = 100 * (mu + crit * se_mu)
    )
  }
  se_e <- se(exposed$g)
  se_u <- se(unexposed$g)
  se_d <- se(g_diff)
  list(
    exposed = pack(exposed$mu, se_e, "short"),
    unexposed = pack(unexposed$mu, se_u, "reference"),
    difference = pack(exposed$mu - unexposed$mu, se_d, "difference"),
    # Independent-margin SE, retained so a test can show we did not use it.
    se_difference_if_independent = sqrt(se_e^2 + se_u^2),
    variance_method = "delta_method_on_survey_coefficient_covariance_fixed_standard_population"
  )
}

make_geo_design <- function(data, lonely = c("adjust", "average", "remove", "fail"),
                              ids = c("psu", "independent")) {
  ensure_survey()
  lonely <- match.arg(lonely)
  ids <- match.arg(ids)
  options(survey.lonely.psu = lonely)
  d <- data[data$in_geo %in% TRUE, , drop = FALSE]
  d <- d[is.finite(d$wt) & d$wt > 0, , drop = FALSE]
  # "psu" is the BRFSS cluster variable, nested in strata because the same
  # sequence number is reused across strata.
  # "independent" reproduces the CDC R snippet (id = ~1): elements, not _PSU.
  if (ids == "independent") {
    survey::svydesign(
      ids = ~1,
      strata = ~ststr,
      weights = ~wt,
      data = d
    )
  } else {
    survey::svydesign(
      ids = ~psu,
      strata = ~ststr,
      weights = ~wt,
      data = d,
      nest = TRUE
    )
  }
}
