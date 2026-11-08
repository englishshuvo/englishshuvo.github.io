# Aggregate outputs only. No respondent-level rows.

fmt_int <- function(x) format(as.integer(round(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pp <- function(x) sprintf("%.2f", x)
fmt_pct <- function(x_pp) sprintf("%.2f%%", x_pp)

stat_line <- function(stat, noun = "percentage points") {
  sprintf(
    "%s %s (95%% CI %s to %s)",
    fmt_pp(stat$estimate_pp),
    noun,
    fmt_pp(stat$ci_low_pp),
    fmt_pp(stat$ci_high_pp)
  )
}

write_outputs <- function(out_dir, provenance, audit, estimates) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(provenance, file.path(out_dir, "provenance.json"), auto_unbox = TRUE, pretty = TRUE, digits = 10)
  jsonlite::write_json(audit, file.path(out_dir, "schema-audit.json"), auto_unbox = TRUE, pretty = TRUE, digits = 8)
  if (!is.null(estimates)) {
    utils::write.csv(estimates$flow, file.path(out_dir, "sample-flow.csv"), row.names = FALSE)
    utils::write.csv(estimates$missingness, file.path(out_dir, "missingness.csv"), row.names = FALSE)
    primary_rows <- do.call(rbind, lapply(estimates$primary$levels, function(x) {
      data.frame(
        group = x$group,
        prevalence = x$estimate,
        se = x$se,
        ci_low = x$ci_low,
        ci_high = x$ci_high,
        prevalence_pp = x$estimate_pp,
        se_pp = x$se_pp,
        ci_low_pp = x$ci_low_pp,
        ci_high_pp = x$ci_high_pp,
        stringsAsFactors = FALSE
      )
    }))
    utils::write.csv(primary_rows, file.path(out_dir, "prevalence-by-sleep.csv"), row.names = FALSE)
    covm <- estimates$primary$covariance
    cov_out <- list(
      groups = rownames(covm),
      covariance = matrix(as.numeric(covm), nrow = nrow(covm))
    )
    payload <- estimates
    payload$primary$covariance <- cov_out
    jsonlite::write_json(payload, file.path(out_dir, "estimates.json"), auto_unbox = TRUE, pretty = TRUE, digits = 10, na = "null")
    write_results_markdown(estimates, file.path(out_dir, "results-fragment.md"))
    write_key_strings(estimates, audit, file.path(out_dir, "key-strings.txt"))
  }
  writeLines(utils::capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
}

write_results_markdown <- function(estimates, path) {
  p <- estimates$primary
  lines <- c(
    "### Weighted prevalence of frequent mental distress",
    "",
    "| Usual sleep | Prevalence | 95% CI |",
    "| --- | ---: | --- |"
  )
  label <- c(short = "Under 7 hours", reference = "7 to 9 hours", long = "10 to 24 hours, descriptive")
  for (g in c("short", "reference", "long")) {
    x <- p$levels[[g]]
    lines <- c(lines, sprintf(
      "| %s | %s | %s to %s |",
      label[[g]], fmt_pct(x$estimate_pp), fmt_pct(x$ci_low_pp), fmt_pct(x$ci_high_pp)
    ))
  }
  d <- p$difference
  r <- p$prevalence_ratio
  lines <- c(
    lines,
    "",
    "### Primary contrast",
    "",
    sprintf(
      "Under 7 hours minus 7 to 9 hours: **%s percentage points** (95%% CI %s to %s).",
      fmt_pp(d$estimate_pp), fmt_pp(d$ci_low_pp), fmt_pp(d$ci_high_pp)
    ),
    "",
    sprintf(
      "Prevalence ratio, under 7 hours over 7 to 9 hours: **%.3f** (95%% CI %.3f to %.3f). This is a ratio of the two prevalences, not an odds ratio.",
      r$estimate, r$ci_low, r$ci_high
    ),
    ""
  )
  sec <- estimates$secondary$margins
  lines <- c(
    lines,
    "### Age- and sex-standardized margins",
    "",
    sprintf(
      "Standardized prevalence, under 7 hours: %s (95%% CI %s to %s).",
      fmt_pct(sec$exposed$estimate_pp), fmt_pct(sec$exposed$ci_low_pp), fmt_pct(sec$exposed$ci_high_pp)
    ),
    "",
    sprintf(
      "Standardized prevalence, 7 to 9 hours: %s (95%% CI %s to %s).",
      fmt_pct(sec$unexposed$estimate_pp), fmt_pct(sec$unexposed$ci_low_pp), fmt_pct(sec$unexposed$ci_high_pp)
    ),
    "",
    sprintf(
      "Standardized difference: **%s percentage points** (95%% CI %s to %s).",
      fmt_pp(sec$difference$estimate_pp), fmt_pp(sec$difference$ci_low_pp), fmt_pp(sec$difference$ci_high_pp)
    ),
    ""
  )
  b <- estimates$bounds$difference
  lines <- c(
    lines,
    "### Sensitivities for the primary difference",
    "",
    "| Analysis | Difference, percentage points | 95% CI |",
    "| --- | ---: | --- |",
    sprintf("| Primary, complete outcome, lonely PSU adjust | %s | %s to %s |", fmt_pp(d$estimate_pp), fmt_pp(d$ci_low_pp), fmt_pp(d$ci_high_pp)),
    sprintf("| Singleton strata, average variance | %s | %s to %s |", fmt_pp(estimates$lonely$average$estimate_pp), fmt_pp(estimates$lonely$average$ci_low_pp), fmt_pp(estimates$lonely$average$ci_high_pp)),
    sprintf("| Singleton strata, remove | %s | %s to %s |", fmt_pp(estimates$lonely$remove$estimate_pp), fmt_pp(estimates$lonely$remove$ci_low_pp), fmt_pp(estimates$lonely$remove$ci_high_pp)),
    sprintf("| CDC R snippet cross-check, id = ~1 | %s | %s to %s |", fmt_pp(estimates$independent_ids$estimate_pp), fmt_pp(estimates$independent_ids$ci_low_pp), fmt_pp(estimates$independent_ids$ci_high_pp)),
    sprintf("| Exclude usual sleep below 3 or above 12 hours | %s | %s to %s |", fmt_pp(estimates$extreme$estimate_pp), fmt_pp(estimates$extreme$ci_low_pp), fmt_pp(estimates$extreme$ci_high_pp)),
    sprintf("| Reference restricted to 7 to 8 hours | %s | %s to %s |", fmt_pp(estimates$ref78$estimate_pp), fmt_pp(estimates$ref78$ci_low_pp), fmt_pp(estimates$ref78$ci_high_pp)),
    sprintf("| No-assumption bounds, lower endpoint | %s | %s to %s |", fmt_pp(b$lower$estimate_pp), fmt_pp(b$lower$ci_low_pp), fmt_pp(b$lower$ci_high_pp)),
    sprintf("| No-assumption bounds, upper endpoint | %s | %s to %s |", fmt_pp(b$upper$estimate_pp), fmt_pp(b$upper$ci_low_pp), fmt_pp(b$upper$ci_high_pp)),
    ""
  )
  writeLines(lines, path)
}

write_key_strings <- function(estimates, audit, path) {
  d <- estimates$primary$difference
  s <- estimates$primary$levels$short
  r <- estimates$primary$levels$reference
  sec <- estimates$secondary$margins$difference
  strings <- c(
    fmt_int(audit$n_records),
    fmt_int(audit$geography$n_states_dc),
    fmt_int(estimates$flow$unweighted_n[estimates$flow$step == "5. Observed mental-health days"]),
    paste0(fmt_pct(s$estimate_pp), " (95% CI ", fmt_pct(s$ci_low_pp), " to ", fmt_pct(s$ci_high_pp), ")"),
    paste0(fmt_pct(r$estimate_pp), " (95% CI ", fmt_pct(r$ci_low_pp), " to ", fmt_pct(r$ci_high_pp), ")"),
    paste0(fmt_pp(d$estimate_pp), " percentage points (95% CI ", fmt_pp(d$ci_low_pp), " to ", fmt_pp(d$ci_high_pp), ")"),
    paste0(fmt_pp(sec$estimate_pp), " percentage points (95% CI ", fmt_pp(sec$ci_low_pp), " to ", fmt_pp(sec$ci_high_pp), ")")
  )
  writeLines(strings, path)
}

check_case_study <- function(study_root) {
  key_path <- file.path(study_root, "output", "key-strings.txt")
  qmd <- file.path(study_root, "..", "..", "notebooks", "brfss-2022-sleep-fmd", "case-study.qmd")
  qmd <- normalizePath(qmd, mustWork = FALSE)
  if (!file.exists(key_path)) stop("key-strings.txt is missing. Run the estimation first.")
  if (!file.exists(qmd)) stop("Case study is missing: ", qmd)
  keys <- readLines(key_path, warn = FALSE)
  text <- paste(readLines(qmd, warn = FALSE), collapse = "\n")
  missing <- keys[!vapply(keys, function(k) grepl(k, text, fixed = TRUE), logical(1))]
  if (length(missing)) {
    stop("Case study is missing executed results:\n", paste(missing, collapse = "\n"))
  }
  if (grepl("\u2014|\u2013", text)) stop("Case study contains an em dash or an en dash.")
  message("Case study contains the executed key strings.")
  invisible(TRUE)
}
