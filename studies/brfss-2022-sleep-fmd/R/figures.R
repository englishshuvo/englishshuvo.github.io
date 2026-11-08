# One figure for the primary descriptive prevalences. The inferential contrast
# is short sleep minus 7 to 9 hours. Long sleep is drawn as context.

write_primary_figure <- function(primary, path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package ggplot2 is required for the figure.")
  groups <- c("short", "reference", "long")
  labels <- c(
    short = "Under 7 hours",
    reference = "7 to 9 hours",
    long = "10 or more hours\n(descriptive)"
  )
  rows <- lapply(groups, function(g) {
    x <- primary$levels[[g]]
    data.frame(
      group = labels[[g]],
      estimate = x$estimate_pp,
      lo = x$ci_low_pp,
      hi = x$ci_high_pp,
      stringsAsFactors = FALSE
    )
  })
  dat <- do.call(rbind, rows)
  dat$group <- factor(dat$group, levels = labels)
  diff <- primary$difference
  cap <- sprintf(
    "Difference, under 7 hours minus 7 to 9 hours: %.2f percentage points (95%% CI %.2f to %.2f).\nLong sleep is shown as context and is not part of that contrast. Design-based estimates, BRFSS 2022, 50 states and DC.",
    diff$estimate_pp, diff$ci_low_pp, diff$ci_high_pp
  )
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = group, y = estimate)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lo, ymax = hi),
      width = 0.12, linewidth = 0.6, color = "#0b4f6c"
    ) +
    ggplot2::geom_point(size = 2.8, color = "#0b4f6c") +
    ggplot2::scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
    ggplot2::labs(
      title = "Frequent mental distress by usual hours of sleep",
      subtitle = "Adults in the 50 states and the District of Columbia, BRFSS 2022",
      x = NULL,
      y = "Frequent mental distress (%)",
      caption = cap
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#14181d"),
      plot.subtitle = ggplot2::element_text(color = "#5a6572"),
      plot.caption = ggplot2::element_text(color = "#5a6572", hjust = 0, size = 9),
      axis.text = ggplot2::element_text(color = "#14181d"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  ggplot2::ggsave(path, p, width = 7.2, height = 5.2, dpi = 160, bg = "white")
  invisible(path)
}
