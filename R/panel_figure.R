
# Shared definitions and builder for the three heterogeneity figures

# Panel label for each scenario, in plotting order (suffix added per outcome)
panel_labels <- c(
  main            = "A: Main",
  dead_level      = "B: Number Dead",
  affected_level  = "C: Number Affected",
  flood_length    = "D: Flood Duration",
  buffer          = "E: Distance to Flood",
  main_cause      = "F: Main Flood Cause",
  decade_level    = "G: Decade",
  rural_level     = "H: Urban/Rural",
  sex_level       = "I: Sex",
  wealth_level    = "J: Wealth",
  hh_head_level   = "K: Household Head Education",
  flood_level     = "L: Baseline Flood Frequency",
  school_level    = "M: Baseline Schooling"
)

# Display label for each raw coefficient level (levels not listed are kept as is)
level_labels <- c(
  flooded_10km_age_5_or_6 = "All",
  `poorest quintile`      = "Poorest Quintile",
  `not poorest`           = "Other Quintile",
  low                     = "Low",
  medium                  = "Medium",
  high                    = "High",
  `less frequent`         = "Less Frequent",
  `more frequent`         = "More Frequent",
  `<5000`                 = "<5,000",
  `5000-100000`           = "5,000-100,000",
  `>100000`               = ">100,000",
  `less than 2km`         = "0-2km",
  `<1 month`              = "0-1 months",
  `1–3 months`            = "1-3 months",
  `≥3 months`             = "3+ months"
)

# Order levels appear within a panel
level_order <- c(
  "All", "Urban", "Rural", "Male", "Female", "Poorest Quintile", "Other Quintile",
  "Low", "Medium", "High",
  "<100", "100-500", ">500", "<5,000", "5,000-100,000", ">100,000",
  "0-1 months", "1-3 months", "3+ months", "0-2km", "2-5km", "5-10km",
  "Less Frequent", "More Frequent", "2000s", "2010s",
  "Less than primary", "Primary or higher", "Other", "Tropical Storm"
)

# Build one figure; suffix picks the outcome, scale is 100 for probabilities
plot_panel_figure <- function(all_results, suffix, ylab, file, breaks,
                              scale = 100, labels = ggplot2::waiver(),
                              height = 10, width = 8) {

  scenarios <- paste0(names(panel_labels), suffix)

  res <- all_results %>%
    filter(scenario %in% scenarios) %>%
    mutate(scenario_base = sub(paste0(suffix, "$"), "", scenario))

  res_plot <- res %>%
    transmute(
      scenario_plot = factor(unname(panel_labels[scenario_base]), levels = panel_labels),
      level = relabel_levels(level),
      estimate = estimate * scale,
      conf.low = conf.low * scale,
      conf.high = conf.high * scale,
      p.value
    ) %>%
    mutate(level_f = factor(level, levels = level_order)) %>%
    arrange(level)

  wald_annot <- wald_p_by_scenario(all_results, suffix)

  ggplot(res_plot, aes(x = level_f, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey50") +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.3, size = 0.5) +
    coord_flip(clip = "off") +
    facet_wrap(~scenario_plot, scales = "free_y", ncol = 2, dir = "v") +
    geom_text(
      data = wald_annot,
      aes(x = Inf, y = Inf, label = label),
      inherit.aes = FALSE,
      hjust = 1.05, vjust = 1.1,
      size = 3
    ) +
    labs(x = NULL, y = ylab) +
    theme_clean(base_size = 11) +
    theme(
      panel.spacing.y = unit(10, "pt"),
      strip.text.x = element_text(size = 10, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.background = element_blank()
    ) +
    scale_y_continuous(breaks = breaks, labels = labels)

  ggsave(file, height = height, width = width)
}

# Apply the display labels, keeping any level that has no label defined
relabel_levels <- function(x) {
  out <- unname(level_labels[as.character(x)])
  if_else(is.na(out), as.character(x), out)
}
