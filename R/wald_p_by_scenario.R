
# Wald statistic for equality of effects in each panel of panel_labels

wald_p_by_scenario <- function(all_results, suffix = "") {
  scenarios <- setdiff(names(panel_labels), "main")

  mn <- subset(all_results, scenario == paste0("main", suffix))

  out <- lapply(scenarios, function(sc) {

    lv <- subset(all_results, scenario == paste0(sc, suffix))

    # Nothing to test if the scenario is missing or has a single level
    df <- nrow(lv) - 1
    if (df < 1) return(NULL)

    W <- sum(((lv$estimate - mn$estimate) / lv$std.error)^2)
    p <- pchisq(W, df = df, lower.tail = FALSE)

    data.frame(
      scenario = sc,
      W = W,
      df = df,
      p_value = p
    )
  })

  wald_results <- do.call(rbind, out)

  # Tidy for plotting
  wald_annot <- wald_results %>%
    mutate(
      scenario_plot = factor(unname(panel_labels[scenario]), levels = panel_labels),
      label = paste0("Wald test (equal effects): ",
                     scales::pvalue(p_value, accuracy = 0.001, add_p = TRUE))
    )
  wald_annot
}
