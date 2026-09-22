
plot_results <- function() {
  
  # Import the results CSV
  require_input(c("results/results.csv", "results/single_flood_results.csv",
                  "results/matching_results.csv"), results_source)
  all_results <- read.csv("results/results.csv")
  
  # Plot the main results with different specifications
  results <- all_results %>%
    filter(scenario %in% c(
      "main", "pop_weight", "no_cluster", "no_covariates", "only_wealth",
      "only_sex", "hh_covariates", "adj_temp", "main_12", "main_15"
    )) %>%
    mutate(scenario = factor(scenario, levels = c(
      "main", "pop_weight", "no_cluster", "no_covariates", "only_wealth",
      "only_sex", "hh_covariates", "adj_temp", "main_12", "main_15", "main_primary"
    ))) %>%
    arrange(scenario) %>%
    mutate(
      scen = c("main", "weighted by\n population", "no cluster\n FE", "no covariates",
               "only wealth\n covariate", "only sex\n covariate", "add household\n covariates",
               "adjusted for\n temperature", "restrict\n age 12", "restrict\n age 15")
    ) %>%
    select(estimate, conf.low, conf.high, scen)

  # Append the single-flood sensitivity result (clusters with 1 DFO event)
  single <- read.csv("results/single_flood_results.csv") %>%
    transmute(estimate, conf.low, conf.high, scen = "single flood\n clusters")
  results <- bind_rows(results, single)

  # Append the matched (propensity-score) sensitivity result
  matched <- read.csv("results/matching_results.csv") %>%
    transmute(estimate, conf.low, conf.high, scen = "matched")
  results <- bind_rows(results, matched)

  results$scen <- factor(results$scen, levels = rev(unique(results$scen)))
  results$scen <- fct_relevel(results$scen, "adjusted for\n temperature")  # first level plots at the bottom
  results$is_main <- results$scen == "main"
  ggplot(results, aes(x = scen, y = estimate*100, ymin = conf.low*100, ymax = conf.high*100, color = is_main)) +
    geom_pointrange(aes(ymin = conf.low*100, ymax = conf.high*100), linewidth = 0.3, size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black"), guide = "none") +
    labs(
      x = "",
      y = "Percentage point change in the probability of no schooling (95% CI)",
      title = ""
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme_clean(base_size = 11) +
    theme(
      panel.spacing.y = unit(10, "pt"),
      strip.text.x = element_text(size = 10, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.background=element_blank() 
    ) +
    ylim(-0.8, 1.5) +
    coord_flip()
  ggsave("figures/results_by_specification.png", width = 5, height = 5)
  
  # Plot results by country
  results <- all_results %>%
    filter(str_detect(scenario, "country_level")) %>%
    mutate(
      scen = countrycode(level, "dhs", "country.name")) %>%
    arrange(estimate)
  results$scen <- factor(results$scen, levels = unique(results$scen))
  ggplot(results, aes(x = scen, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.3, size = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      x = "",
      y = "Percentage point change in the probability of having no schooling",
      title = ""
    ) +
    theme_clean(base_size = 11) +
    theme(
      panel.spacing.y = unit(10, "pt"),
      strip.text.x = element_text(size = 10, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.background=element_blank() 
    ) +
    coord_flip()
  ggsave("figures/results_by_country.png", width = 5, height = 8)

  # Plot results by region and flood cause
  results <- all_results %>%
    filter(scenario == "region_cause_level") %>%
    mutate(
      region = sub(" \\| .*$", "", level),
      cause  = sub("^.* \\| ", "", level)
    )
  ggplot(results, aes(x = cause, y = estimate*100, ymin = conf.low*100, ymax = conf.high*100)) +
    geom_pointrange(linewidth = 0.3, size = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~region) +
    labs(x = "",
         y = "Percentage point change in the probability of no schooling (95% CI)") +
    theme_clean(base_size = 11) +
    theme(
      panel.spacing.y = unit(10, "pt"),
      strip.text.x = element_text(size = 9, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      plot.background = element_blank()
    ) +
    coord_flip()
  ggsave("figures/region_cause_heterogeneity.png", width = 8, height = 4)


  # Main heterogeneity figures (same panels for all three; see panel_figure.R) ---

  plot_panel_figure(
    all_results, suffix = "",
    ylab = "Percentage point change in the probability of no schooling (95% CI)",
    file = "figures/main.jpeg",
    breaks = seq(-0.5, 2, by = 0.5)
  )

  plot_panel_figure(
    all_results, suffix = "_primary",
    ylab = "Percentage point change in the probability of not completing primary (95% CI)",
    file = "figures/main_primary.jpeg",
    breaks = seq(-1, 3, by = 1)
  )

  plot_panel_figure(
    all_results, suffix = "_attain",
    ylab = "Change in the number of primary years attained (95% CI)",
    file = "figures/main_attain.jpeg",
    breaks = seq(-0.3, 0.1, by = 0.1),
    scale = 1,
    labels = scales::label_number(accuracy = 0.1)
  )
}
