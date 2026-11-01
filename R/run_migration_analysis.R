
# Migration analysis using years-in-residence (v104) for the mother or household head

run_migration_analysis <- function(who = c("mother", "head")) {

  who <- match.arg(who)
  v104 <- paste0(who, "_v104")
  v106 <- paste0(who, "_v106")

  require_input("data/analysis_data/flood_56_age_10.rds", analysis_data_source)
  dat <- readRDS("data/analysis_data/flood_56_age_10.rds")

  # Restrict to the main model's estimation sample so Ns match the regression
  main_mod <- feols(
    no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric |
      clustid + country_year,
    data = dat, cluster = ~clustid, weights = ~weight
  )
  dat <- dat[obs(main_mod), ]

  # Keep only those with years-in-residence (the migration measure)
  dat_mig <- dat %>% filter(!is.na(.data[[v104]]))

  # Living in the same location at flood time (v104 = 95 always, 96/99 missing)
  dat_mig <- dat_mig %>%
    mutate(same_location = case_when(
      .data[[v104]] == 95 ~ TRUE,
      .data[[v104]] %in% c(96, 99) ~ NA,
      .data[[v104]] >= hv007 - year_age_4 ~ TRUE,
      TRUE ~ FALSE
    )) 
  
  # filter to those with non-missing location status
  dat_mig <- dat_mig %>% filter(!is.na(same_location)) 
  
  # Main regression on the subset with migration information
  model1 <- feols(
    no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    data = dat_mig, cluster = ~clustid, weights = ~weight
  )
  
  # Restrict to the regression's estimation sample so the table Ns match it
  dat_mig <- dat_mig[obs(model1), ]
  
  # filter to those living in same location
  dat_mig2 <- dat_mig %>%
    filter(same_location)

  # Main regression on the non-migrant subset
  model <- feols(
    no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    data = dat_mig2, cluster = ~clustid, weights = ~weight
  )

  # Restrict to the regression's estimation sample so the table Ns match it
  dat_mig2 <- dat_mig2[obs(model), ]

  # Comparison table (make_tables() style): full sample vs non-migrant subset
  pct <- function(x) paste0(sum(x, na.rm = TRUE), " (",
                            round(mean(x, na.rm = TRUE) * 100, 1), "%)")
  mean_iqr <- function(x) {
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    paste0(round(mean(x, na.rm = TRUE), 1), " (",
           round(q[1], 1), "–", round(q[2], 1), ")")
  }
  tab_summary <- function(d, group_name) {
    d %>%
      summarise(
        flooded = pct(flooded_10km_age_5_or_6 == 1),
        n = as.character(n()),
        mean_age = mean_iqr(hv105),
        mean_education_years = mean_iqr(hv108),
        Male = pct(hv104 == "Male"),
        Female = pct(hv104 == "Female"),
        Urban = pct(hv025 == "Urban"),
        Rural = pct(hv025 == "Rural"),
        no_edu = pct(hv109 == "No education"),
        primary_incomp = pct(hv109 == "Incomplete primary"),
        primary = pct(hv109 == "Complete Primary"),
        secondary_incomp = pct(hv109 == "Incomplete Secondary"),
        secondary = pct(hv109 == "Complete Secondary"),
        higher = pct(hv109 == "Higher"),
        poorest = pct(hv270 == "Poorest"),
        poorer = pct(hv270 == "Poorer"),
        middle = pct(hv270 == "Middle"),
        richer = pct(hv270 == "Richer"),
        richest = pct(hv270 == "Richest")
      ) %>%
      pivot_longer(everything(), names_to = "variable", values_to = group_name)
  }

  comparison <- tab_summary(dat, "Full sample") %>%
    left_join(tab_summary(dat_mig, "Migration information available"), by = "variable") %>%
    left_join(tab_summary(dat_mig2, "Reported not moved"), by = "variable") %>%
    mutate(variable = recode(variable,
                             n = "Sample Size",
                             mean_age = "Mean Age",
                             mean_education_years = "Mean Years of Education",
                             Male = "Male",
                             Female = "Female",
                             Urban = "Urban",
                             Rural = "Rural",
                             no_edu = "No Education",
                             primary_incomp = "Incomplete primary",
                             primary = "Completed Primary",
                             secondary_incomp = "Incomplete secondary",
                             secondary = "Completed Secondary",
                             higher = "Higher Education",
                             poorest = "Poorest Quintile",
                             poorer = "Poorer Quintile",
                             middle = "Middle Quintile",
                             richer = "Richer Quintile",
                             richest = "Richest Quintile",
                             flooded = "Flooded"
    ))
  print(comparison)
  write_xlsx(comparison, sprintf("results/migration_comparison_%s.xlsx", who))

  # QBA: migrants from flooded areas look unexposed, so each draw reassigns a
  # fraction of the observed-unexposed to flooded and refits
  f <- 1-nrow(dat_mig2)/(nrow(dat_mig))
  message(sprintf("Migrant fraction (f): %.4f", f))

  model_full <- feols(
    no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    data = dat, cluster = ~clustid, weights = ~weight
  )

  # Flood coefficient from both models, with the number of observations fitted on
  mig_results <- bind_rows(
    broom::tidy(model_full, conf.int = TRUE) %>%
      mutate(sample = "Full sample", n = nobs(model_full)),
    broom::tidy(model1, conf.int = TRUE) %>%
      mutate(sample = "Migration information available", n = nobs(model)),
    broom::tidy(model, conf.int = TRUE) %>%
      mutate(sample = "Reported not moved", n = nobs(model))
  ) %>%
    filter(term == "flooded_10km_age_5_or_6") %>%
    mutate(migrant_fraction = f, who = who)

  write.csv(mig_results, sprintf("results/migration_results_%s.csv", who),
            row.names = FALSE)

  # Compare the flood estimate: full sample vs non-migrant subset
  compare_est <- mig_results %>%
    mutate(across(c(estimate, conf.low, conf.high), ~ .x * 100))
  compare_est$sample <- factor(compare_est$sample,
                               levels = c("Reported not moved",
                                          "Migration information available", 
                                          "Full sample"))

  ggplot(compare_est, aes(x = sample, y = estimate,
                          ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3,
               color = "grey50") +
    geom_pointrange(linewidth = 0.3, size = 0.5) +
    coord_flip() +
    labs(x = NULL,
         y = "Percentage point change in the probability of no schooling (95% CI)") +
    theme_clean(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.background = element_blank()
    )
  ggsave(sprintf("figures/migration_subset_estimate_%s.png", who),
         width = 6, height = 2)

  set.seed(1)   
  n_draws <- 100

  # Only observed-unexposed children are eligible for reassignment to flooded.
  unexp <- which(dat$flooded_10km_age_5_or_6 == 0)
  no_edu_unexp <- dat$no_education[unexp] == 1
  no_edu_unexp[is.na(no_edu_unexp)] <- FALSE

  # Per-row reassignment probabilities; both scenarios average to f
  # Nondifferential: rate f regardless of the no_education outcome
  p_nondiff <- rep(f, length(unexp))
  # Differential: diff_ratio-fold more likely among no_education == 1, rescaled to f
  diff_ratio <- 1.05
  p1 <- mean(no_edu_unexp)
  rate_lo <- f / (1 + (diff_ratio - 1) * p1)
  p_diff <- ifelse(no_edu_unexp, diff_ratio * rate_lo, rate_lo)

  # In-migration: truly unexposed children who moved into flooded areas look exposed
  exp_rows <- which(dat$flooded_10km_age_5_or_6 == 1)
  no_edu_exp <- dat$no_education[exp_rows] == 1
  no_edu_exp[is.na(no_edu_exp)] <- FALSE

  # Fixed in-migration rate, assumed lower than f
  g <- f
  # Differential: diff_ratio-fold more likely among no_education == 1, rescaled to g
  p1_in <- mean(no_edu_exp)
  rate_lo_in <- g / (1 + (diff_ratio - 1) * p1_in)
  p_in_diff <- ifelse(no_edu_exp, diff_ratio * rate_lo_in, rate_lo_in)

  # Draw misclassified children, flip their exposure to `to`, refit, return the flood coefficient per draw
  run_qba <- function(prob, rows = unexp, to = 1) {
    replicate(n_draws, {
      d <- dat
      reassign <- rows[runif(length(rows)) < prob]
      d$flooded_10km_age_5_or_6[reassign] <- to
      m <- feols(
        no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
        data = d, cluster = ~clustid, weights = ~weight
      )
      coef(m)["flooded_10km_age_5_or_6"]
    })
  }

  qba_coefs <- list(Differential = run_qba(p_diff),
                    Nondifferential = run_qba(p_nondiff),
                    `In-migration differential` = run_qba(p_in_diff, exp_rows, to = 0))

  qba_df <- bind_rows(
    data.frame(coef = qba_coefs$Differential * 100, scenario = "Out-migration differential"),
    data.frame(coef = qba_coefs$Nondifferential * 100, scenario = "Nondifferential"),
    data.frame(coef = qba_coefs$`In-migration differential` * 100, scenario = "In-migration differential")
  )

  obs <- coef(model_full)["flooded_10km_age_5_or_6"] * 100
  qba <- qba_df %>%
    group_by(scenario) %>%
    summarise(
      coef_observed = obs,
      coef_reassigned_mean = mean(coef),
      coef_reassigned_lo = quantile(coef, 0.025),
      coef_reassigned_hi = quantile(coef, 0.975)
    ) %>%
    mutate(migrant_fraction = f, in_migrant_rate = g, n_draws = n_draws, who = who)

  write.csv(qba, sprintf("results/migration_qba_%s.csv", who), row.names = FALSE)

  # Reassigned coefficient distributions, with the observed estimate in red
  ggplot(qba_df, aes(coef)) +
    geom_histogram(bins = 30, fill = "grey70", colour = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3,
               color = "grey50") +
    geom_vline(xintercept = obs, color = "red", linewidth = 0.5) +
    annotate("text", x = obs, y = Inf, label = "Main effect", color = "red",
             size = 3, hjust = 1.1, vjust = 1.5) +
    facet_wrap(~ scenario, ncol = 1) +
    labs(x = "Percentage point change in the probability of no schooling",
         y = "Count") +
    theme_clean(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      strip.text = element_text(size = 10),
      plot.background = element_blank()
    )
  ggsave(sprintf("figures/migration_qba_hist_%s.png", who),
         width = 5, height = 5)

}
