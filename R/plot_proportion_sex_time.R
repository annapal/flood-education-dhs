
plot_proportion_sex_time <- function() {

  require_input("data/analysis_data/flood_56_age_10.rds", analysis_data_source)
  dhs_dat <- readRDS("data/analysis_data/flood_56_age_10.rds")
  
  # Likelihood of starting school over time
  prop_time <- dhs_dat %>%
    filter(!is.na(hv104)) %>%
    mutate(
      year_group = cut(
        year_age_5,
        breaks = seq(min(year_age_5, na.rm = TRUE),
                     max(year_age_5, na.rm = TRUE) + 1,
                     by = 3),
        right = FALSE,
        include.lowest = TRUE,
        labels = paste(
          seq(min(year_age_5, na.rm = TRUE),
              max(year_age_5, na.rm = TRUE) - 2, by = 3),
          seq(min(year_age_5, na.rm = TRUE) + 2,
              max(year_age_5, na.rm = TRUE), by = 3),
          sep = "-"
        )
      )
    ) %>%
    group_by(year_group, hv104) %>%
    summarise(
      no_education_prop = mean(no_education, na.rm = TRUE),
      .groups = "drop"
    )
  
  prop_time_plot <- prop_time %>%
    mutate(year_group = fct_relevel(as.character(year_group), unique(as.character(year_group))))
  prop_time_plot$year_id <- as.numeric(prop_time_plot$year_group)
  
  # Plot the data
  p <- ggplot(prop_time_plot, aes(x = year_id, y = no_education_prop, color = hv104)) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Male" = "#1f78b4", "Female" = "#e31a1c"),
      name = "Sex"
    ) +
    scale_x_continuous(
      breaks = prop_time_plot$year_id,
      labels = prop_time_plot$year_group
    ) +
    labs(
      x = "Year",
      y = "Proportion with no schooling"
    ) +
    theme_clean(base_size = 11) +
    theme(
      panel.spacing.y = unit(10, "pt"),
      strip.text.x = element_text(size = 10, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.background=element_blank() 
    ) +
    ylim(0, 0.17)
  ggsave("figures/proportion_sex_time.jpeg", plot=p)
}
