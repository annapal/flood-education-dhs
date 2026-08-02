
# Plot proportion exposed to flood and proportion with no schooling

plot_proportions <- function() {
  
  dhs_dat <- readRDS("data/analysis_data/flood_56_age_10.rds")
  
  # Proportion receiving no schooling & prop exposed to flood
  prop_school <- dhs_dat %>%
    group_by(country_id) %>%
    summarise(
      no_education_prop = mean(hv109 == "No education", na.rm = TRUE),
      flood_exposed = mean(flooded_10km_age_5_or_6, na.rm = TRUE)
    ) %>%
    mutate(country = countrycode(country_id, "dhs", "country.name.en")) %>%
    bind_rows(
      dhs_dat %>%
        summarise(
          country_id = "ALL",
          no_education_prop = mean(hv109 == "No education", na.rm = TRUE),
          flood_exposed = mean(flooded_10km_age_5_or_6, na.rm = TRUE)
        ) %>%
        mutate(country = "All countries")
    )
  
  # Merge all the data
  df_long <- prop_school %>%
    pivot_longer(
      cols = c(no_education_prop, flood_exposed),
      names_to = "indicator",
      values_to = "proportion"
    ) %>%
    mutate(
      indicator = recode(indicator,
                         no_education_prop = "B: No Schooling",
                         flood_exposed = "A: Flood Exposed")
    )
  
  # Make the plot
  p <- ggplot(
    df_long %>%
      mutate(
        fill_color = case_when(
          country == "All countries" & indicator == "B: No Schooling" ~ "#6B2F0F",
          country == "All countries" & indicator == "A: Flood Exposed" ~ "#1B2F55",
          indicator == "B: No Schooling" ~ "#DD8452",
          indicator == "A: Flood Exposed" ~ "#4C72B0"
        )
      ),
    aes(x = proportion, y = fct_rev(country), fill = fill_color)
  ) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~indicator, ncol = 2, scales = "free_x") +
    xlim(0, 1) +
    scale_fill_identity() +
    labs(x = "Proportion", y = NULL) +
    theme_clean(base_size = 11) +
    theme(
      panel.spacing.y = unit(10, "pt"),
      strip.text.x = element_text(size = 10, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.background=element_blank() 
    )
  ggsave("figures/exposure_bar.jpeg", height = 8, width=6, plot=p)
  
}