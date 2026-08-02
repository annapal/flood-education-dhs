
add_country_weights <- function(dhs_dat_combined) {
  # Add weights such that each country is represented equally
  dhs_dat_combined <- dhs_dat_combined %>%
    group_by(country_id) %>%
    mutate(
      total_country_weight = sum(weight, na.rm = TRUE),
      weight_equal = if_else(total_country_weight > 0, weight / total_country_weight, NA_real_)
    ) %>%
    ungroup()
  
  # Add weights such that each country is represented proportional to population
  pop_dat <- calculate_country_weights()
  dhs_dat_combined <- dhs_dat_combined %>%
    left_join(pop_dat %>% select(dhs_cde, country_weight),
              by = c("country_id" = "dhs_cde")) %>%
    mutate(weight_pop = weight_equal * country_weight)
  
  dhs_dat_combined
}