
# Make a summary table of all the countries, sample size and surveys

make_summary_table <- function() {
  
  require_input("data/analysis_data/flood_56_age_10.rds", analysis_data_source)
  dhs_dat <- readRDS("data/analysis_data/flood_56_age_10.rds")

  # Restrict to the main model's estimation sample so per-country Ns match it
  main_mod <- feols(
    no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric |
      clustid + country_year,
    data = dhs_dat, cluster = ~clustid, weights = ~weight
  )
  dhs_dat <- dhs_dat[obs(main_mod), ]

  all_countries <- dhs_dat %>%
    group_by(country_id) %>%
    summarise(
      Surveys = paste(unique(hv000), collapse = ", "),
      Sample = n()
    ) %>%
    mutate(
      Country = countrycode(country_id, "dhs", "country.name")
    ) %>%
    select(Country, Surveys, Sample)
  
  write_xlsx(all_countries, "results/table_main.xlsx")
}
