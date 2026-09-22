
# Calculate the number of households in each country and year

calculate_no_households <- function() {
  
  # Get population data from WPP
  require_input("data/population/WPP2024_Demographic_Indicators_Medium.csv", wpp_source)
  pop_dat <- read.csv("data/population/WPP2024_Demographic_Indicators_Medium.csv")
  
  # Add DHS country codes
  pop_dat$dhs_cde <- countrycode(pop_dat$ISO3_code, "iso3c", "dhs", warn=FALSE)
  pop_dat <- pop_dat %>%
    filter(!is.na(dhs_cde)) %>% # Filter only DHS countries
    select(dhs_cde, Time, TPopulation1Jan) %>%
    mutate(population = TPopulation1Jan*1000)
  
  # Get household size data from CORESIDENCE
  require_input("data/population/12721630/A0_CORESIDENCE_NATIONAL_DATASET_2024.csv",
                coresidence_source)
  house_dat <- read.csv("data/population/12721630/A0_CORESIDENCE_NATIONAL_DATASET_2024.csv")
  house_dat$dhs_cde <- countrycode(house_dat$C1, "iso3c", "dhs", warn=FALSE)
  house_dat <- house_dat %>%
    filter(!is.na(dhs_cde)) %>%
    select(dhs_cde, T1, HS17)
  
  # Calculate the number of households
  pop_dat <- pop_dat %>%
    group_by(dhs_cde) %>%
    mutate(
      match_info = map2(
        dhs_cde, Time,
        ~ {
          rows <- house_dat %>% filter(dhs_cde == .x)
          if (nrow(rows) == 0) return(tibble(HS17 = NA_real_, HS17_year = NA_integer_))
          closest <- rows %>%
            slice_min(abs(T1 - .y), n = 1, with_ties = FALSE)
          tibble(HS17 = closest$HS17, HS17_year = closest$T1)
        }
      ),
      HS17 = map_dbl(match_info, ~ .x$HS17),
      HS17_year = map_int(match_info, ~ .x$HS17_year),
      households = population / HS17
    ) %>%
    select(-match_info) %>%
    ungroup()
  
  pop_dat
}
