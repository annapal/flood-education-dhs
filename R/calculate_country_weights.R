
# Calculate the country weights, based on population of each country

calculate_country_weights <- function() {
  
  # Get list of all countries to include
  countries <- read.csv("data/countries.csv")$dhs_cde
  
  # Get population data from WPP
  pop_dat <- read.csv("data/population/WPP2024_Demographic_Indicators_Medium.csv")
  
  # Add DHS country codes & filter data
  pop_dat$dhs_cde <- countrycode(pop_dat$ISO3_code, "iso3c", "dhs", warn=FALSE)
  pop_dat <- pop_dat %>%
    filter(dhs_cde %in% countries & Time==2025) %>% # Filter only included countries & 2025 data
    select(dhs_cde, Time, TPopulation1Jan)
  
  # Calculate the country weights
  pop_dat <- pop_dat %>%
    mutate(total = sum(TPopulation1Jan, na.rm=TRUE),
           country_weight = TPopulation1Jan / total)
  
  pop_dat
}