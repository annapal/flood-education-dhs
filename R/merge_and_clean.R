
# Merge flood data with survey data and clean variables

merge_and_clean <- function(dhs_dat, fl_dat, pop_dat, country) {
  
  # Merge the flood data for each age of exposure
  for (age in 0:14) {
    
    # Calculate the year a person was each age 0-14
    year_val <- dhs_dat$hv007 - dhs_dat$hv105 + age
    dhs_dat[[paste0("year_age_", age)]] <- ifelse(year_val <= dhs_dat$hv007, year_val, NA)
    
    # Create a renamed copy of fl_dat with suffix for age
    fl_dat_renamed <- fl_dat %>%
      rename(hv001 = DHSCLUST) %>%
      rename_with(~ paste0(.x, "_age_", age), 
                  .cols = -c(hv001))
    
    # Perform the join on year and cluster
    dhs_dat <- suppressMessages(left_join(dhs_dat, fl_dat_renamed))
  }
  
  # Merge flood exposure for up to 5 years prior to the survey year
  for (year in 0:5) {
    
    # Calculate the years before the survey year
    year_val <- dhs_dat$hv007 - year
    dhs_dat[[paste0("year_lag_", year)]] <- dhs_dat$hv007 - year
    
    # Create a renamed copy of fl_dat with suffix for lagged year
    fl_dat_renamed <- fl_dat %>%
      rename(hv001 = DHSCLUST) %>%
      rename_with(~ paste0(.x, "_lag_", year), 
                  .cols = -c(hv001))
    
    # Perform the join on year and cluster
    dhs_dat <- suppressMessages(left_join(dhs_dat, fl_dat_renamed))
  }
  
  # Create unique case id, household id, and cluster id
  dhs_dat$caseid <- paste(dhs_dat$hv000, dhs_dat$hhid, dhs_dat$hvidx, sep = "-")
  dhs_dat$hhid <- paste(dhs_dat$hv000, dhs_dat$hhid, sep = "-")
  dhs_dat$clustid <- paste(dhs_dat$hv000, dhs_dat$hv001, sep = "-")
  
  # Fix 2-digit years
  dhs_dat <- dhs_dat %>%
    mutate(
      hv007 = case_when(
        hv007 < 50 ~ 2000 + hv007,
        hv007 < 100 ~ 1900 + hv007,
        TRUE ~ hv007  # already a full year
      )
    )
  
  # Get total number of households in the country at the time of the survey
  households <- pop_dat %>%
    filter(dhs_cde == country, Time == min(dhs_dat$hv007)) %>%
    pull(households)
  
  # Denormalise survey weights
  wt <- households/length(unique(dhs_dat$hhid))
  dhs_dat$weight <- (dhs_dat$hv005/1000000)*wt
  
  dhs_dat
}