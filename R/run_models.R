
# Function that runs all the models listed in model_specs

run_models <- function(model_specs) {

  # Make list of scenarios by dataset that they use
  dat <- vapply(model_specs, function(x) as.character(x$data)[1], character(1L))
  unique_data <- unique(dat)
  by_data <- split(names(dat), dat)
  
  # Dataset to store the results
  results <- data.frame()
  
  # Load analysis dataset one at a time, then perform all relevant analyses
  for (datset in names(by_data)) {
    
    # Load the dataset
    dhs_dat <- readRDS(paste0("data/analysis_data/", datset, ".rds"))
    
    # Calculate baseline schooling rate
    dhs_dat <- dhs_dat %>%
      group_by(clustid) %>%
      mutate(
        prop_educated = 1-mean(no_education, na.rm = TRUE)
      ) %>%
      ungroup()
    dhs_dat <- dhs_dat %>%
      mutate(
        school_baseline_cat = ntile(prop_educated, 3),
        school_baseline_cat = case_when(
          school_baseline_cat == 1 ~ "low",
          school_baseline_cat == 2 ~ "medium",
          school_baseline_cat == 3 ~ "high",
        )
      )

    # Heterogeneity moderators: household head education and decade turned 5
    dhs_dat <- dhs_dat %>%
      mutate(
        hh_head_educ_cat = factor(
          case_when(
            hh_edu_attain %in% c("No education", "Incomplete primary") ~ "Less than primary",
            hh_edu_attain %in% c("Complete Primary", "Incomplete Secondary",
                                 "Complete Secondary", "Higher") ~ "Primary or higher"
          ),
          levels = c("Less than primary", "Primary or higher")
        ),
        decade_cat = if_else(year_age_5 < 2010, "2000s", "2010s")
      )

    # Global region (23-region scheme), and a combined region-by-cause factor
    dhs_dat <- dhs_dat %>%
      mutate(
        region23_cat = countrycode(country_id, "dhs", "region23"),
        region_cause_cat = if_else(
          is.na(region23_cat) | is.na(flood_storm_cause),
          NA_character_,
          paste(region23_cat, flood_storm_cause, sep = " | ")
        )
      )

    # Flood impacts for the exposure flood: prefer age-6, fall back to age-5
    flood_arc <- read_xlsx("data/FloodArchive.xlsx") %>%
      mutate(flood_id = paste0("DFO_", ID)) %>%
      select(flood_id, Dead, Displaced)

    dhs_dat <- dhs_dat %>%
      left_join(rename(flood_arc, dead_age_5 = Dead, displaced_age_5 = Displaced),
                by = c("dfo_id_age_5" = "flood_id")) %>%
      left_join(rename(flood_arc, dead_age_6 = Dead, displaced_age_6 = Displaced),
                by = c("dfo_id_age_6" = "flood_id")) %>%
      mutate(
        flood_dead = if_else(flooded_10km_age_5_or_6 == 1,
                             coalesce(dead_age_6, dead_age_5), NA_real_),
        flood_displaced = if_else(flooded_10km_age_5_or_6 == 1,
                                  coalesce(displaced_age_6, displaced_age_5), NA_real_)
      )

    # Impact categories; unexposed are the reference, missing impacts stay NA
    dhs_dat <- dhs_dat %>%
      mutate(
        flood_dead_cat = case_when(
          flooded_10km_age_5_or_6 == 0 ~ "not flooded",
          flood_dead < 100 ~ "<100",
          flood_dead <= 500 ~ "100-500",
          flood_dead > 500 ~ ">500"
        ),
        flood_dead_cat = factor(flood_dead_cat,
                                levels = c("not flooded", "<100", "100-500", ">500")),
        flood_displaced_cat = case_when(
          flooded_10km_age_5_or_6 == 0 ~ "not flooded",
          flood_displaced < 5000 ~ "<5000",
          flood_displaced <= 100000 ~ "5000-100000",
          flood_displaced > 100000 ~ ">100000"
        ),
        flood_displaced_cat = factor(flood_displaced_cat,
                                     levels = c("not flooded", "<5000", "5000-100000", ">100000"))
      )

    # ERA5 temperature over each child's exposure window (year_age_5 to year_age_6)
    annual_era5 <- readRDS(path.expand("~/data/era5_cluster_year.rds"))

    # One row per cluster per two-year window
    clusters <- dhs_dat %>%
      select(clustid) %>%
      distinct(clustid)

    windows <- tibble(year_start = 2000:2017) %>%
      mutate(year_end = year_start + 1)

    cluster_years <- tidyr::crossing(clusters, windows) %>%
      arrange(clustid, year_start)

    # Window mean is the average of the two annual means (12 months each, so equally weighted)
    cluster_years <- cluster_years %>%
      left_join(rename(annual_era5, temp_start = temp_c),
                by = c("clustid", "year_start" = "year")) %>%
      left_join(rename(annual_era5, temp_end = temp_c),
                by = c("clustid", "year_end" = "year")) %>%
      mutate(mean_temp = (temp_start + temp_end) / 2) %>%
      select(-temp_start, -temp_end)

    # Standardise each window against the cluster's own 2000-2018 distribution
    cluster_years <- cluster_years %>%
      group_by(clustid) %>%
      mutate(temp_z = (mean_temp - mean(mean_temp, na.rm = TRUE)) / sd(mean_temp, na.rm = TRUE)) %>%
      ungroup()

    dhs_dat <- dhs_dat %>%
      left_join(select(cluster_years, clustid, year_age_5 = year_start, mean_temp, temp_z),
                by = c("clustid", "year_age_5"))

    # Run the scenarios
    for (scen in by_data[[datset]]) {
      params <- model_specs[[scen]]
      
      # Run model
      model <- feols(params$formula, data = dhs_dat, cluster = ~clustid, weights = params$weights)
      
      # Get results
      coefs <- broom::tidy(model, conf.int = TRUE)
      flooded_coefs <- coefs %>%
        filter(str_detect(term, "flooded_\\d+(\\.\\d+)?km_age_\\d+(_or_\\d+)*$")) %>%
        mutate(
          level = sub(".*::(.*):flooded_\\d+(\\.\\d+)?km_age_\\d+(_or_\\d+)*$", "\\1", term),
          scenario = scen,
          data = datset
        )
      
      # Save the results
      results <- rbind(results, flooded_coefs)
    }
  }
  
  write.csv(results, "results/results.csv")
}
