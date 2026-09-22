
# Create a pooled dataset for each country

merge_country_data <- function(files) {
  
  # Get DHS file names
  m_files <- list.files("data/merged_dhs/", full.names = FALSE)
  require_found(m_files, ".rds files", "data/merged_dhs/",
                "Produced by merge_geocodes() earlier in _run.R")

  # Read in all the flood data
  f_files <- list.files("data/flood_clusters/", full.names = TRUE)
  require_found(f_files, ".rds files", "data/flood_clusters/",
                "Produced by process_flood_tifs(); this step is commented out in _run.R and must be run once")
  flood_data <- f_files %>%
    lapply(readRDS) %>%
    bind_rows() %>%
    select(DHSCLUST, source_file,
           flooded_2km, flooded_5km, flooded_10km, flooded_20km, flooded_50km,
           flooded_100km, flooded_150km, flooded_200km,
           start_date, end_date, dfo_main_cause, dfo_id)
  
  # Read in flood archive for additional variables
  require_input("data/FloodArchive.xlsx", flood_archive_source)
  flood_arc <- read_xlsx("data/FloodArchive.xlsx")
  flood_arc$flood_id <- paste("DFO_", flood_arc$ID, sep="")
  
  # Add clean data name and dhs code to reference data
  files <- files %>%
    filter(!is.na(geo_file))
  files$clean_file <- paste(sub(".*dhs/([^/]+)/.*", "\\1", files$dta_file), ".rds", sep="")
  files$dhs_cde <- substr(files$clean_file, 1, 2)
  
  # Get population data for denormalising survey weights
  pop_dat <- calculate_no_households()
  
  # All countries included in the analysis
  # countries <- unique(files$dhs_cde)
  countries <- read.csv("data/countries.csv")$dhs_cde
  
  # Make dataframe to store the dataframes
  dhs_dat_combined <- data.frame()
  
  for (country in countries) {
  
    # Get all files associated with that country
    country_files <- files %>%
      filter(dhs_cde==country) %>%
      pull(clean_file)
    
    # Empty list to store the datasets for that country
    country_list <- list()
    
    for (i in 1:length(country_files)) {
      
      surv <- country_files[i]
      dhs_dat <- readRDS(paste0("data/merged_dhs/", surv))
      
      # Get the flood data, matched on basename (flood paths are relative)
      s_file <- files$geo_file[which(files$clean_file==surv)]
      fl_dat <- flood_data %>%
        filter(basename(source_file)==basename(s_file))
      fl_dat <- tidy_flood_dat(fl_dat)
      
      # Complete the flood data so that there is an entry for each year between 2000-2018
      years <- 2000:2018
      flood_cols <- names(fl_dat) %>% str_subset("^flooded_")
      clusters <- sort(unique(dhs_dat$hv001))
      fl_dat <- fl_dat %>%
        ungroup() %>%
        complete(DHSCLUST = clusters, year = years) %>%       # add missing clusters + years
        mutate(across(all_of(flood_cols), ~ tidyr::replace_na(., 0L))) %>%
        arrange(DHSCLUST, year)
      
      # Merge the flood data & clean variables
      dhs_dat <- merge_and_clean(dhs_dat, fl_dat, pop_dat, country)
      
      # Add flood counts within each cluster
      fl_dat_counts <- fl_dat %>%
        group_by(DHSCLUST) %>%
        summarise(
          across(
            starts_with("flooded"),
            ~ sum(.x, na.rm = TRUE),
            .names = "{.col}_count"
          ),
          .groups = "drop"
        ) %>%
        rename(hv001 = DHSCLUST)
      dhs_dat <- left_join(dhs_dat, fl_dat_counts)
      
      # Add survey data to the list
      country_list[[surv]] <- dhs_dat
    }
    
    # Merge datasets together
    comb_dat <- bind_rows(country_list)
    
    # Create and clean some additional variables
    comb_dat <- comb_dat %>%
      mutate(
        # Create a variable for country id
        country_id = country,
        
        # Create a variable for estimated birth year (hv007 - hv105)
        birth_year = hv007 - hv105,
        
        # Create a unique country-year identifier based on birth year 
        country_year = paste0(country, "_", birth_year),
        
        # Create binary variable: 1 = No education, 0 = Any education, NA = unknown
        no_education = if_else(hv109 == "No education", 1, if_else(is.na(hv109), NA_real_, 0)),
        
        # Create binary variable: 1 = No primary, 0 = Primary+, NA = unknown
        no_primary = if_else((hv109 == "No education")|(hv109 == "Incomplete primary"), 
                               1, if_else(is.na(hv109), NA_real_, 0)),
        
        # Create binary variable for education of household head
        no_education_hh_head = if_else(hh_edu_attain == "No education", 1, 
                                       if_else(is.na(hh_edu_attain), NA_real_, 0))
      )
    
    # Remove observations with 0 weight
    comb_dat <- comb_dat %>% filter(weight!=0)
    
    # Make sure that each cluster has the same rural-urban status
    comb_dat <- comb_dat %>%
      group_by(clustid) %>%
      mutate(
        # Count Urban and Rural in the cluster
        n_rural = sum(hv025 == "Rural"),
        n_urban = sum(hv025 == "Urban"),
        
        # Set to the majority value in the cluster
        hv025_majority = if_else(n_rural >= n_urban, "Rural", "Urban")
      ) %>%
      # Replace the original hv025 with the majority version
      mutate(hv025 = hv025_majority) %>%
      select(-n_rural, -n_urban, -hv025_majority) %>%
      ungroup()
    
    # Add baseline schooling rate in each cluster
    comb_dat <- comb_dat %>%
      group_by(clustid) %>%
      mutate(
        prop_no_education = mean(no_education, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Make variable for cluster-sex and country_year-sex
    comb_dat <- comb_dat %>%
      mutate(
        clustid_sex = paste(clustid, hv104),
        country_year_sex = paste(country_year, hv104)
      )
    
    # Add the processed data to the list
    dhs_dat_combined <- rbind(dhs_dat_combined, comb_dat)
    print(country)
  }
  
  # Combine all dataframes in the list into one
  # dhs_dat_combined <- bind_rows(dat_list)
  
  # Create a binary variable for exposure at ages 12 and 13
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flooded_10km_age_12_or_13 = if_else(
        flooded_10km_age_12 == 1 | flooded_10km_age_13 == 1, 1, 0)
    )
  
  # Create a binary variable for exposure at ages 5 and 6
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flooded_2km_age_5_or_6 = if_else(
        flooded_2km_age_5 == 1 | flooded_2km_age_6 == 1, 1, 0),
      flooded_5km_age_5_or_6 = if_else(
        flooded_5km_age_5 == 1 | flooded_5km_age_6 == 1, 1, 0),
      flooded_10km_age_5_or_6 = if_else(
        flooded_10km_age_5 == 1 | flooded_10km_age_6 == 1, 1, 0),
      flooded_20km_age_5_or_6 = if_else(
        flooded_20km_age_5 == 1 | flooded_20km_age_6 == 1, 1, 0),
      flooded_50km_age_5_or_6 = if_else(
        flooded_50km_age_5 == 1 | flooded_50km_age_6 == 1, 1, 0)
    )
  
  # Add flood cause
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flood_cause_10km_age_5_or_6 = case_when(
        flooded_10km_age_5_or_6 == 1 ~
          case_when(
            !is.na(dfo_main_cause_age_6) ~ dfo_main_cause_age_6,
            !is.na(dfo_main_cause_age_5) ~ dfo_main_cause_age_5,
            TRUE ~ NA_character_ 
          ),
        TRUE ~ NA_character_ 
      )
    )
  
  # Convert cause to TC related or not
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flood_storm_cause = case_when(
        is.na(flood_cause_10km_age_5_or_6) ~ "No Flood",
        str_detect(flood_cause_10km_age_5_or_6, 
                   regex("tropical storm|cyclone|typhoon", ignore_case = TRUE)) ~ "Tropical Storm",
        TRUE ~ "Other"
      )
    )
  
  # Add length of flood
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      # Parse dates
      start6 = as.Date(start_date_age_6, format = "%Y-%m-%d"),
      end6   = as.Date(end_date_age_6,   format = "%Y-%m-%d"),
      start5 = as.Date(start_date_age_5, format = "%Y-%m-%d"),
      end5   = as.Date(end_date_age_5,   format = "%Y-%m-%d"),
      
      # Inclusive durations
      duration_age_6 = if_else(!is.na(start6) & !is.na(end6),
                               as.integer(end6 - start6 + 1L), NA_integer_),
      duration_age_5 = if_else(!is.na(start5) & !is.na(end5),
                               as.integer(end5 - start5 + 1L), NA_integer_),
      
      # Max duration if flooded == 1, NA otherwise
      flood_duration_days_10km_age_5_or_6 = if_else(
        flooded_10km_age_5_or_6 == 1,
        if_else(is.na(duration_age_6) & is.na(duration_age_5),
                NA_integer_,
                pmax(duration_age_6, duration_age_5, na.rm = TRUE)),
        NA_integer_
      )
    ) %>%
    # Remove helpers
    select(-start6, -end6, -start5, -end5, -duration_age_6, -duration_age_5)
  
  # Add categories
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flood_duration_cat = case_when(
        is.na(flood_duration_days_10km_age_5_or_6) ~ "not flooded",
        flood_duration_days_10km_age_5_or_6 < 30 ~ "<1 month",
        flood_duration_days_10km_age_5_or_6 < 90 ~ "1–3 months",
        TRUE ~ "≥3 months"
      )
    )
  
  # Add flood severity
  dhs_dat_combined <- dhs_dat_combined %>%
    left_join(
      flood_arc %>% select(flood_id, Severity),
      by = c("dfo_id_age_5" = "flood_id")
    ) %>%
    rename(dfo_severity_age_5 = Severity)
  
  dhs_dat_combined <- dhs_dat_combined %>%
    left_join(
      flood_arc %>% select(flood_id, Severity),
      by = c("dfo_id_age_6" = "flood_id")
    ) %>%
    rename(dfo_severity_age_6 = Severity)
  
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      # Max severity if flooded == 1, NA otherwise
      dfo_severity_age_5_or_6 = if_else(
        flooded_10km_age_5_or_6 == 1,
        if_else(is.na(dfo_severity_age_5) & is.na(dfo_severity_age_6),
                NA_integer_,
                pmax(dfo_severity_age_5, dfo_severity_age_6, na.rm = TRUE)),
        0
      )
    )
  
  # Add geographic region
  dhs_dat_combined$region <- countrycode(dhs_dat_combined$country_id, "dhs", "region")
  
  # Add high vs low flood exposure categories
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flood_baseline_cat = case_when(
        flooded_10km_count < 5 ~ "less frequent",
        TRUE ~ "more frequent"
      )
    )
  
  # Add wealth quintile cat
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      wealth_cat = case_when(
        hv270_numeric <= 1 ~ "poorest quintile",
        TRUE ~ "not poorest"
      )
    )
  
  # Calculate distance from exposure
  dhs_dat_combined <- dhs_dat_combined %>%
    mutate(
      flooded_dist = case_when(
        flooded_2km_age_5 == 1 | flooded_2km_age_6 == 1 ~ "less than 2km",
        flooded_5km_age_5 == 1 | flooded_5km_age_6 == 1 ~ "2-5km",
        flooded_10km_age_5 == 1 | flooded_10km_age_6 == 1 ~ "5-10km",
        TRUE ~ "0"
      ),
      flooded_00km_age_5_or_6 = ifelse(flooded_dist=="0", 0, 1)
    )
  
  # Save the combined data
  saveRDS(dhs_dat_combined, "data/dhs_dat_combined.rds")
}
