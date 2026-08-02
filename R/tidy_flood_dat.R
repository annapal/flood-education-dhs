
# Tidy the flood data before it is merged

tidy_flood_dat <- function(fl_dat) {
  
  # Remove geometry and source file name
  fl_dat <- st_drop_geometry(fl_dat) %>%
    select(-source_file)
  
  # Calculate year of the flood events
  fl_dat <- fl_dat %>%
    # Convert strings to Date
    mutate(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date)
    )
  
  if (nrow(fl_dat)>0) {
    # Get sequence of years per row
    fl_dat <- fl_dat %>%
      rowwise() %>%
      mutate(year = list(seq(year(start_date), year(end_date)))) %>%
      unnest(year) %>%
      ungroup()
    
    # If two floods affected the same cluster and year
    fl_dat <- fl_dat %>%
      # Calculate the total number of flooded zones (2km to 200km)
      mutate(
        flood_exposure_score = flooded_2km + flooded_5km + flooded_10km + 
          flooded_20km + flooded_50km + flooded_100km + flooded_150km + flooded_200km,
        flood_duration = as.integer(end_date - start_date)
      ) %>%
      # Group by cluster and year to compare flood events within the same time/location
      group_by(DHSCLUST, year) %>%
      # Keep rows with the highest number of flooded zones
      slice_max(order_by = flood_exposure_score, with_ties = TRUE) %>%
      # If tied, keep the row with the longest duration
      slice_max(order_by = flood_duration, with_ties = FALSE) %>%
      # Return to an ungrouped dataframe
      ungroup() %>%
      select(-flood_exposure_score, -flood_duration)
  } else {
    fl_dat <- fl_dat %>%
      mutate(year = NA_real_)
  }
  
  fl_dat
}

