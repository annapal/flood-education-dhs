
make_analysis_datasets <- function(dat_path = "~/data") {
  
  # Read in the data
  require_input("data/dhs_dat_combined.rds",
                "Produced by merge_country_data() earlier in _run.R")
  dhs_dat_combined <- readRDS("data/dhs_dat_combined.rds")
  if (inherits(dhs_dat_combined, "sf")) {
    dhs_dat_combined <- sf::st_drop_geometry(dhs_dat_combined)
  }

  # Merge mother's years-in-residence (v104) and education (v106)
  xwalk <- match_geo_df(dat_path) %>%
    filter(!is.na(ir_file)) %>%
    transmute(dta_name = sub(".*dhs/([^/]+)/.*", "\\1", dta_file),
              source_file = path.expand(ir_file))
  require_input("data/ir_mr_extract.rds",
                "Produced by make_ir_extract() earlier in _run.R")
  ir_mr <- readRDS("data/ir_mr_extract.rds") %>%
    filter(!is.na(v001), !is.na(v002), !is.na(v003))
  mother_ir <- ir_mr %>%
    filter(sex == "Female") %>%
    transmute(source_file, v001, v002, v003,
              mother_v104 = v104, mother_v106 = v106)
  
  # Household head: linked by hv218 (head's line number)
  head_ir <- ir_mr %>%
    transmute(source_file, v001, v002, v003,
              head_v104 = v104, head_v106 = v106)
  dhs_dat_combined <- dhs_dat_combined %>%
    left_join(xwalk, by = "dta_name") %>%
    left_join(mother_ir,
              by = c("source_file", "hv001" = "v001",
                     "hv002" = "v002", "hv112" = "v003")) %>%
    left_join(head_ir,
              by = c("source_file", "hv001" = "v001",
                     "hv002" = "v002", "hv218" = "v003")) %>%
    select(-source_file)

  # Create output directory if it doesn't exist
  dir_path <- "data/analysis_data"
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Filter included countries
  countries <- read.csv("data/countries.csv")
  dhs_dat_combined <- dhs_dat_combined %>%
    filter(country_id %in% countries$dhs_cde)
  
  # Add no. years primary attained
  dhs_dat_combined$years_primary <- ifelse(
    dhs_dat_combined$no_primary == 0, 
    7, 
    ifelse(dhs_dat_combined$hv124 > 7, 7, dhs_dat_combined$hv124)
  )
  
  # Subset to those who were 5-6 between 2000 & 2018 and greater than 10 years old
  dhs_dat_combined2 <- dhs_dat_combined %>%
    filter(year_age_5 >=2000, year_age_6 <= 2018, hv105 > 9)
  dhs_dat_combined2 <- add_country_weights(dhs_dat_combined2) # Add weights
  
  saveRDS(dhs_dat_combined2, "data/analysis_data/flood_56_age_10.rds")

  # Subset to windows starting at each age 5 down to 0 and ending at age 6 or 7
  windows <- expand.grid(lo = 5:0, hi = c(6, 7))
  for (w in seq_len(nrow(windows))) {
    lo <- windows$lo[w]; hi <- windows$hi[w]
    yr_lo <- paste0("year_age_", lo)
    yr_hi <- paste0("year_age_", hi)
    fl_cols <- paste0("flooded_10km_age_", lo:hi)

    dhs_dat_combined2 <- dhs_dat_combined %>%
      filter(.data[[yr_lo]] >= 2000, .data[[yr_hi]] <= 2018, hv105 > 9) %>%
      mutate(flooded_10km_age_window = if_else(
        rowSums(across(all_of(fl_cols)) == 1, na.rm = TRUE) > 0, 1, 0))
    dhs_dat_combined2 <- add_country_weights(dhs_dat_combined2) # Add weights

    saveRDS(dhs_dat_combined2,
            paste0("data/analysis_data/flood_", lo, hi, "_age_10.rds"))
  }
  
  # Subset to those who were 5-6 between 2000 & 2018 and greater than 12 years old
  dhs_dat_combined2 <- dhs_dat_combined %>%
    filter(year_age_5 >=2000, year_age_6 <= 2018, hv105 > 11)
  dhs_dat_combined2 <- add_country_weights(dhs_dat_combined2) # Add weights
  
  saveRDS(dhs_dat_combined2, "data/analysis_data/flood_56_age_12.rds")
  
  # Subset to those who were 5-6 between 2000 & 2018 and greater than 15 years old
  dhs_dat_combined2 <- dhs_dat_combined %>%
    filter(year_age_5 >=2000, year_age_6 <= 2018, hv105 > 14)
  dhs_dat_combined2 <- add_country_weights(dhs_dat_combined2) # Add weights
  
  saveRDS(dhs_dat_combined2, "data/analysis_data/flood_56_age_15.rds")
  
  # Subset to those who were 12-13 between 2000 & 2018 and greater than 15 years old
  dhs_dat_combined2 <- dhs_dat_combined %>%
    filter(year_age_12 >=2000, year_age_13 <= 2018, hv105 > 14)
  dhs_dat_combined2 <- add_country_weights(dhs_dat_combined2) # Add weights
  
  saveRDS(dhs_dat_combined2, "data/analysis_data/flood_1213_age_15.rds")
  
  # Remove clusters that are 100% flooded
  dhs_dat_combined2 <- dhs_dat_combined %>%
    filter(year_age_5 >=2000, year_age_6 <= 2018, hv105 > 9) %>%
    group_by(clustid) %>%
    mutate(
      prop_flooded = mean(flooded_10km_age_5_or_6, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(prop_flooded!=1)
  dhs_dat_combined2 <- add_country_weights(dhs_dat_combined2) # Add weights
  
  saveRDS(dhs_dat_combined2, "data/analysis_data/flood_56_age_10_flood_rm.rds")
}
