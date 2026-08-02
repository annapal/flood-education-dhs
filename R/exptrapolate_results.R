
exptrapolate_results <- function(dat_path) {

  # Get list of included countries
  countries <- read.csv("data/countries.csv")
  countries$iso3 <- countrycode(countries$dhs_cde, "dhs", "iso3c")
  
  # Get country boundaries
  world_all <- geodata::world(resolution = 1, path = "data")
  world <- world_all
  
  # Get data from world population prospects
  wpp_dat <- read.csv("data/population/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv") %>%
    filter(ISO3_code %in% world$GID_0, Time %in% 2000:2018, AgeGrp %in% 5:9)
  
  # Calculate proportion of 5-9 year olds in each country that are aged 5&6
  prop_5_6 <- wpp_dat %>%
    group_by(ISO3_code, Time) %>%
    summarise(
      total_male = sum(PopMale, na.rm = TRUE),
      total_female = sum(PopFemale, na.rm = TRUE),
      male_5_6 = sum(PopMale[AgeGrp %in% c(5, 6)], na.rm = TRUE),
      female_5_6 = sum(PopFemale[AgeGrp %in% c(5, 6)], na.rm = TRUE)
    ) %>%
    mutate(
      prop_male_5_6 = male_5_6 / total_male,
      prop_female_5_6 = female_5_6 / total_female
    )
  
  # Flood files
  f <- list.files(paste0(dat_path, "/gfd_v1_4"),
                  pattern = "\\.tif(f)?$",
                  full.names = TRUE,
                  ignore.case = TRUE,
                  recursive = TRUE)
  
  # Dataframe to store results
  all_dat <- data.frame()
  
  # Calculate number exposed to flood each year
  for (year in 2000:2018) {

    # Get flood data for a specific year
    f_files <- f[grepl(paste0("From_", year), f)]
    f_year <- mosaic(
      sprc(lapply(f_files, \(f) rast(f)[["flooded"]])),
      fun = "max")

    for (sex in c("f", "m")) {

      # Get proportions aged 5 & 6
      prop_year <- prop_5_6 %>% filter(Time==year)
      if (sex == "f") {
        prop_year <- prop_year %>%
          select(ISO3_code, prop_female_5_6)
      } else {
        prop_year <- prop_year %>%
          select(ISO3_code, prop_male_5_6)
      }
      colnames(prop_year) <- c("iso3", "prop")

      # Population of 5-9 year olds by country
      pop_dat <- rast(paste0(dat_path, "/worldpop/global_", sex, "_5_", year, "_1km.tif"))
      pop_10km_deg <- aggregate(pop_dat, fact = 10, fun = sum, na.rm = TRUE) # Convert to 10km grid cells
      world <- project(world, pop_10km_deg)
      pop_sub <- mask(pop_10km_deg, world)

      # Extract and sum population per polygon
      pop_by_country <- extract(pop_sub, world, fun = sum, na.rm = TRUE)
      pop_by_country$ID <- world$GID_0
      colnames(pop_by_country) <- c("iso3", "pop_sum")

      # Align grids
      f_year_res <- resample(f_year, pop_sub, method = "max")
      f_year_res[f_year_res != 1] <- NA

      # Calculate flooded population (5-9 year olds) in each country
      pop_on_flood <- mask(pop_sub, f_year_res)  # keep flooded cells
      pop_flood_by_country <- extract(pop_on_flood, world, fun = sum, na.rm = TRUE)
      pop_by_country[,paste0("pop_flood")] <- pop_flood_by_country[,2]

      # Calculate number of 5 & 6 year olds
      pop_by_country <- left_join(pop_by_country, prop_year)
      pop_by_country$pop_sum_age56 <- pop_by_country$pop_sum * pop_by_country$prop
      pop_by_country$pop_flood_age56 <- pop_by_country$pop_flood * pop_by_country$prop
      pop_by_country$sex <- sex
      pop_by_country$year <- year

      # Add to dataframe
      all_dat <- rbind(all_dat, pop_by_country)
      print(paste0(sex, " ", year))
      saveRDS(all_dat, "data/pop_exp_data.rds")
    }
    print(year)
  }
  
  # all_dat <- readRDS("data/pop_exp_data.rds") %>%
  #   select(-prop, -pop_sum_age56, -pop_flood_age56)
  # prop_5_6_long <- prop_5_6 %>%
  #   pivot_longer(
  #     cols = c(prop_male_5_6, prop_female_5_6),
  #     names_to = "sex",
  #     names_prefix = "prop_",
  #     values_to = "prop"
  #   ) %>%
  #   mutate(
  #     sex = ifelse(sex == "male_5_6", "m", "f")
  #   ) %>%
  #   select(ISO3_code, Time, sex, prop) %>%
  #   rename(
  #     iso3 = ISO3_code,
  #     year = Time
  #   )
  # all_dat <- left_join(all_dat, prop_5_6_long)
  # all_dat$pop_sum_age56 <- all_dat$pop_sum * all_dat$prop
  # all_dat$pop_flood_age56 <- all_dat$pop_flood * all_dat$prop
  
  # Data frame to store numbers
  flood_exp_dat <- data.frame(Description = NA, Estimate = NA)
  
  # TOTAL NUMBER OF 5-6 YEAR OLDS EXPOSED TO FLOOD
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Total number of 5-6 year olds exposed to flood",
                           sum(all_dat$pop_flood_age56, na.rm=TRUE)))
  
  # TOTAL NUMBER OF 5-6 YEAR OLDS EXPOSED TO FLOOD IN LMICS
  lmic_dat <- read_excel("data/population/CLASS_2025_10_07.xlsx") %>%
    filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
    select(Code)
  all_dat_lmic <- all_dat %>%
    filter(iso3 %in% lmic_dat$Code)
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Total number of 5-6 year olds exposed to flood in LMICS",
                           sum(all_dat_lmic$pop_flood_age56, na.rm=TRUE)))
  
  # TOTAL NUMBER OF 5-6 YEAR OLDS EXPOSED IN STUDY COUNTRIES
  all_dat_study <- all_dat %>%
    filter(iso3 %in% countries$iso3)
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Total number of 5-6 year olds exposed to flood in study countries",
                           sum(all_dat_study$pop_flood_age56, na.rm=TRUE)))
  
  # Number without education in study countries
  all_dat_study$eff <- ifelse(all_dat_study$sex=="f", 0.0035189067, 0.0010275443)
  all_dat_study$eff_lb <- ifelse(all_dat_study$sex=="f", 1.239951e-03, 0)
  all_dat_study$eff_ub <- ifelse(all_dat_study$sex=="f", 0.005797863, 0.003209912)
  all_dat_study$flood_aff <- all_dat_study$pop_flood_age56*all_dat_study$eff
  all_dat_study$flood_aff_lb <- all_dat_study$pop_flood_age56*all_dat_study$eff_lb
  all_dat_study$flood_aff_ub <- all_dat_study$pop_flood_age56*all_dat_study$eff_ub
  
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Number without education (study countries)",
                           sum(all_dat_study$flood_aff, na.rm=TRUE)))
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Number without education (study countries) LB",
                           sum(all_dat_study$flood_aff_lb, na.rm=TRUE)))
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Number without education (study countries) UB",
                           sum(all_dat_study$flood_aff_ub, na.rm=TRUE)))
  
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Number of females without education (study countries)",
                           sum(all_dat_study[all_dat_study$sex=="f",]$flood_aff, na.rm=TRUE)))
  flood_exp_dat <- rbind(flood_exp_dat, 
                         c("Number of males without education (study countries)",
                           sum(all_dat_study[all_dat_study$sex=="m",]$flood_aff, na.rm=TRUE)))
  write.csv(flood_exp_dat, "results/exptrapolation_results.csv", row.names = FALSE)
  
  # Get number of events over time
  f <- list.files(paste0(dat_path, "/gfd_v1_4"),
                  pattern = "\\.json(f)?$",
                  full.names = TRUE,
                  ignore.case = TRUE,
                  recursive = TRUE)
  df <- map_dfr(f, function(x) {
    dat <- fromJSON(x, flatten = TRUE)
    dat$.file <- x
    dat
  })
  iso_ok <- countries %>% dplyr::pull(iso3) %>% as.character() %>% unique()
  df <- df %>%
    mutate(
      cc = str_remove_all(cc, "\\[|\\]|'"),
      cc = str_split(cc, ",\\s*")
    ) %>%
    unnest(cc) %>%
    mutate(cc = str_trim(cc)) %>%
    filter(cc %in% iso_ok)
  df_yearly <- df %>%
    mutate(year = year(as.Date(began))) %>%    # extract year from 'began'
    group_by(year) %>%
    summarise(n_unique_ids = n_distinct(id)) %>%
    arrange(year)
  
  # Plot the results
  all_dat_plot <- all_dat_study %>%
    group_by(sex, year) %>%
    summarise(flood_aff_sum = sum(flood_aff, na.rm = TRUE), .groups = "drop") %>%
    mutate(sex = recode(sex, f = "Female", m = "Male"))
  
  p2 <- ggplot(df_yearly, aes(x = year, y = n_unique_ids)) +
    geom_line(color = "navyblue", linewidth = 1) +
    geom_point(color = "navyblue", size = 2) +
    labs(
      x = "Year",
      y = "No. Flood Events",
      title = "A"
    ) +
    ylim(0,60) +
    theme_clean(base_size = 14) +
    theme(plot.background=element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  p1 <- ggplot(all_dat_plot, aes(x = year, y = flood_aff_sum, fill = sex)) +
    geom_col(position = "stack") +
    labs(x = "Year",
         y = "No. without schooling due to flood",
         fill = "Sex",
         title = "B") +
    theme_clean(base_size = 14) +
    theme(legend.position = "top") +
    theme(plot.background=element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  plot_grid(p2, p1, ncol = 1, align = "v", rel_heights = c(1, 2))
  ggsave("figures/fig3.jpeg", height = 7, width = 7)
}
