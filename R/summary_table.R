
# Make summary table for data in each country

summary_table <- function(dhs_dat_subs, country, flood_var) {
  
  # Create and format summary table
  table <- dhs_dat_subs %>%
    group_by(!!sym(flood_var)) %>%
    summarise(
      n = n(),
      mean_age = round(mean(hv105, na.rm = TRUE), 1),
      mean_education_years = round(mean(hv108, na.rm = TRUE), 1),
      
      male_count = sum(hv104 == "Male", na.rm = TRUE),
      male_pct   = round(mean(hv104 == "Male", na.rm = TRUE) * 100, 1),
      female_count = sum(hv104 == "Female", na.rm = TRUE),
      female_pct   = round(mean(hv104 == "Female", na.rm = TRUE) * 100, 1),
      
      urban_count = sum(hv025 == "Urban", na.rm = TRUE),
      urban_pct   = round(mean(hv025 == "Urban", na.rm = TRUE) * 100, 1),
      rural_count = sum(hv025 == "Rural", na.rm = TRUE),
      rural_pct   = round(mean(hv025 == "Rural", na.rm = TRUE) * 100, 1),
      
      no_edu = paste0(sum(hv109 == "No education", na.rm = TRUE), " (", 
                      round(mean(hv109 == "No education", na.rm = TRUE) * 100, 1), "%)"),
      primary_incomp = paste0(sum(hv109 == "Incomplete primary", na.rm = TRUE), " (", 
                              round(mean(hv109 == "Incomplete primary", na.rm = TRUE) * 100, 1), "%)"),
      primary = paste0(sum(hv109 == "Complete Primary", na.rm = TRUE), " (", 
                       round(mean(hv109 == "Complete Primary", na.rm = TRUE) * 100, 1), "%)"),
      secondary_incomp = paste0(sum(hv109 == "Incomplete Secondary", na.rm = TRUE), " (", 
                                round(mean(hv109 == "Incomplete Secondary", na.rm = TRUE) * 100, 1), "%)"),
      secondary = paste0(sum(hv109 == "Complete Secondary", na.rm = TRUE), " (", 
                         round(mean(hv109 == "Complete Secondary", na.rm = TRUE) * 100, 1), "%)"),
      higher = paste0(sum(hv109 == "Higher", na.rm = TRUE), " (", 
                      round(mean(hv109 == "Higher", na.rm = TRUE) * 100, 1), "%)"),
      
      poorest = paste0(sum(hv270 == "Poorest", na.rm = TRUE), " (", 
                       round(mean(hv270 == "Poorest", na.rm = TRUE) * 100, 1), "%)"),
      poorer = paste0(sum(hv270 == "Poorer", na.rm = TRUE), " (", 
                      round(mean(hv270 == "Poorer", na.rm = TRUE) * 100, 1), "%)"),
      middle = paste0(sum(hv270 == "Middle", na.rm = TRUE), " (", 
                      round(mean(hv270 == "Middle", na.rm = TRUE) * 100, 1), "%)"),
      richer = paste0(sum(hv270 == "Richer", na.rm = TRUE), " (", 
                      round(mean(hv270 == "Richer", na.rm = TRUE) * 100, 1), "%)"),
      richest = paste0(sum(hv270 == "Richest", na.rm = TRUE), " (", 
                       round(mean(hv270 == "Richest", na.rm = TRUE) * 100, 1), "%)")
    ) %>%
    mutate(
      Male   = paste0(male_count, " (", male_pct, "%)"),
      Female = paste0(female_count, " (", female_pct, "%)"),
      Urban  = paste0(urban_count, " (", urban_pct, "%)"),
      Rural  = paste0(rural_count, " (", rural_pct, "%)"),
      flood_status = ifelse(!!sym(flood_var) == 1, "Flooded", "Not Flooded")
    ) %>%
    select(
      flood_status, n, mean_age, mean_education_years,
      Male, Female, Urban, Rural,
      no_edu, primary_incomp, primary,
      secondary_incomp, secondary, higher,
      poorest, poorer, middle, richer, richest
    ) %>%
    mutate(across(-flood_status, as.character)) %>%
    pivot_longer(-flood_status, names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = flood_status, values_from = value) %>%
    mutate(variable = recode(variable,
                             n = "Sample Size",
                             mean_age = "Mean Age",
                             mean_education_years = "Mean Years of Education",
                             Male = "Male",
                             Female = "Female",
                             Urban = "Urban",
                             Rural = "Rural",
                             no_edu = "No Education",
                             primary_incomp = "Incomplete primary",
                             primary = "Completed Primary",
                             secondary_incomp = "Incomplete secondary",
                             secondary = "Completed Secondary",
                             higher = "Higher Education",
                             poorest = "Poorest Quintile",
                             poorer  = "Poorer Quintile",
                             middle  = "Middle Quintile",
                             richer  = "Richer Quintile",
                             richest = "Richest Quintile"
    ))  %>%
    gt(rowname_col = "variable") %>%
    tab_header(
      title = "Summary of Characteristics by Flood Exposure",
      subtitle = countrycode(country, "dhs", "country.name.en")
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_row_group(label = "Wealth Quintile", rows = c(
      "Poorest Quintile", "Poorer Quintile", "Middle Quintile",
      "Richer Quintile", "Richest Quintile")) %>%
    tab_row_group(label = "Education", rows = c(
      "No Education", "Incomplete primary", "Completed Primary",
      "Incomplete secondary", "Completed Secondary",
      "Higher Education"
    )) %>%
    tab_row_group(label = "Residence", rows = c("Urban", "Rural")) %>%
    tab_row_group(label = "Sex", rows = c("Male", "Female")) %>%
    tab_row_group(label = "Summary", rows = c("Sample Size", "Mean Age", "Mean Years of Education")) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    )
  
  table
}
