
# Make table1: one descriptive table per main model, on its own estimation sample

make_tables <- function() {

  specs <- c("main", "main_primary", "main_attain")

  tables <- lapply(specs, make_model_table)
  names(tables) <- specs

  # Save the tables (one sheet per model)
  write_xlsx(tables, "results/table1.xlsx")

}

# Descriptive table for one model in model_specs, split by flood exposure
make_model_table <- function(spec_name) {

  params <- model_specs[[spec_name]]

  # Import the data
  dhs_dat <- readRDS(paste0("data/analysis_data/", params$data, ".rds"))

  # Restrict to the model's estimation sample so the table Ns match the regression
  mod <- feols(params$formula, data = dhs_dat, cluster = ~clustid,
               weights = params$weights)
  dhs_dat <- dhs_dat[obs(mod), ]

  # Summarise one group (count and percent for categorical variables)
  tab_stats <- function(d) {
    pct <- function(x) paste0(sum(x, na.rm = TRUE), " (",
                              round(mean(x, na.rm = TRUE) * 100, 1), "%)")
    d %>%
      summarise(
        n = as.character(n()),
        n_clusters = as.character(n_distinct(clustid)),
        mean_age = as.character(round(mean(hv105, na.rm = TRUE), 1)),
        mean_education_years = as.character(round(mean(hv108, na.rm = TRUE), 1)),

        Male   = pct(hv104 == "Male"),
        Female = pct(hv104 == "Female"),
        Urban  = pct(hv025 == "Urban"),
        Rural  = pct(hv025 == "Rural"),

        no_edu           = pct(hv109 == "No education"),
        primary_incomp   = pct(hv109 == "Incomplete primary"),
        primary          = pct(hv109 == "Complete Primary"),
        secondary_incomp = pct(hv109 == "Incomplete Secondary"),
        secondary        = pct(hv109 == "Complete Secondary"),
        higher           = pct(hv109 == "Higher"),

        poorest = pct(hv270 == "Poorest"),
        poorer  = pct(hv270 == "Poorer"),
        middle  = pct(hv270 == "Middle"),
        richer  = pct(hv270 == "Richer"),
        richest = pct(hv270 == "Richest")
      ) %>%
      pivot_longer(everything(), names_to = "variable")
  }

  # Clusters overlap the two groups, so the total is computed separately
  table <- tab_stats(filter(dhs_dat, flooded_10km_age_5_or_6 == 1)) %>%
    rename(Flooded = value) %>%
    left_join(rename(tab_stats(filter(dhs_dat, flooded_10km_age_5_or_6 == 0)),
                     `Not Flooded` = value), by = "variable") %>%
    left_join(rename(tab_stats(dhs_dat), Total = value), by = "variable") %>%
    mutate(variable = recode(variable,
                             n = "Sample Size",
                             n_clusters = "Number of Clusters",
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
    ))

  table
}
