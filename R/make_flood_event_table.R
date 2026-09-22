
# Summarise the distinct exposure floods in the main model sample by characteristic

make_flood_event_table <- function() {

  require_input("data/analysis_data/flood_56_age_10.rds", analysis_data_source)
  dat <- readRDS("data/analysis_data/flood_56_age_10.rds")

  # Number dead / affected come from FloodArchive, keyed on DFO id
  require_input("data/FloodArchive.xlsx", flood_archive_source)
  flood_arc <- read_xlsx("data/FloodArchive.xlsx") %>%
    mutate(flood_id = paste0("DFO_", ID)) %>%
    select(flood_id, Dead, Displaced)

  # One row per distinct flood: stack age-5 and age-6 exposures, then dedupe on id
  age5 <- dat %>%
    filter(flooded_10km_age_5 == 1) %>%
    transmute(flood_id = dfo_id_age_5, cause = dfo_main_cause_age_5,
              start = start_date_age_5, end = end_date_age_5)
  age6 <- dat %>%
    filter(flooded_10km_age_6 == 1) %>%
    transmute(flood_id = dfo_id_age_6, cause = dfo_main_cause_age_6,
              start = start_date_age_6, end = end_date_age_6)

  events <- bind_rows(age5, age6) %>%
    filter(!is.na(flood_id)) %>%
    group_by(flood_id) %>%
    summarise(cause = first(cause),
              start = as.Date(first(start)),
              end   = as.Date(first(end)),
              .groups = "drop") %>%
    mutate(
      # Match the model's derivations: storm cause and inclusive duration (days)
      cause = if_else(str_detect(cause, regex("tropical storm|cyclone|typhoon",
                                              ignore_case = TRUE)),
                      "Tropical Storm", "Other"),
      duration = as.integer(end - start + 1L),
      year = year(start)
    ) %>%
    left_join(flood_arc, by = "flood_id")

  # Categorise each event using the model's cut-points
  events <- events %>%
    mutate(
      dead_cat = factor(case_when(
        Dead < 100  ~ "<100",
        Dead <= 500 ~ "100-500",
        Dead > 500  ~ ">500"), levels = c("<100", "100-500", ">500")),
      affected_cat = factor(case_when(
        Displaced < 5000   ~ "<5000",
        Displaced <= 100000 ~ "5000-100000",
        Displaced > 100000 ~ ">100000"),
        levels = c("<5000", "5000-100000", ">100000")),
      duration_cat = factor(case_when(
        duration < 30 ~ "<1 month",
        duration < 90 ~ "1-3 months",
        duration >= 90 ~ "≥3 months"),
        levels = c("<1 month", "1-3 months", "≥3 months")),
      cause_cat = factor(cause, levels = c("Tropical Storm", "Other")),
      decade_cat = factor(paste0(floor(year / 10) * 10, "s"))
    )

  # Count events by category, with the percentage of all analysed events
  n_total <- nrow(events)
  count_by <- function(var, label) {
    events %>%
      count(Category = .data[[var]], name = "N_events") %>%
      mutate(Characteristic = label, Category = as.character(Category),
             Pct_events = round(100 * N_events / n_total, 1)) %>%
      select(Characteristic, Category, N_events, Pct_events)
  }

  tbl <- bind_rows(
    count_by("dead_cat",     "Number dead"),
    count_by("affected_cat", "Number affected"),
    count_by("duration_cat", "Flood duration"),
    count_by("cause_cat",    "Main cause"),
    count_by("decade_cat",   "Decade")
  ) %>%
    # Move any NA (missing FloodArchive value) to the end within each block
    arrange(factor(Characteristic,
                   levels = c("Number dead", "Number affected", "Flood duration",
                              "Main cause", "Decade")),
            is.na(Category))

  message(sprintf("Distinct flood events analysed: %d", nrow(events)))

  write_xlsx(tbl, "results/flood_event_characteristics.xlsx")
  tbl
}
