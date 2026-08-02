
# Sensitivity analysis on clusters with exactly one DFO flood (10km) in 2000-2018

run_single_flood_analysis <- function() {

  dat <- readRDS("data/analysis_data/flood_56_age_10.rds")

  # Gather every (cluster, flood event, year) the data records across ages 0-14
  ages <- 0:14
  long <- purrr::map_dfr(ages, function(a) {
    tibble(
      clustid = dat$clustid,
      dfo_id  = dat[[paste0("dfo_id_age_", a)]],
      year    = dat[[paste0("year_age_", a)]],
      flooded = dat[[paste0("flooded_10km_age_", a)]]
    )
  })

  # Count distinct flood events per cluster within the study period
  n_events <- long %>%
    filter(flooded == 1, !is.na(dfo_id), year >= 2000, year <= 2018) %>%
    distinct(clustid, dfo_id) %>%
    count(clustid, name = "n_dfo")

  # Drop clusters with >1 event; keep single-flood and never-flooded clusters
  multi_clusters <- n_events$clustid[n_events$n_dfo > 1]
  dat_single <- filter(dat, !clustid %in% multi_clusters)
  single_clusters <- unique(dat_single$clustid)

  message(sprintf("Clusters with <=1 DFO event: %d of %d | rows: %d of %d",
                  length(single_clusters), n_distinct(dat$clustid),
                  nrow(dat_single), nrow(dat)))

  saveRDS(dat_single, "data/analysis_data/flood_56_age_10_single_flood.rds")

  # Headline model on the single-flood subset
  model <- feols(
    no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    data = dat_single, cluster = ~clustid, weights = ~weight
  )
  print(summary(model))

  results <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "flooded_10km_age_5_or_6") %>%
    mutate(data = "flood_56_age_10_single_flood",
           n_clusters = length(single_clusters),
           n_rows = nrow(dat_single))
  write.csv(results, "results/single_flood_results.csv", row.names = FALSE)
}
