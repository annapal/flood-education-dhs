# Spillover check: control group composition by buffer, then the 10-20km band

test_flooded_dist2 <- function() {

  dhs_dat <- readRDS("data/analysis_data/flood_56_age_10.rds")

  buffers <- c(2, 5, 10, 20, 50, 100, 150, 200)

  # Rows the model uses
  est_dat <- dhs_dat %>%
    filter(!is.na(no_education), !is.na(hv104), !is.na(hv270_numeric))

  # Unexposed at each buffer, from the per-age columns
  unexp_dat <- est_dat %>%
    mutate(across(all_of(paste0("flooded_", buffers, "km_age_5")),
                  ~ as.integer(!(.x == 1 | get(sub("_age_5$", "_age_6", cur_column())) == 1)),
                  .names = "unexp_{.col}"))

  # Share of each cluster's children who are unexposed
  clust_share <- unexp_dat %>%
    group_by(clustid) %>%
    summarise(across(starts_with("unexp_flooded_"), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(starts_with("unexp_flooded_"), names_to = "buffer", values_to = "share_unexposed") %>%
    mutate(buffer = factor(as.integer(str_extract(buffer, "\\d+")),
                           levels = buffers, labels = paste0("Beyond ", buffers, "km")))

  control_tab <- clust_share %>%
    group_by(buffer) %>%
    summarise(
      clusters = n(),
      clusters_with_no_controls = sum(share_unexposed == 0, na.rm = TRUE),
      pct_clusters_with_no_controls = 100 * mean(share_unexposed == 0, na.rm = TRUE),
      .groups = "drop"
    )
  print(control_tab)
  write.csv(control_tab, "results/control_group_by_buffer.csv", row.names = FALSE)


  # Effect at 0-10km and 10-20km, against controls beyond 20km

  band_levels <- c("less than 10km", "10-20km")
  band_labels <- c("0-10km", "10-20km")

  dhs_dat <- dhs_dat %>%
    mutate(
      flooded_dist20 = case_when(
        flooded_10km_age_5 == 1 | flooded_10km_age_6 == 1 ~ "less than 10km",
        flooded_20km_age_5 == 1 | flooded_20km_age_6 == 1 ~ "10-20km",
        TRUE ~ "0"
      ),
      flooded_00km20_age_5_or_6 = ifelse(flooded_dist20 == "0", 0, 1)
    )

  print(count(dhs_dat, flooded_dist20))

  model20 <- feols(
    no_education ~ i(flooded_dist20, flooded_00km20_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    data = dhs_dat, cluster = ~clustid, weights = ~weight
  )
  print(summary(model20))

  res20 <- broom::tidy(model20, conf.int = TRUE) %>%
    filter(str_detect(term, "flooded_\\d+km20_age_\\d+(_or_\\d+)*$")) %>%
    mutate(
      level = sub("^flooded_dist20::(.*):flooded_00km20_age_5_or_6$", "\\1", term),
      level = factor(level, levels = band_levels, labels = band_labels)
    ) %>%
    filter(!is.na(level))

  print(res20)
  write.csv(res20, "results/spillover_10_20km.csv", row.names = FALSE)

  p <- ggplot(res20, aes(x = level, y = estimate * 100)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey50") +
    geom_pointrange(aes(ymin = conf.low * 100, ymax = conf.high * 100),
                    linewidth = 0.3, size = 0.5) +
    coord_flip() +
    labs(x = NULL, y = "Change in probability of no education (pp)") +
    theme_clean(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.background = element_blank()
    )
  ggsave("figures/spillover_10_20km.jpeg", height = 2, width = 6, plot = p)
}
