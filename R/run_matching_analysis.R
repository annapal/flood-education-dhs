

run_matching_analysis <- function() {

  ps_vars <- c("hv270_numeric", "hv025", "hh_edu_attain", "hh_head_sex")
  need <- c("flooded_10km_age_5_or_6", "hv104", ps_vars, "country_id", "clustid",
            "no_education", "weight")

  dat <- readRDS("data/analysis_data/flood_56_age_10.rds") %>%
    filter(if_all(all_of(need), ~ !is.na(.))) %>%
    mutate(row_id = row_number())

  # Propensity score (logit) for exposure, used to pick nearest controls
  ps_fml <- reformulate(c(ps_vars, "factor(country_id)"),
                        response = "flooded_10km_age_5_or_6")
  ps_mod <- glm(ps_fml, data = dat, family = binomial())
  dat$ps_logit <- predict(ps_mod, type = "link")

  matched <- list(); set_id <- 0L

  # Optimal 1:1 matching on PS within cluster, exact on sex, no replacement
  for (cl in unique(dat$clustid)) {
    cd <- filter(dat, clustid == cl)
    print(cl)

    for (sx in unique(cd$hv104)) {                        # exact match: sex
      exp_d  <- filter(cd, flooded_10km_age_5_or_6 == 1, hv104 == sx)
      ctrl_d <- filter(cd, flooded_10km_age_5_or_6 == 0, hv104 == sx)
      if (nrow(exp_d) == 0L || nrow(ctrl_d) == 0L) next

      # PS-distance matrix: exposed (rows) x controls (cols)
      dmat <- abs(outer(exp_d$ps_logit, ctrl_d$ps_logit, "-"))

      # Hungarian assignment; solve_LSAP needs rows <= cols, so transpose if needed
      if (nrow(dmat) <= ncol(dmat)) {
        asg <- clue::solve_LSAP(dmat)
        pairs <- data.frame(e = seq_along(asg), c = as.integer(asg))
      } else {
        asg <- clue::solve_LSAP(t(dmat))
        pairs <- data.frame(e = as.integer(asg), c = seq_along(asg))
      }

      for (p in seq_len(nrow(pairs))) {
        e  <- exp_d[pairs$e[p], ]
        cc <- ctrl_d[pairs$c[p], ]
        set_id <- set_id + 1L
        matched[[length(matched) + 1L]] <- bind_rows(
          mutate(e,  subclass = set_id, ps_dist = 0),
          mutate(cc, subclass = set_id, ps_dist = abs(cc$ps_logit - e$ps_logit)))
      }
    }
  }

  matched_dat <- bind_rows(matched)
  n_exp <- sum(dat$flooded_10km_age_5_or_6 == 1)
  message(sprintf("Matched sets: %d | exposed matched: %d of %d | controls: %d",
                  set_id, set_id, n_exp,
                  sum(matched_dat$flooded_10km_age_5_or_6 == 0)))

  saveRDS(matched_dat, "data/analysis_data/flood_56_age_10_matched.rds")
  matched_dat <- readRDS("data/analysis_data/flood_56_age_10_matched.rds")

  # Matched-set fixed effects, SEs by cluster
  model <- feols(no_education ~ flooded_10km_age_5_or_6 | country_year,
                 data = matched_dat, cluster = ~clustid, weights = ~weight)
  print(summary(model))

  results <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "flooded_10km_age_5_or_6") %>%
    mutate(data = "flood_56_age_10_matched")
  write.csv(results, "results/matching_results.csv", row.names = FALSE)
}
