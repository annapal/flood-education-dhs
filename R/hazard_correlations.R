
# GDIS disaster correlations with flooding

hazard_correlations <- function() {

  gdis_path <- path.expand("~/data/pend-gdis-1960-2018-disasterlocations-gpkg/pend-gdis-1960-2018-disasterlocations.gpkg")

  # Study countries (DHS codes -> ISO3 for matching GDIS)
  countries <- read_csv("data/countries.csv", show_col_types = FALSE) %>%
    mutate(iso3 = countrycode(name, "country.name", "iso3c"))

  # Subset to study countries in the SQL query
  iso3_sql <- paste0("'", paste(na.omit(countries$iso3), collapse = "','"), "'")

  gdis <- st_read(
    gdis_path,
    query = paste0("SELECT * FROM GPKG WHERE iso3 IN (", iso3_sql, ")"),
    quiet = TRUE
  )

  # Subset to drought, earthquake and extreme temperature
  gdis <- gdis %>%
    mutate(year = as.integer(str_sub(disasterno, 1, 4)),
           level = as.integer(level),
           disastertype = str_trim(disastertype)) %>%
    st_make_valid() %>%
    filter(disastertype %in% c("drought", "earthquake", "extreme temperature"))

  saveRDS(gdis, "data/gdis.rds")

  # Exposure to drought/earthquake/extreme temperature at ages 5-6 -------------

  dhs_dat <- readRDS("data/analysis_data/flood_56_age_10.rds")

  # Unique cluster points as sf in WGS84 (same CRS as GDIS)
  clust_sf <- dhs_dat %>%
    select(clustid, LATNUM, LONGNUM) %>%
    distinct(clustid, .keep_all = TRUE) %>%
    filter(!(LATNUM == 0 & LONGNUM == 0)) %>%
    st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = 4326)

  # Cluster-years in which the point falls inside a GDIS disaster polygon
  hits <- st_intersects(clust_sf, gdis)

  types <- c(drought = "drought", earthquake = "earthquake", extreme_temp = "extreme temperature")

  # One column per disaster type, one row per cluster-year
  clust_disaster <- tibble(clustid = clust_sf$clustid, poly = hits) %>%
    unnest(poly) %>%
    mutate(year = gdis$year[poly],
           type = names(types)[match(gdis$disastertype[poly], types)],
           value = 1L) %>%
    distinct(clustid, year, type, value) %>%
    pivot_wider(names_from = type, values_from = value, values_fill = 0L)

  # 1 if the cluster was in a polygon of that type in the year the child turned 5 or 6
  dhs_dat <- dhs_dat %>%
    left_join(rename_with(clust_disaster, ~paste0(.x, "_5"), all_of(names(types))),
              by = c("clustid", "year_age_5" = "year")) %>%
    left_join(rename_with(clust_disaster, ~paste0(.x, "_6"), all_of(names(types))),
              by = c("clustid", "year_age_6" = "year")) %>%
    mutate(drought_age_5_or_6 = as.integer(coalesce(drought_5, 0L) == 1L |
                                             coalesce(drought_6, 0L) == 1L),
           earthquake_age_5_or_6 = as.integer(coalesce(earthquake_5, 0L) == 1L |
                                                coalesce(earthquake_6, 0L) == 1L),
           extreme_temp_age_5_or_6 = as.integer(coalesce(extreme_temp_5, 0L) == 1L |
                                                  coalesce(extreme_temp_6, 0L) == 1L)) %>%
    select(-all_of(c(paste0(names(types), "_5"), paste0(names(types), "_6"))))

  print(count(dhs_dat, drought_age_5_or_6))
  print(count(dhs_dat, earthquake_age_5_or_6))
  print(count(dhs_dat, extreme_temp_age_5_or_6))

  # Correlation plot: flood exposure vs the other hazards ----------------------

  cor_vars <- c(flood = "flooded_10km_age_5_or_6", drought = "drought_age_5_or_6",
                earthquake = "earthquake_age_5_or_6",
                `extreme\ntemperature` = "extreme_temp_age_5_or_6")

  # Heatmap of one correlation matrix
  cor_panel <- function(cor_mat, title, lim) {
    dimnames(cor_mat) <- list(names(cor_vars), names(cor_vars))
    diag(cor_mat) <- NA

    cor_long <- as.data.frame(as.table(cor_mat)) %>%
      setNames(c("var1", "var2", "corr"))

    ggplot(cor_long, aes(var1, fct_rev(var2), fill = corr)) +
      geom_tile(colour = "white", linewidth = 2) +
      geom_text(aes(label = ifelse(is.na(corr), "", sprintf("%.2f", corr))),
                size = 3.5, colour = "grey15") +
      scale_fill_gradient2(low = "#4C72B0", mid = "grey95", high = "#DD8452",
                           midpoint = 0, limits = c(-lim, lim), na.value = "white",
                           name = "Correlation") +
      coord_fixed() +
      labs(title = title, x = NULL, y = NULL) +
      theme_clean(base_size = 11) +
      theme(plot.title = element_text(size = 10, face = "bold", hjust = 0),
            axis.text = element_text(size = 10),
            panel.grid = element_blank(),
            plot.background = element_blank())
  }

  # Raw correlations
  cor_raw <- cor(as.data.frame(dhs_dat)[, cor_vars], use = "pairwise.complete.obs")

  # Within-cluster
  resid_mat <- sapply(cor_vars, function(v)
    resid(feols(as.formula(paste(v, "~ 1 | clustid")), data = dhs_dat)))

  cor_within <- cor(resid_mat, use = "pairwise.complete.obs")

  # Shared scale limit from the off-diagonal correlations in both matrices
  lim <- max(abs(c(cor_raw[lower.tri(cor_raw)], cor_within[lower.tri(cor_within)])), na.rm = TRUE)

  fig <- cor_panel(cor_raw, "A: Raw", lim) +
    cor_panel(cor_within, "B: Within cluster", lim) +
    plot_layout(guides = "collect")

  ggsave("figures/hazard_correlation.png", plot = fig, width = 10, height = 5)
}
