
# Figure 1: (A) total days flooded, (B) proportion of cluster with no education

plot_map_figure <- function() {

  dhs_dat <- readRDS("data/analysis_data/flood_56_age_10.rds")
  countries <- countrycode(unique(substr(dhs_dat$clustid, 1, 2)), "dhs", "iso3c")

  # Base world map, flagging study countries
  world_map <- map("world", fill = TRUE, plot = FALSE)
  world_sf <- st_as_sf(world_map) %>%
    mutate(
      iso3 = countrycode(ID, "country.name", "iso3c"),
      in_study = iso3 %in% countries
    )

  study_countries_sf <- world_sf %>%
    filter(in_study) %>%
    st_make_valid()

  # Shared map settings
  study_xlim <- c(-20, 110)
  study_ylim <- c(-35, 40)
  haiti_bb <- st_bbox(study_countries_sf %>% filter(iso3 == "HTI"))
  pad <- 0.5

  map_theme <- theme_minimal() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0, 0),
      legend.justification = c(0, 0),
      legend.title = element_text(face = "bold"),
      plot.tag = element_text(face = "bold"),
      plot.tag.position = c(0.01, 0.98),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  inset_theme <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 9, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = "black"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  # Country background layer
  country_layer <- list(
    geom_sf(data = world_sf, aes(fill = in_study), color = "black", linewidth = 0.2),
    scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "white"), guide = "none")
  )


  # Panel A: flood duration -------------------------------------------------

  total_flood_length <- rast("data/global_flood_length.tif")
  total_flood_length[total_flood_length == 0] <- NA

  duration_df <- as.data.frame(total_flood_length, xy = TRUE, na.rm = TRUE) %>%
    mutate(flood_cat = cut(flooded,
                           breaks = c(0, 5, 10, 50, 100, 500, Inf),
                           labels = c("1-5", "6-10", "11-50", "51-100", "101-500", "500+")))

  tiles_sf <- st_as_sf(
    duration_df,
    coords = c("x", "y"),
    crs = st_crs(world_sf),
    remove = FALSE
  )

  # Keep only tiles inside study countries
  tiles_masked <- st_join(
    tiles_sf,
    study_countries_sf,
    join = st_within,
    left = FALSE
  )

  # Drop tiles falling on lakes/water (e.g. Lake Victoria)
  lakes_sf <- st_make_valid(st_as_sf(map("lakes", fill = TRUE, plot = FALSE)))
  in_lake <- lengths(st_within(tiles_masked, lakes_sf)) > 0
  tiles_masked <- tiles_masked[!in_lake, ]

  flood_tiles <- geom_tile(data = tiles_masked, aes(x = x, y = y, fill = flood_cat))

  flood_plot <- ggplot() +
    country_layer +
    ggnewscale::new_scale_fill() +
    flood_tiles +
    scale_fill_viridis_d(
      name = "Total no.\ndays flooded\n",
      option = "cividis",
      direction = 1,
      begin = 0.1,
      end = 0.95
    ) +
    coord_sf(xlim = study_xlim, ylim = study_ylim, expand = FALSE) +
    labs(tag = "A") +
    map_theme

  flood_inset <- ggplot() +
    country_layer +
    ggnewscale::new_scale_fill() +
    flood_tiles +
    scale_fill_viridis_d(
      option = "cividis", direction = 1, begin = 0.1, end = 0.95,
      drop = FALSE, guide = "none"
    ) +
    coord_sf(
      xlim = c(haiti_bb["xmin"] - pad, haiti_bb["xmax"] + pad),
      ylim = c(haiti_bb["ymin"] - pad, haiti_bb["ymax"] + pad),
      expand = FALSE
    ) +
    labs(title = "Haiti") +
    inset_theme

  flood_plot <- flood_plot +
    inset_element(flood_inset, left = 0.78, bottom = 0.02, right = 0.99, top = 0.25)


  # Panel B: proportion with no education -----------------------------------

  # Average proportion receiving no education in each cluster
  prop_school <- dhs_dat %>%
    group_by(clustid, geometry) %>%
    summarise(
      no_education_prop = mean(no_education, na.rm = TRUE)
    )

  merged_prop <- st_as_sf(prop_school) %>%
    arrange(no_education_prop)

  cluster_points <- geom_sf(
    data = merged_prop,
    aes(color = no_education_prop),
    size = 0.01,
    shape = 20
  )

  cluster_plot <- ggplot() +
    country_layer +
    cluster_points +
    scale_color_viridis_c(
      name = "Proportion of\ncluster with\nno education\n",
      option = "magma",
      direction = 1
    ) +
    coord_sf(xlim = study_xlim, ylim = study_ylim, expand = FALSE) +
    labs(tag = "B") +
    map_theme

  cluster_inset <- ggplot() +
    country_layer +
    cluster_points +
    scale_color_viridis_c(option = "magma", direction = 1, guide = "none") +
    coord_sf(
      xlim = c(haiti_bb["xmin"] - pad, haiti_bb["xmax"] + pad),
      ylim = c(haiti_bb["ymin"] - pad, haiti_bb["ymax"] + pad),
      expand = FALSE
    ) +
    labs(title = "Haiti") +
    inset_theme

  cluster_plot <- cluster_plot +
    inset_element(cluster_inset, left = 0.78, bottom = 0.02, right = 0.99, top = 0.25)


  # Combine -----------------------------------------------------------------

  fig <- wrap_elements(flood_plot) / wrap_elements(cluster_plot)

  ggsave("figures/flood_map.jpeg", plot = fig, height = 10, width = 9)
}
