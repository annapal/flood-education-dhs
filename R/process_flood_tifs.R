
# Link flood tifs with DHS geocodes

process_flood_tifs <- function(dat_path) {
  
  # Create directories if they don't already exist
  dirs <- c("figures/flood_clusters/", "data/flood_clusters/")
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Get dhs cluster files
  geo_files <- list.files(paste0(dat_path, "/geocodes"), pattern = "\\.shp$", 
                          recursive = TRUE, 
                          full.names = TRUE, 
                          ignore.case = TRUE)
  require_found(geo_files, ".shp files", paste0(dat_path, "/geocodes"),
                "DHS Program GPS datasets, https://dhsprogram.com/ (README step 1)")

  # Read in files
  shape_list <- lapply(geo_files, function(f) {
    st_read(f, quiet = TRUE) %>%
      mutate(source_file = f)  # keep track of file name
  })
  
  # Convert to single dataframe
  all_clust <- bind_rows(shape_list)
  all_clust <- all_clust[!(all_clust$LATNUM == 0 & all_clust$LONGNUM == 0), ]
  
  # Prepare cluster data with correct projection
  clust_proj <- st_transform(all_clust, 3857)
  
  # Get flood event names
  events <- basename(list.dirs(paste0(dat_path, "/gfd_v1_4"),
                               recursive = FALSE, full.names = TRUE))
  require_found(events, "event folders", paste0(dat_path, "/gfd_v1_4"), gfd_source)
  
  for (i in 1:length(events)) {
    # Get event name and ID
    event <- events[i]
    dfo_id <- sub("_From.*", "", event)
    message("Starting event: ", event)
    
    # Get tif files in the directory
    tif_files <- list.files(paste0(dat_path, "/gfd_v1_4/", event),  
                            pattern = "\\.tif$", full.names = TRUE)
    
    # Stitch files together (if necessary) or just load single file
    if (length(tif_files)>1) {
      r_list <- lapply(tif_files, rast)
      f_tif <- do.call(merge, r_list)
    } else {
      f_tif <- rast(tif_files[1])
    }
    
    # Check if all flooded values are 0
    if (all(values(f_tif$flooded) == 0, na.rm = TRUE)) next
    
    # Load raster and flooded layer
    flooded_rast <- f_tif[["flooded"]]
    flooded_points <- as.points(flooded_rast == 1, na.rm = TRUE, values = TRUE)
    flooded_points <- flooded_points[flooded_points$flooded == 1]
    
    # Convert and project
    flooded_proj <- st_transform(st_as_sf(flooded_points), 3857)
    
    # Tag cluster proximity for each buffer distance
    buffer_dists <- c(2, 5, 10, 20, 50, 100, 150, 200)
    clust_copy <- all_clust
    for (dist_km in buffer_dists) {
      buffer_geom <- st_union(st_buffer(flooded_proj, dist = dist_km * 1000))
      intersects <- st_intersects(clust_proj, buffer_geom, sparse = FALSE)[, 1]
      clust_copy[[paste0("flooded_", dist_km, "km")]] <- as.integer(intersects)
    }
    
    # Keep data on affected countries
    flooded_sources <- unique(clust_copy$source_file[clust_copy$flooded_200km == 1])
    clust_aff <- clust_copy[clust_copy$source_file %in% flooded_sources, ]
    
    if (nrow(clust_aff) == 0) next
    
    # Plot 10 km result
    flooded_buffer_10km <- st_union(st_buffer(flooded_proj, dist = 10000))
    p <- ggplot() +
      geom_sf(data = flooded_buffer_10km, fill = "lightblue", color = NA, alpha = 0.5) +
      geom_sf(data = clust_aff, aes(color = factor(flooded_10km)), size = 0.25) +
      scale_color_manual(values = c("gray50", "red"), labels = c("Not flooded", "Flooded")) +
      theme_minimal() +
      labs(title = event, color = "Cluster (10km radius)")
    suppressMessages(ggsave(paste0("figures/flood_clusters/", event, ".jpeg"), plot = p))
    
    # Add start/end dates
    matches <- regmatches(event, regexec("_(\\d{8})_to_(\\d{8})", event))[[1]]
    clust_aff$start_date <- as.Date(matches[2], format = "%Y%m%d")
    clust_aff$end_date <- as.Date(matches[3], format = "%Y%m%d")
    
    # Add main cause
    other_info <- fromJSON(paste0(dat_path, "/gfd_v1_4/", event, "/", dfo_id, "_properties.json"))
    clust_aff$dfo_main_cause <- other_info$dfo_main_cause
    clust_aff$dfo_id <- dfo_id

    # Save output
    saveRDS(clust_aff, paste0("data/flood_clusters/", event, ".rds"))
  }
}
