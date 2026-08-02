
# Mosaic GFD tiles into a global raster of total days flooded

make_flood_raster <- function(dat_path) {
  # Step 1: List all GeoTIFF files
  tif_files <- list.files(
    path = paste0(dat_path, "/gfd_v1_4/"),
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Step 2: Define a global grid template
  global_template <- rast(
    nrows = 3600, ncols = 7200,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90,
    crs = "EPSG:4326"
  )

  # Step 3: Initialize the total_duration raster
  first_valid <- TRUE
  for (i in seq_along(tif_files)) {
    # Load raster
    r <- rast(tif_files[i])

    # Extract date range from filename
    dates <- str_extract_all(basename(tif_files[i]), "\\d{8}")[[1]]
    start_date <- as.Date(dates[1], format = "%Y%m%d")
    end_date <- as.Date(dates[2], format = "%Y%m%d")
    flood_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1

    # Extract flooded layer
    flooded <- r[[1]]

    # Compute flood_length layer
    flood_length <-  classify(flooded, matrix(c(0, 0,
                                                1, flood_days),
                                              ncol = 2, byrow = TRUE))

    # Reproject to global template
    flood_length_proj <- project(flood_length, global_template, method = "bilinear")

    # Initialize or mosaic
    if (first_valid) {
      total_flood_length <- flood_length_proj
      first_valid <- FALSE
    } else {
      total_flood_length <- mosaic(total_flood_length, flood_length_proj, fun = "sum")
    }

    print(paste0(i, "/", length(tif_files), ": ", tif_files[i]))

    # Clean up
    rm(r, flooded, flood_length, flood_length_proj)
    gc()
  }

  # Save raster
  writeRaster(total_flood_length, "data/global_flood_length.tif", overwrite = TRUE)
}
