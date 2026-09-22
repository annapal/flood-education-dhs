# Download ERA5-Land temperature

download_era5_temp <- function() {

  library(ecmwfr)

  # CDS Personal Access Token: from CDSAPI_KEY, else whatever wf_set_key() stored in the keyring
  cds_key <- Sys.getenv("CDSAPI_KEY")
  if (nzchar(cds_key)) wf_set_key(key = cds_key)
  stopifnot(nzchar(wf_get_key()))

  clusters <- readRDS("data/analysis_data/flood_56_age_10.rds") %>%
    select(clustid, LATNUM, LONGNUM) %>%
    distinct(clustid, .keep_all = TRUE) %>%
    filter(!is.na(LATNUM), !is.na(LONGNUM),
           !(abs(LATNUM) < 1e-6 & abs(LONGNUM) < 1e-6))  # missing GPS, stored as ~0,0

  pts_v <- vect(as.data.frame(clusters), geom = c("LONGNUM", "LATNUM"), crs = "EPSG:4326")

  # Extract at the cluster points, falling back to a 30km buffer mean where ERA5-Land has no land cell
  extract_temp <- function(r) {
    vals <- terra::extract(r, pts_v, ID = FALSE)
    na_i <- which(is.na(vals[[1]]))
    if (length(na_i) > 0) {
      buf <- buffer(pts_v[na_i], width = 30000)
      vals[na_i, ] <- terra::extract(r, buf, fun = mean, na.rm = TRUE, ID = FALSE)
    }
    vals
  }

  # Bbox padded by 0.5 deg and snapped out to the 0.1 deg grid
  pad <- 0.5
  area <- c(
    N = min(90, ceiling((max(clusters$LATNUM) + pad) * 10) / 10),
    W = max(-180, floor((min(clusters$LONGNUM) - pad) * 10) / 10),
    S = max(-90, floor((min(clusters$LATNUM) - pad) * 10) / 10),
    E = min(180, ceiling((max(clusters$LONGNUM) + pad) * 10) / 10)
  )
  print(area)

  # Ouput directory
  out_dir <- path.expand("~/data/era5-dhs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  for (yr in 2000:2018) {

    target <- paste0("era5_land_t2m_", yr, ".nc")
    if (file.exists(file.path(out_dir, target))) next

    request <- list(
      dataset_short_name = "reanalysis-era5-land-monthly-means",
      product_type = "monthly_averaged_reanalysis",
      variable = "2m_temperature",
      year = as.character(yr),
      month = sprintf("%02d", 1:12),
      time = "00:00",
      area = unname(area),
      data_format = "netcdf",
      download_format = "unarchived",
      target = target
    )

    wf_request(request = request, transfer = TRUE, path = out_dir, verbose = TRUE)
  }

  # Annual mean temperature (deg C) per cluster, from the 12 monthly layers of each year
  annual_temp <- map_dfr(2000:2018, function(yr) {
    r <- mean(rast(file.path(out_dir, paste0("era5_land_t2m_", yr, ".nc"))))
    tibble(clustid = clusters$clustid, year = yr, temp_c = extract_temp(r)[[1]] - 273.15)
  })

  # Annual mean temperature per cluster per year
  saveRDS(annual_temp, path.expand("~/data/era5_cluster_year.rds"))
}
