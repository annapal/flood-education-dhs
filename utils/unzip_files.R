
# Unzip all DHS files

unzip_files <- function(zip_folder) {
  
  # List all .zip files in the folder
  zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)
  
  # Extract each zip file
  for (zip_file in zip_files) {
    # Create a subfolder for each ZIP file
    folder_name <- tools::file_path_sans_ext(basename(zip_file))
    out_dir <- file.path(zip_folder, folder_name)
    dir.create(out_dir, showWarnings = FALSE)
    
    unzip(zip_file, exdir = out_dir)
  }
}

# Set directory containing the zip files
unzip_files(paste0(dat_path, "/dhs"))
unzip_files(paste0(dat_path,"/geocodes"))
unzip_files(paste0(dat_path,"/gfd_v1_4"))

