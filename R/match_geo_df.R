
# Create dataframe that has data file name and corresponding geo file name

match_geo_df <- function(dat_path) {
  # Find all .DTA files recursively in subfolders
  dta_files <- list.files(paste0(dat_path, "/dhs"), pattern = "\\.dta$", 
                          recursive = TRUE, 
                          full.names = TRUE, 
                          ignore.case = TRUE)
  
  # Get file names for geocoded data
  geo_files <- list.files(paste0(dat_path, "/geocodes"), pattern = "\\.shp$",
                          recursive = TRUE,
                          full.names = TRUE,
                          ignore.case = TRUE)

  # Get women's (IR) and men's (MR) recode data files
  ir_files <- list.files(paste0(dat_path, "/dhs-ir"), pattern = "\\.dta$",
                         recursive = TRUE,
                         full.names = TRUE,
                         ignore.case = TRUE)
  mr_files <- list.files(paste0(dat_path, "/dhs-mr"), pattern = "\\.dta$",
                         recursive = TRUE,
                         full.names = TRUE,
                         ignore.case = TRUE)

  # For each dta file, match its geocode, women's (IR) and men's (MR) files
  output <- list()
  for (i in seq_along(dta_files)) {
    dta_file <- dta_files[i]

    # Take the first match, or NA if none
    geo_file <- match_geo_file(dta_file, geo_files, "GE")
    ir_file <- match_geo_file(dta_file, ir_files, "IR")
    mr_file <- match_geo_file(dta_file, mr_files, "MR")
    geo_file <- if (length(geo_file) == 0) NA_character_ else geo_file[1]
    ir_file <- if (length(ir_file) == 0) NA_character_ else ir_file[1]
    mr_file <- if (length(mr_file) == 0) NA_character_ else mr_file[1]

    output[[i]] <- list(dta_file = dta_file, geo_file = geo_file,
                        ir_file = ir_file, mr_file = mr_file)
  }
  files <- do.call(rbind, lapply(output, as.data.frame))
  
  # Add excluded surveys here
  excluded_surveys <- c()
  
  # Remove excluded surveys
  files <- files %>%
    filter(!(dta_file %in% excluded_surveys))
  
  # Save the file spreadsheet
  write.csv(files, "data/files.csv", row.names = FALSE)
  
  files
}
