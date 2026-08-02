
# Merge DHS geocodes to household data, trimmed to save memory (data/merged_dhs/)

merge_geocodes <- function(files) {
  
  # Create output directory if it doesn't exist
  dir_path <- "data/merged_dhs"
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  for (i in 1:nrow(files)) {
    
    # Get file names
    dta_name <- files$dta_file[i]
    geo_name <- files$geo_file[i]
    
    # If geo data is available
    if (!is.na(geo_name)) {
      
      # Read in dhs dataset
      dat <- read_dta(dta_name)
      
      # Read in the geocodes
      geo_dat <- st_read(geo_name, quiet=TRUE)
      
      # Desired variables
      vars <- c("hvidx", "hhid",
                "hv000", "hv001", "hv002", "hv005", "hv006", "hv007", "hv008", "hv025", 
                "hv112", "hv114", "hv116", "hv218",
                "hv270", "hv271",
                "hv104", "hv105", "hv106", "hv107", "hv108", "hv109", "hv110", 
                "hv121", "hv122", "hv123", "hv124", "hv125", "hv126", "hv127", "hv128", "hv129")
      
      # Set missing columns as NA
      missing_vars <- setdiff(vars, names(dat))
      dat[missing_vars] <- NA
      
      # Select relevant variables
      dat <- dat %>% select(all_of(vars))

      # Re-factor and label variables for consistency across datasets
      dat$hvidx <- as.integer(dat$hvidx)
      attr(dat$hvidx, "label") <- "Line number"

      dat$hv112 <- as.integer(dat$hv112)
      attr(dat$hv112, "label") <- "Line number of mother"

      dat$hv114 <- as.integer(dat$hv114)
      attr(dat$hv114, "label") <- "Line number of father"

      dat$hv116[dat$hv116 >= 8] <- NA
      dat$hv116 <- factor(dat$hv116, levels = c(0, 1, 2),
                          labels = c("Never married", "Currently married",
                                     "Formerly/ever married"))
      attr(dat$hv116, "label") <- "Currently, formerly, never married"

      dat$hv000 <- as.character(dat$hv000)
      attr(dat$hv000, "label") <- "Country code and phase"
      
      dat$hv001 <- as.integer(dat$hv001)
      attr(dat$hv001, "label") <- "Cluster number"
      
      dat$hv002 <- as.integer(dat$hv002)
      attr(dat$hv002, "label") <- "Household number"
      
      dat$hv005 <- as.integer(dat$hv005)
      attr(dat$hv005, "label") <- "Sample weight"
      
      dat$hv006 <- as.integer(dat$hv006)
      attr(dat$hv006, "label") <- "Month of interview"
      
      dat$hv007 <- as.integer(dat$hv007)
      attr(dat$hv007, "label") <- "Year of interview"
      
      dat$hv008 <- as.integer(dat$hv008)
      attr(dat$hv008, "label") <- "Date of interview (cmc)"
      
      dat$hv025 <- factor(dat$hv025, levels=c(1,2), labels=c("Urban", "Rural"))
      attr(dat$hv025, "label") <- "Urban/rural residence"
      
      dat$hv270 <- factor(dat$hv270, levels=c(1,2,3,4,5), 
                          labels=c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
      attr(dat$hv270, "label") <- "Wealth quintile"
      
      dat$hv270_numeric <- as.numeric(dat$hv270)
      attr(dat$hv270_numeric, "label") <- "Wealth quintile (numeric)"
      
      dat$hv271 <- as.numeric(dat$hv271)/100000
      attr(dat$hv271, "label") <- "Wealth index score"
      
      dat$hv104 <- factor(dat$hv104, levels=c(1,2), labels=c("Male", "Female"))
      attr(dat$hv104, "label") <- "Sex of household member"
      
      dat$hv105 <- as.integer(dat$hv105)
      dat$hv105[dat$hv105 >= 95] <- NA
      attr(dat$hv105, "label") <- "Age of household member"
      
      dat$hv106[dat$hv106 >= 8] <- NA
      dat$hv106 <- factor(dat$hv106, levels=c(0,1,2,3), 
                          labels=c("No education", "Primary", "Secondary", "Higher"))
      attr(dat$hv106, "label") <- "Highest level of education"
      
      dat$hv107 <- as.integer(dat$hv107)
      dat$hv107[dat$hv107 >= 95] <- NA
      attr(dat$hv107, "label") <- "Highest year of education"
      
      dat$hv108 <- as.integer(dat$hv108)
      dat$hv108[dat$hv108 >= 95] <- NA
      attr(dat$hv108, "label") <- "Education in single years"
      
      dat$hv109[dat$hv109 >= 8] <- NA
      dat$hv109 <- factor(dat$hv109, levels=c(0,1,2,3,4,5), 
                          labels=c("No education", 
                                   "Incomplete primary",
                                   "Complete Primary",
                                   "Incomplete Secondary",
                                   "Complete Secondary", 
                                   "Higher"))
      attr(dat$hv109, "label") <- "Educational attainment"
      
      dat$hv110 <- factor(dat$hv110, levels=c(0,1), labels=c("No", "Yes"))
      attr(dat$hv110, "label") <- "Member still in school"
      
      dat$hv121 <- factor(dat$hv121, levels=c(0,1,2), 
                          labels=c("No", "Currently Attending", "Attended at some time"))
      attr(dat$hv121, "label") <- "Member attended school during current school year"
      
      dat$hv122 <- factor(dat$hv122, levels=c(0,1,2,3), 
                          labels=c("No education", "Primary", "Secondary", "Higher"))
      attr(dat$hv122, "label") <- "Education level during current school year"
      
      dat$hv123 <- as.integer(dat$hv123)
      dat$hv123[dat$hv123 >= 95] <- NA
      attr(dat$hv123, "label") <- "Grade of education during current school year"
      
      dat$hv124 <- as.integer(dat$hv124)
      dat$hv124[dat$hv124 >= 95] <- NA
      attr(dat$hv124, "label") <- "Education in single years - current school year"
      
      dat$hv125 <- factor(dat$hv125, levels=c(0,1), labels=c("No", "Yes"))
      attr(dat$hv125, "label") <- "Member attended school during previous school year"
      
      dat$hv126[dat$hv126 >= 8] <- NA
      dat$hv126 <- factor(dat$hv126, levels=c(0,1,2,3), 
                          labels=c("No education", "Primary", "Secondary", "Higher"))
      attr(dat$hv126, "label") <- "Education level during previous school year"
      
      dat$hv127 <- as.integer(dat$hv127)
      dat$hv127[dat$hv127 >= 95] <- NA
      attr(dat$hv127, "label") <- "Grade of education during previous school year"
      
      dat$hv128 <- as.integer(dat$hv128)
      dat$hv128[dat$hv128 >= 95] <- NA
      attr(dat$hv128, "label") <- "Education in single years - previous school year"
      
      dat$hv129[dat$hv129 >= 8] <- NA
      dat$hv129 <- factor(dat$hv129, levels=c(0,1,2,3,4,5), 
                          labels=c("Never attended",
                                   "Entered school",
                                   "Advanced",
                                   "Repeating",
                                   "Dropout",
                                   "Left school 2+ years ago"))
      attr(dat$hv129, "label") <- "School attendance status"
      
      # Convert dates to gregorian calendar where necessary
      if (substr(dat$hv000[1], 1, 2)=="ET") {
        dat$hv008 <- dat$hv008+92
        dat$hv007 <- trunc((dat$hv008-1)/12) + 1900
        dat$hv006 <- dat$hv008 - ((dat$hv007-1900)*12)
      }
      if (substr(dat$hv000[1], 1, 2)=="NP") {
        dat$hv008 <- dat$hv008-681
        dat$hv007 <- trunc((dat$hv008-1)/12) + 1900
        dat$hv006 <- dat$hv008 - ((dat$hv007-1900)*12)
      }
      
      # Add sex and highest level of education of household head
      head_dat <- dat %>%
        filter(hvidx == hv218) %>%
        select(hhid, hv104, hv109) %>%
        rename(hh_head_sex = hv104, hh_edu_attain = hv109)
      dat <- dat %>%
        left_join(head_dat, by = "hhid")
      
      # Select relevant variables of geodata
      geo_dat <- geo_dat %>%
        select(DHSCLUST, LATNUM, LONGNUM, ALT_GPS) %>%
        rename(hv001 = DHSCLUST) # Make cluster var name same
      
      # Merge with dat
      dat <- left_join(dat, geo_dat, by="hv001")
      
      # Add source file names
      dat$dta_name <- sub(".*dhs/([^/]+)/.*", "\\1", dta_name)
      dat$geo_name <- sub(".*geocodes/([^/]+)/.*", "\\1", geo_name)
      
      # Save the data
      saveRDS(dat, paste0("data/merged_dhs/", dat$dta_name[1], ".rds"))
    }
  }
}
