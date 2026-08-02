
# Combine the women's (IR) and men's (MR) surveys for the analysis countries

# Read one survey's Stata file, keeping vars and renaming to common names
read_dhs_file <- function(f, vars, sex) {

  # Common column names (women's variable names, sans prefix)
  common <- c("caseid", "v000", "v001", "v002", "v003", "v104", "v106")

  dat <- read_dta(f, encoding = "latin1")

  # Set missing columns as NA, then keep and rename to common names
  missing_vars <- setdiff(vars, names(dat))
  dat[missing_vars] <- NA
  dat <- dat %>% select(all_of(vars))
  names(dat) <- common

  # Factor highest education level (as in the household roster, hv106)
  dat$v106[dat$v106 >= 8] <- NA
  dat$v106 <- factor(dat$v106, levels = c(0, 1, 2, 3),
                     labels = c("No education", "Primary", "Secondary", "Higher"))
  attr(dat$v106, "label") <- "Highest level of education"

  # Add source file (full path, matching data/files.csv) and sex flag
  dat$source_file <- path.expand(f)
  dat$sex <- sex
  dat
}

# Country code from a DHS file path (first two letters of the file name)
country_code <- function(file) substr(basename(file), 1, 2)

make_ir_extract <- function(dat_path = "~/data") {

  # Surveys to include: countries in countries.csv that also have geodata
  files <- match_geo_df(dat_path)
  countries <- read.csv("data/countries.csv")
  files <- files %>%
    filter(!is.na(geo_file), country_code(dta_file) %in% countries$dhs_cde)

  # Read women's (IR) and men's (MR) surveys
  women <- lapply(files$ir_file[!is.na(files$ir_file)], read_dhs_file,
                  vars = c("caseid", "v000", "v001", "v002", "v003", "v104", "v106"),
                  sex = "Female")
  men <- lapply(files$mr_file[!is.na(files$mr_file)], read_dhs_file,
                vars = c("mcaseid", "mv000", "mv001", "mv002", "mv003", "mv104", "mv106"),
                sex = "Male")

  dat <- bind_rows(women, men)
  saveRDS(dat, "data/ir_mr_extract.rds")
  dat
}
