# Match files of a given type (GE/IR/MR) to one DHS survey, on the folder's first
# 6 chars, with the version matched within its block (0-9, A-H, I-Q, R-Z)

match_geo_file <- function(file_name, files, type = "GE") {

  # Country code, phase and version of the source survey
  survey <- basename(dirname(file_name))
  cc <- substr(survey, 1, 2)
  phase_num <- substr(survey, 5, 5)
  version <- substr(survey, 6, 6)

  # Version block
  if (version %in% LETTERS[1:8]) {
    range <- LETTERS[1:8]
  } else if (version %in% LETTERS[9:17]) {
    range <- LETTERS[9:17]
  } else if (version %in% LETTERS[18:26]) {
    range <- LETTERS[18:26]
  } else if (version %in% as.character(0:9)) {
    range <- as.character(0:9)
  } else {
    return(character(0))
  }

  # Match on the first 6 chars of each candidate's folder name
  possible_stems <- toupper(paste0(cc, type, phase_num, range))
  candidate_stems <- toupper(substr(basename(dirname(files)), 1, 6))
  files[candidate_stems %in% possible_stems]
}
