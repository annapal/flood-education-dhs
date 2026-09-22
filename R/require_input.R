
# Fail early, with a clear message, when a required input is missing

# Where each external input comes from (see README "Instructions for running the code")
flood_archive_source <- "Dartmouth Flood Observatory, https://floodobservatory.colorado.edu/ (README step 3)"
wpp_source <- "UN World Population Prospects 2024, https://population.un.org/wpp/ (README step 4)"
coresidence_source <- "Global coresidence dataset, https://zenodo.org/records/12721630 (README step 4)"
gfd_source <- "Global Flood Database v1.4, https://global-flood-database.cloudtostreet.ai/ (README step 2)"
worldpop_source <- "WorldPop 1km age-sex rasters, https://www.worldpop.org/ (README step 5)"
gdis_source <- "GDIS disaster locations, https://doi.org/10.7927/zz3b-8y61"

# Where each pipeline-generated input comes from
analysis_data_source <- "Produced by make_analysis_datasets() earlier in _run.R"
results_source <- "Produced by run_models() and the sensitivity analyses earlier in _run.R"

# Stop unless every path exists; `source` says where the file comes from
require_input <- function(paths, source = NULL) {
  missing <- paths[!file.exists(path.expand(paths))]
  if (length(missing) == 0) return(invisible(paths))
  msg <- paste0("Missing required input file(s):\n  ",
                paste(missing, collapse = "\n  "))
  if (!is.null(source)) msg <- paste0(msg, "\n\nSource: ", source)
  stop(msg, call. = FALSE)
}

# Stop unless a directory search found something; `what` names what was sought
require_found <- function(found, what, dir, source = NULL) {
  if (length(found) > 0) return(invisible(found))
  msg <- paste0("No ", what, " found in: ", dir)
  if (!is.null(source)) msg <- paste0(msg, "\n\nSource: ", source)
  stop(msg, call. = FALSE)
}
