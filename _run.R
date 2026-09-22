
source("packages.R")
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

# Set file path for where data is stored
dat_path <- "~/data"


# Prepare the data --------------------------------------------------------

# Reference data frame matching dhs files to dhs geocodes
files <- match_geo_df(dat_path) 

# Merge geocodes to dhs data (saved in data/merged_dhs)
merge_geocodes(files)

# Link GFD to dhs clusters (saved in data/flood_clusters)
# process_flood_tifs() # This takes a long time

# Download ERA5-Land temperature (saved in ~/data/era5_cluster_year.rds)
# download_era5_temp() # Takes a long time, needs a CDS token

# Merge all data together (saved in "data/dhs_dat_combined.rds")
merge_country_data(files)

# Pull data from womens and mens surveys
make_ir_extract()

# Make final analysis datasets (saved in "data/analysis_data)
make_analysis_datasets()


# Descriptive data --------------------------------------------------------

make_summary_table() # Make SI Table of surveys
make_tables() # Make Table S2
make_flood_event_table() # Distinct flood events by characteristic

plot_proportions() # Figure S2
plot_proportion_sex_time() # Figure S3
# make_flood_raster(dat_path) # Takes a long time (saved in data/global_flood_length.tif)
plot_map_figure() # Figure 1 (A: flood duration, B: cluster education)


# Run the analysis --------------------------------------------------------

# Run all the models (results saved in results/results.csv)
run_models(model_specs)

# Run additional sensitivity analyses
run_single_flood_analysis() # (outputs saved in results/)
run_matching_analysis() # (outputs saved in results/)
hazard_correlations() # Correlation with other GDIS hazards (figure saved in figures/)
test_flooded_dist2() # Spillover 10-20km (outputs saved in results/ and figures/)

# Plot the results (saved in figures/)
plot_results()

# Migration sensitivity analysis (outputs saved in results/ and figures/)
run_migration_analysis(who = "mother")

# Run extrapolation
exptrapolate_results(dat_path) # Takes a long time, outputs saved in results/ and figures/

