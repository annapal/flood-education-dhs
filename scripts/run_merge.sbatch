#!/bin/bash
#SBATCH --account=ehsmsph
#SBATCH --job-name=merge_dhs
#SBATCH --time=0-08:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --chdir=/burg-archive/home/ap4973/repos/flood-education-dhs
#SBATCH --output=merge_dhs_%j.out

module load singularity/3.7.1

singularity exec ~/geospatial.sif Rscript -e '
  source("packages.R")
  invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

  dat_path <- "~/data"
  files <- match_geo_df(dat_path)

  merge_geocodes(files)        # saves data/merged_dhs/*.rds
  merge_country_data(files)    # saves data/dhs_dat_combined.rds
'
