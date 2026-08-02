#!/bin/bash
#SBATCH --account=ehsmsph
#SBATCH --partition=short
#SBATCH --job-name=matching
#SBATCH --time=0-12:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --chdir=/burg-archive/home/ap4973/repos/flood-education-dhs
#SBATCH --output=matching_%j.out

module load singularity/3.7.1

singularity exec ~/geospatial.sif Rscript -e '
  source("packages.R")
  invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
  run_matching_analysis()   # reads data/analysis_data/flood_56_age_10.rds, writes flood_56_age_10_matched.rds
'
