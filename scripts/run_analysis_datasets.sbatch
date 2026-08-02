#!/bin/bash
#SBATCH --account=ehsmsph
#SBATCH --partition=short
#SBATCH --job-name=analysis_dat
#SBATCH --time=0-05:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --chdir=/burg-archive/home/ap4973/repos/flood-education-dhs
#SBATCH --output=analysis_dat_%j.out

module load singularity/3.7.1

singularity exec ~/geospatial.sif Rscript -e '
  source("packages.R")
  invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
  make_analysis_datasets()   # reads dhs_dat_combined.rds + ir_mr_extract.rds, writes data/analysis_data/
'
