#!/bin/bash
#SBATCH --account=ehsmsph
#SBATCH --job-name=ir_extract
#SBATCH --time=0-04:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=100G
#SBATCH --chdir=/burg-archive/home/ap4973/repos/flood-education-dhs
#SBATCH --output=ir_extract_%j.out

module load singularity/3.7.1

singularity exec ~/geospatial.sif Rscript -e '
  source("packages.R")
  invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
  make_ir_extract()
'
