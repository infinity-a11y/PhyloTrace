#!/bin/bash

cd execute
source ~/miniconda3/etc/profile.d/conda.sh
conda activate PhyloTrace
unset R_HOME

# Set base path
base_path=$(Rscript -e "cat(readRDS('screening_meta.rds')[,'wd'])")
path_assembly=$(Rscript -e "cat(readRDS('screening_meta.rds')[,'assembly_path'])")
assembly=$(Rscript -e "cat(readRDS('screening_meta.rds')[,'assembly'])")
species=$(Rscript -e "cat(readRDS('screening_meta.rds')[,'species'])")

if [ "$species" = "Escherichia_coli" ]; then
  species="Escherichia"
fi

if [ "$species" = "Burkholderia_mallei_FLI" ] || [ "$species" = "Burkholderia_mallei_RKI" ]; then
species="Burkholderia_mallei"
fi

if [ "$species" = "Klebsiella_oxytoca_sensu_lato" ]; then
  species="Klebsiella_oxytoca"
fi

if [ "$species" = "Salmonella_enterica" ]; then
  species="Salmonella"
fi

if [ "$species" = "Campylobacter_jejuni_coli" ]; then
  species="Campylobacter"
fi

if [ "$species" = "Klebsiella_pneumoniae_sensu_lato" ]; then
  species="Klebsiella_pneumoniae"
fi

# Remove the existing directory (if it exists)
if [ -d "$base_path/execute/screening" ]; then
    rm -r "$base_path/execute/screening"
fi

mkdir "$base_path/execute/screening"

# Get cores
coresall=$(nproc --all)
cores=$((num_processors - 2))

amrfinder -n "$path_assembly" --threads $cores --plus --organism $species -o "screening/output_file.tsv"
