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

cat "$path_assembly"

# Remove the existing directory (if it exists)
if [ -d "$base_path/execute/screening" ]; then
    rm -r "$base_path/execute/screening"
fi

mkdir "$base_path/execute/screening"

# Directory name
results="$base_path/execute/screening/results"

# Remove the existing directory (if it exists)
if [ -d "$results" ]; then
    rm -r "$results"
fi

# Create a new directory
mkdir "$results"

# Get cores
coresall=$(nproc --all)
cores=$((num_processors - 2))

amrfinder -n "$path_assembly" --threads $cores #--organism "Klebsiella_oxytoca" 

