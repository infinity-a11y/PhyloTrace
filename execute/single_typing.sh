#!/bin/bash

cd execute
source ~/miniconda3/etc/profile.d/conda.sh
conda activate PhyloTrace
unset R_HOME

# Set base path
base_path=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'wd'])")

# reset progress
echo 0 > "$base_path/logs/progress.txt"

# Get variables
scheme=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'scheme'])")
alleles=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'alleles'])")
genome_name=$(Rscript -e "cat(basename(readRDS('single_typing_df.rds')[,'genome']))")
genome_name=$(echo "$genome_name" | sed 's/~/ /g')
rename_file=$(Rscript -e "cat(readRDS('meta_info_single.rds')[, 'assembly_id'])")

# Remove the existing directory (if it exists)
if [ -d "$base_path/execute/blat_single" ]; then
    rm -r "$base_path/execute/blat_single"
fi

mkdir "$base_path/execute/blat_single"

# Directory name
results="$base_path/execute/blat_single/results"

# Remove the existing directory (if it exists)
if [ -d "$results" ]; then
    rm -r "$results"
fi

# Create a new directory
mkdir "$results"

# Check assembly file and save in the execute folder
Rscript "$base_path/execute/check_duplicate_single.R"
wait
genome="$base_path/execute/blat_single/$genome_name"

# Rename file
mv "$genome" "$base_path/execute/blat_single/$rename_file.fasta"

# Run parallelized BLAT
#TODO remove X
find "$alleles" -type f \( -name "*.fasta" -o -name "*.fa" -o -name "*.fna" \) | parallel pblat "$base_path/execute/blat_single/$rename_file.fasta" {} "$results/{/.}.psl" > /dev/null 2>&1

# Start appending results
echo 888888 >> "$base_path/logs/progress.txt"
Rscript "$base_path/execute/single_eval.R"
wait

# Single typing finalized
echo 999999 >> "$base_path/logs/progress.txt"
