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
Rscript "$base_path/execute/check_duplicate.R"
wait
genome="$base_path/execute/blat_single/assembly.fasta"

# Run parallelized BLAT
parallel --citation
find "$alleles" -type f \( -name "*.fasta" -o -name "*.fa" -o -name "*.fna" \) | parallel pblat $genome {} "$results/{/.}.psl"

# Start appending results
echo 888888 >> "$base_path/logs/progress.txt"
echo "Initiating addition of $genome_name (attaching)" >> "$base_path/logs/output.log"
Rscript "$base_path/execute/single_typing.R"
wait

# Single typing finalized
echo 999999 >> "$base_path/logs/progress.txt"
echo "Finished typing of $genome_name" >> "$base_path/logs/output.log"