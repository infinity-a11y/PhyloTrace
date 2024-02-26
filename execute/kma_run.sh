#!/bin/bash

cd execute
source ~/miniconda3/etc/profile.d/conda.sh
conda activate PhyloTrace
unset R_HOME

# Get variables
base_path=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'wd'])")
scheme=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'scheme'])")
genome=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'genome'])")
alleles=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'alleles'])")

echo "$base_path"
echo "$scheme"
echo "$genome"
echo "$alleles"

# Logfile
log_file="$base_path/execute/script_log.txt"

# Function to log messages to the file
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

# Create a log file or truncate if it exists
echo 0 > "$base_path/execute/progress.fifo"
mkdir "$base_path/execute/kma_single"
kma_database="$base_path/execute/kma_single/$scheme"

# Index genome for kma
kma index -i "$genome" -o "$kma_database"

# Directory name
results="$base_path/execute/kma_single/results"

# Remove the existing directory (if it exists)
if [ -d "$results" ]; then
    rm -r "$results"
fi

# Create a new directory
mkdir "$results"

# Run kma
count=0
for query_file in "$alleles"/*.{fasta,fa,fna}; do
if [ -f "$query_file" ]; then
query_filename=$(basename "$query_file")
query_filename_noext="${query_filename%.*}"
output_file="$results/$query_filename_noext"
kma -i "$query_file" -o "$output_file" -t_db "$kma_database" -nc -status
((count++))
echo $count > "$base_path/execute/progress.fifo"
fi
done

# Start appending results
echo 888888 >> "$base_path/execute/progress.fifo"
Rscript "$base_path/execute/single_typing.R"

# Single typing finalized
echo 999999 >> "$base_path/execute/progress.fifo"