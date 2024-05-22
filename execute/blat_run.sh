#!/bin/bash

cd execute
source ~/miniconda3/etc/profile.d/conda.sh
conda activate PhyloTrace
unset R_HOME

# Get variables
base_path=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'wd'])")
scheme=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'scheme'])")
alleles=$(Rscript -e "cat(readRDS('single_typing_df.rds')[,'alleles'])")

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

# Logfile
log_file="$base_path/execute/script_log.txt"

# Function to log messages to the file
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

# Create a log file or truncate if it exists
echo 0 > "$base_path/execute/progress.txt"

# Run blat
count=0
for query_file in "$alleles"/*.{fasta,fa,fna}; do
  if [ -f "$query_file" ]; then
    query_filename=$(basename "$query_file")
    query_filename_noext="${query_filename%.*}"
  output_file="$results/$query_filename_noext"
  pblat $genome "$query_file" "$output_file.psl"
  ((count++))
  echo $count > "$base_path/execute/progress.txt"
  fi
done

# Start appending results
echo 888888 >> "$base_path/execute/progress.txt"
Rscript "$base_path/execute/single_typing.R"

# Single typing finalized
echo 999999 >> "$base_path/execute/progress.txt"
