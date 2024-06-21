#!/bin/bash

cd execute
source ~/miniconda3/etc/profile.d/conda.sh
conda activate PhyloTrace
unset R_HOME

# Get variables
base_path=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[,'wd'])")
scheme=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[,'scheme'])")
genome_folder=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[,'genome_folder'])")
genome_names=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[,'genome_names'])")
alleles=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[,'alleles'])")

# Remove the existing multi directory
if [ -d "$base_path/execute/blat_multi" ]; then
    rm -r "$base_path/execute/blat_multi"
fi
mkdir "$base_path/execute/blat_multi"

# Remove the existing results directory
results="$base_path/execute/blat_multi/results"
if [ -d "$results" ]; then
    rm -r "$results"
fi
mkdir "$results"

selected_genomes="$base_path/execute/selected_genomes"
log_file="$base_path/execute/script_log.txt"

# Function to log messages to the file
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

# Create a log file or truncate if it exists
echo "Start Multi Typing with $scheme scheme." > "$log_file"

# Remove the existing directory (if it exists)
if [ -d "$selected_genomes" ]; then
    rm -r "$selected_genomes"
fi
mkdir $selected_genomes

file_names=($genome_names)

# Loop through the list of file names and copy them to the new folder
for file in "${file_names[@]}"; do
    if [ -f "$genome_folder/$file" ]; then
        cp "$genome_folder/$file" "$selected_genomes/"
        log_message "Initiated $file"
    else
        log_message "$file not found in $genome_folder"
    fi
done

#INDEXING GENOME AS DATABASE
blat_database="$base_path/execute/blat_multi/$scheme"

#RUNNING blat Loop
genome_filename_noext=""

#Indexing Loop
for genome in "$selected_genomes"/*; do
    
    # Check read names of assembly file
    Rscript "$base_path/execute/check_duplicate_multi.R"
    
    if [ -f "$genome" ]; then
    genome_filename=$(basename "$genome")
    genome_filename_noext="${genome_filename%.*}"
    log_message "Processing $genome_filename"
    fi
    mkdir "$results/$genome_filename_noext"
    
    result_folder="$results/$genome_filename_noext"
    
    # Run parallelized BLAT
    find "$alleles" -type f \( -name "*.fasta" -o -name "*.fa" -o -name "*.fna" \) | parallel pblat $genome {} "$result_folder/{/.}.psl"
    
    log_message "Attaching $genome_filename"
    Rscript "$base_path/execute/automatic_typing.R" "$genome_filename"
done
log_message "Multi Typing finalized."
