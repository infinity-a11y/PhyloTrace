#!/bin/bash

cd "$HOME/.local/share/phylotrace/scripts"
echo "This is conda path: $CONDA_PATH"

# source ~/miniconda3/etc/profile.d/conda.sh
# conda activate PhyloTrace
# unset R_HOME

# # Get variables
# base_path=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[['wd']])")
# scheme=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[['scheme']])")
# genome_folder=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[['genome_folder']])")
# genome_names=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[['genome_names']])")
# alleles=$(Rscript -e "cat(readRDS('multi_typing_df.rds')[['alleles']])")
# rename_file=$(Rscript -e "cat(stringr::str_split_1(readRDS('multi_typing_df.rds')[['filenames']], ' '))")

# # Remove the existing multi directory
# if [ -d "$HOME/.local/share/phylotrace/scripts/blat_multi" ]; then
#     rm -r "$HOME/.local/share/phylotrace/scripts/blat_multi"
# fi
# mkdir "$HOME/.local/share/phylotrace/scripts/blat_multi"

# # Remove the existing results directory
# results="$HOME/.local/share/phylotrace/scripts/blat_multi/results"
# if [ -d "$results" ]; then
#     rm -r "$results"
# fi
# mkdir "$results"

# selected_genomes="$HOME/.local/share/phylotrace/scripts/selected_genomes"
# log_file="$HOME/.local/share/phylotrace/logs/script_log.txt"

# # Create a log file or truncate if it exists
# echo "Start Multi Typing with $scheme scheme." > "$log_file"

# # Remove the existing directory (if it exists)
# if [ -d "$selected_genomes" ]; then
#     rm -r "$selected_genomes"
# fi
# mkdir $selected_genomes

# file_names=($genome_names)
# new_names=($rename_file)

# index=0
# # Loop through the list of file names and copy them to the new folder
# for file in "${file_names[@]}"; do

#     # Replace tilde with space in the filename #TODO
#     new_file=$(echo "$file" | sed 's/~/ /g')
    
#     if [ -f "$genome_folder/$new_file" ]; then
#         cp "$genome_folder/$new_file" "$selected_genomes/"
        
#         mv "$selected_genomes/$new_file" "$selected_genomes/${new_names[$index]}.fasta"
        
#         echo "$(date +"%Y-%m-%d %H:%M:%S") - Initiated $new_file" >> "$log_file"
#     else
#         echo "$(date +"%Y-%m-%d %H:%M:%S") - $new_file not found in $genome_folder" >> "$log_file"
#     fi
#     index=$((index + 1))
# done

# #INDEXING GENOME AS DATABASE
# blat_database="$HOME/.local/share/phylotrace/scripts/blat_multi/$scheme"

# #RUNNING blat Loop
# genome_filename_noext=""

# #Indexing Loop
# for genome in "$selected_genomes"/*; do   

#     # Check fasta and formatting
#     if ! Rscript "$HOME/.local/share/phylotrace/scripts/check_duplicate_multi.R" "$base_path"; then
#         echo "$(date +"%Y-%m-%d %H:%M:%S") - FASTA check failed. Typing of $(basename "$genome") aborted." >> "$log_file"
#         continue
#     fi
    
#     if [ -f "$genome" ]; then
#     genome_filename=$(basename "$genome")
#     genome_filename_noext="${genome_filename%.*}"
#     echo "$(date +"%Y-%m-%d %H:%M:%S") - Processing $genome_filename" >> "$log_file"
#     fi
#     mkdir "$results/$genome_filename_noext"
    
#     result_folder="$results/$genome_filename_noext"
    
#     # Run parallelized BLAT
#     if ! find "$alleles" -type f \( -name "*.fasta" -o -name "*.fa" -o -name "*.fna" \) | parallel pblat "$genome" {} "$result_folder/{/.}.psl" > /dev/null 2>&1; then
#         echo "$(date +"%Y-%m-%d %H:%M:%S") - Allele calling failed. Typing of $genome_filename aborted." >> "$log_file"
#         continue
#     fi
    
#     echo "$(date +"%Y-%m-%d %H:%M:%S") - Attaching $genome_filename" >> "$log_file"
    
#     # Check fasta and formatting
#     if ! Rscript "$HOME/.local/share/phylotrace/scripts/multi_eval.R" "$genome_filename"; then
#         echo "$(date +"%Y-%m-%d %H:%M:%S") - Results evaluation failed. Typing of $genome_filename aborted." >> "$log_file"
#         continue
#     fi
# done

# echo "$(date +"%Y-%m-%d %H:%M:%S") - Multi Typing finalized." >> "$log_file"
