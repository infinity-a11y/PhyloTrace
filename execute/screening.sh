#!/bin/bash

cd execute
source ~/miniconda3/etc/profile.d/conda.sh
conda activate PhyloTrace
unset R_HOME

# Set base path
base_path=$(Rscript -e "cat(readRDS('screening_meta.rds')[,'wd'])")
selected=$(Rscript -e "cat(stringr::str_split_1(readRDS('screening_meta.rds')[,'selected'], ' '))")
species=$(Rscript -e "cat(readRDS('screening_meta.rds')[,'species'])")

# Remove the existing directory (if it exists)
if [ -d "$base_path/execute/screening" ]; then
    rm -r "$base_path/execute/screening"
fi

isolates=($selected)

# Loop through the list of file names and copy them to the new folder
for file in "${isolates[@]}"; do

    # Get the directory and base name of the zip file
    zip_dir=$(dirname "$file")
    zip_base=$(basename "$file" .zip)
    
    unzip -o "$file" -d "$zip_dir"
    
    amrfinder -n "$zip_dir/$zip_base.fasta" --plus --organism $species -o "$zip_dir/resProfile.tsv" > amrfinder_stdout.txt 2> amrfinder_stderr.txt
    status=$?
    
    # Check exit status 
    if [ "$status" -ne 0 ]; then
      echo "AMRFinder failed with status $status" > "$zip_dir/status.txt"
      echo "Error details:" >> "$zip_dir/status.txt"
      cat amrfinder_stderr.txt >> "$zip_dir/status.txt"
    else
        # Write success message if AMRFinder executed successfully
        echo "AMRFinder executed successfully for $zip_base" > "$zip_dir/status.txt"
    fi
    
    # Clear unzipped assembly
    rm -rf "$zip_dir/$zip_base.fasta"
done

echo "AMRFinder finalized"
