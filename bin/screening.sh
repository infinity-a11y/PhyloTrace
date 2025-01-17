#!/bin/bash
APP_LOCAL_SHARE_DIR = "$HOME/.local/share/phylotrace"

# Set base path
base_path=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 'screening_meta.rds'))[,'wd'])")
selected=$(Rscript -e "cat(stringr::str_split_1(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 'screening_meta.rds'))[,'selected'], ' '))")
species=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 'screening_meta.rds'))[,'species'])")
database=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 'screening_meta.rds'))[,'database'])")
scheme=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 'screening_meta.rds'))[,'scheme'])")

# Remove the existing directory (if it exists)
if [ -d "$APP_LOCAL_SHARE_DIR/screening" ]; then
    rm -r "$APP_LOCAL_SHARE_DIR/screening"
fi

isolates=($selected)

# Loop through the list of file names and copy them to the new folder
for file in "${isolates[@]}"; do

    # Get the directory and base name of the zip file
    zip_dir=$(dirname "$file")
    zip_base=$(basename "$file" .zip)
    
    unzip -o "$file" -d "$zip_dir"
    
    abritamr run --contigs "$zip_dir/$zip_base.fasta" --species $species --prefix "$zip_dir" > amrfinder_stdout.txt 2> amrfinder_stderr.txt
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

Rscript make_amr_profile.R "$database" "$scheme" "$base_path"

echo "AMRFinder finalized"
