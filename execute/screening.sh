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

error_file="screening/error.txt"

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

amrfinder -n "$path_assembly" --plus --organism $species -o "screening/output_file.tsv" > amrfinder_stdout.txt 2> amrfinder_stderr.txt
status=$?

# Check if status variable is set and is an integer
if [ -z "$status" ]; then
  echo "AMRFinder execution did not set an exit status." > "$base_path/execute/$error_file"
  exit 1
fi

if [ "$status" -ne 0 ]; then
  echo "AMRFinder failed with status $status" > "$base_path/execute/$error_file"
  echo "Error details:" >> "$base_path/execute/$error_file"
  cat amrfinder_stderr.txt >> "$base_path/execute/$error_file"
  exit $status
fi

echo "AMRFinder completed successfully"
