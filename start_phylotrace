#!/bin/bash
# Get the script directory
SCRIPT_DIR=$(dirname $BASH_SOURCE)

# Activate conda
eval "$(conda shell.bash hook)"
conda activate PhyloTrace
R_BROWSER=xdg-open Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)"
