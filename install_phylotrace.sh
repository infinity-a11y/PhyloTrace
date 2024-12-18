#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
CONDA_PATH=$( conda info --base )/bin/conda

eval "$($CONDA_PATH shell.bash hook)"
conda activate PhyloTrace

# Generate PhyloTrace Desktop Entry
cat > PhyloTrace.desktop << EOF
[Desktop Entry]
Name=PhyloTrace
Exec=$SCRIPT_DIR/run_phylotrace.sh
Icon=PhyloTrace
Terminal=true
Type=Application
Categories=Education
EOF

# Generate PhyloTrace run script
cat > run_phylotrace.sh << EOF
#!/bin/bash
SCRIPT_DIR=$SCRIPT_DIR
CONDA_PATH=$CONDA_PATH
EOF

cat >> run_phylotrace.sh << 'EOF'
# Activate conda
eval "$($CONDA_PATH shell.bash hook)"
conda activate PhyloTrace
if [ "$R_BROWSER" != "" ]; then
   Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)"
elif [[ $(uname -r) == *"microsoft"* ]]; then
   R_BROWSER=wslview Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)"
else
   R_BROWSER=xdg-open Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)"
fi
EOF

# Install visNetwork modification
Rscript -e "remotes::install_github('fpaskali/visNetwork', force = TRUE)"

# Setting up the Desktop Icon
mkdir -p $HOME/.local/share/applications
mkdir -p $HOME/.local/share/icons/hicolor/scalable/apps
mv $SCRIPT_DIR/PhyloTrace.desktop $HOME/.local/share/applications
cp $SCRIPT_DIR/www/phylo.png $HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace.png
chmod 700 $SCRIPT_DIR/run_phylotrace.sh
