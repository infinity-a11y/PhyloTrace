#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo "[Desktop Entry]" > $SCRIPT_DIR/PhyloTrace.desktop 
echo "Name=PhyloTrace" >> $SCRIPT_DIR/PhyloTrace.desktop
echo "Exec=$SCRIPT_DIR/start_phylotrace.sh" >> $SCRIPT_DIR/PhyloTrace.desktop
echo "Icon=PhyloTrace" >> $SCRIPT_DIR/PhyloTrace.desktop
echo "Terminal=true" >> $SCRIPT_DIR/PhyloTrace.desktop
echo "Type=Application" >> $SCRIPT_DIR/PhyloTrace.desktop
echo "Categories=Education" >> $SCRIPT_DIR/PhyloTrace.desktop

mkdir -p $HOME/.local/share/applications
mkdir -p $HOME/.local/share/icons/hicolor/scalable/apps
mv $SCRIPT_DIR/PhyloTrace.desktop $HOME/.local/share/applications
cp $SCRIPT_DIR/www/phylo.png $HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace.png
chmod +x $SCRIPT_DIR/start_phylotrace.sh
