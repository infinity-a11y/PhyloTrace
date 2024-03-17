<picture>
    <source media="(prefers-color-scheme: light)" srcset="www/PhyloTrace_bw.png">
    <source media="(prefers-color-scheme: dark)" srcset="www/PhyloTrace.png">
    <img src= "www/">
</picture>

<div style="display: flex; justify-content: center; align-items: center;">
    <img align="center" src="www/features.png" alt="Partner/Feature 1" width="60%">
</div>

## Table of Content

* [1 Installation](#1-installation)
    * [1.1 Install Miniconda](#11-install-miniconda)
    * [1.2 Create Conda Environment](#12-create-conda-environment)
    * [1.3 Create Launcher](#13-create-desktop-launcher)
* [2 Running PhyloTrace](#2-running-phylotrace)
* [3 Troubleshooting](#3-troubleshooting)
    * [3.1 General](#31-general)
    * [3.2 Desktop Launcher](#32-desktop-launcher-not-working)
* [4 Roadmap](#4-roadmap)

## 1 Installation

Download the repository as `.zip` and unpack it to a location on your system.

Is ***Miniconda*** or another ***Conda Distribution*** installed on the system?
- No: Run the installation below and initialize conda. 
- Yes: Proceed to [1.2 Create Conda Environment](#12-create-conda-environment).

### 1.1 Install Miniconda
These four commands quickly and quietly install the latest 64-bit version of the installer and then clean up after themselves. 
```bash
mkdir -p ~/miniconda3
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh
bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3
rm ~/miniconda3/miniconda.sh
```

After installation, the newly-installed Miniconda should be initialized with the following command:
```bash
~/miniconda3/bin/conda init
```

To start using conda, the terminal should be restarted.

>***Optional:*** *To disable automatic activation of conda whenever the terminal is started, the following command can be executed:*
```bash
conda config --set auto_activate_base false
```

### 1.2 Create Conda Environment
Create a conda environment containing necessary dependencies and packages.
```bash
cd path/to/directory
conda env create -f PhyloTrace.yml
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*
>*This process might take a while (depends on system capacities).*

### 1.3 Create Desktop Launcher

Create a run script and an `.desktop` file to create a desktop launcher for PhyloTrace. 
```bash
cd path/to/directory
bash install_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

## 2 Running PhyloTrace

Start PhyloTrace by using the launcher in Applications Menu. A tab with the app will automatically open in the default browser.

**Alternative:**

If this doesn't work the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory
bash run_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

## 3 Troubleshooting

### 3.1 General
There are multiple possible sources for issues with the installation. Common mistakes during the installation are: 
- Change `path/to/directory` in the command chunks with the actual path of the repository containing all PhyloTrace files
- Before installation make sure the whole repository is unzipped to a writable location in your system

If the installation issues persist feel free to contact us via [contact@phylotrace.com](mailto:contact@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans) or open an issue.

### 3.2 Desktop Launcher not Working
If the default browser does not work properly, it can be changed, by changing R_BROWSER environment variable. For example, to use Google Chrome, use the following command:
```bash
R_BROWSER=google-chrome bash run_phylotrace.sh
```

>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*


| Browser  | *browser-name*  |
| ------------- | ------------- |
| Google Chrome  | *google-chrome* **or** *google-chrome-stable*  |
| Firefox  | *firefox*  |
| Chromium  | *chromium*  |
| Brave  | *brave-browser*  |
| Opera  | *opera*  |
| Vivaldi  | *vivaldi*  |

<sub>Executable names for some popular browsers.</sub>

If PhyloTrace is still unable to launch from desktop, a missing default browser was likely not the issue. In this case either open PhyloTrace as described in [2 Running PhyloTrace](#2-running-phylotrace) or contact us via [contact@phylotrace.com](mailto:contact@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans).

## Uninstall PhyloTrace
To remove PhyloTrace from your system, remove the Phylotrace directory and run the following command to remove
Desktop Launcher:
```bash
rm $HOME/.local/share/applications/PhyloTrace.desktop
rm $HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace.png
```

## 4 Roadmap
PhyloTrace is under active development with new functions, enhancements and innovations to follow.


<picture>
    <source srcset="www/roadmap.png" width="30%">
    <img src= "www/">
</picture>

<sub>Generated with AI (GPT-4 DALL·E)</sub>



- [X] ***"Hello World!"*** - Completion Version 1.0.0 
- [ ] Support for additional cgMLST scheme databases (e.g. pubMLST, EnteroBase, Institut Pasteur)  
- [ ] MST Graph Clustering 
- [ ] Implementation of a Gene Enrichment Analysis   
- [ ] Hash-Based Core Genome Multilocus Sequence Typing
- [ ] More sophisticated MST Graphs (e.g. Pie Charts as Nodes)
- [ ] Create new cgMLST Schemes / Add new Loci
- [ ] Implementation of a NGS Sequencing Pipeline
- [ ] Backwards Compatibility with MLST (ST calling)
- [ ] Implementation of wgMLST
