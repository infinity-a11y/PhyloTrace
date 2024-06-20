
<img src="www/PhyloTrace_bw.png#gh-light-mode-only" width="90%">
<img src="www/PhyloTrace.png#gh-dark-mode-only" width="90%">
<br>
PhyloTrace is a platform for bacterial pathogen monitoring on a genomic level. Its components evolve around allele variant typing, to be more precise: core-genome multi locus sequence typing (cgMLST). Complex analyses and computation are wrapped into an appealing and easy-to-handle graphical user interface. This tool can help to reveal patterns explaining outbreak dynamics and events by connecting genomic information with epidemiologic variables. <br><br>

**Features**

- Interactive cgMLST analysis also for large datasets comprising multiple genome assemblies
- Managing a custom local database 
- Connecting informative meta data and custom variables
- Visualizing the relationship between bacterial isolates with phylogenetic trees
- Download and report functionalities
- *and more*

We want to make cgMLST analysis and genomic pathogen monitoring accessible to a broad spectrum of individuals and organizations. Therefore our goal is to build an interface with convenient user experience and easy handling that doesn't require you to be a bioinformatician. The app is in active development. To get a stable version download the newest release. This tool can be used for research and academic purposes. Note that PhyloTrace is not validated for a clinical use yet, but we are working on achieving that.

[www.PhyloTrace.com](https://www.phylotrace.com)  |  [info@phylotrace.com](mailto:info@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans)

![PartnerLogos](www/partners_logo_round.svg)

<sup><sup> Developed in collaboration with Hochschule Furtwangen University (HFU) and Medical University of Graz (MUG). Featured on ShinyConf 2024. </sup> </sup>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11121153.svg)](https://doi.org/10.5281/zenodo.11121153)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)

---
## Table of Content

* [1 Getting Started](#1-getting-started)
    * [1.1 Compatibility](#11-compatibility)
    * [1.2 Citation](#12-citation)
* [2 Installation](#2-installation)
    * [2.1 Install Miniconda](#21-install-miniconda)
    * [2.2 Create Conda Environment](#22-create-conda-environment)
    * [2.3 Create Launcher](#23-create-desktop-launcher)
    * [2.4 Uninstall](#24-uninstall)
    * [2.5 Troubleshooting](#25-troubleshooting)
* [3 Using PhyloTrace](#3-using-phylotrace)
* [4 Roadmap](#4-roadmap)

## 1 Getting Started

The **user manual** containing detailed instruction and information is available at [www.phylotrace.com/user-manual](https://www.phylotrace.com/user-manual). 

### 1.1 Compatibility

#### Minimum System Requirements

| Component         | Description   |
| ------------------- | ------------- |
| Operating System    | Any Linux distribution capable of running R and Conda (e.g. Ubuntu, Fedora, Debian, ArchLinux, OpenSuse, etc.) |
| Web Browser | Compatible with latest version of major browsers (Chrome, Brave, Chromium, Opera, Vivaldi) |
| Storage 		  	  | ≥ 250 GB SSD/HDD |
| RAM				  | ≥ 8 GB |
| CPU  				  | Multi-core processor, ≥ 2.5 GHz |
>*These system requirements are provided as estimates. It is possible for the application to run on lower-spec systems, depending on application workload and usage patterns.*

>*So far there is no compatibility with the **Firefox** browser.*

PhyloTrace was tested on the following Linux distributions:

| Distribution  | Version  |
| ------------- | ------------- |
| Debian | 12 |
| Fedora | 39 |
| Ubuntu | 22.04 |
| Linux Mint | 21.3 |
| MX Linux | 23.2 |

### 1.2 Citation

If you use PhyloTrace for your paper or publication, please give us credit by citing:

- *Freisleben, M. & Paskali, F. (2024). PhyloTrace. Zenodo. DOI: 10.5281/zenodo.11121153.*

*In Bibtex format:*
```
@software{Freisleben_Paskali_2024,
  author       = {Freisleben, Marian and Paskali, Filip},
  title        = {PhyloTrace},
  year         = {2024},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.11121153},
  url          = {https://doi.org/10.5281/zenodo.11121153}
}
```

## 2 Installation

Download the source code of the latest release [Version 1.3.0](https://github.com/infinity-a11y/PhyloTrace/archive/refs/heads/master.zip) and extract/unzip it to a location on your system.

### 2.1 Install Miniconda
Is ***Miniconda*** or another ***Conda Distribution*** installed on the system?
- No: Run the installation below and initialize conda. 
- Yes: Proceed to [2.2 Create Conda Environment](#22-create-conda-environment).
  
These four commands quickly and quietly install the latest 64-bit version of Miniconda and then clean up after themselves.
>For installation of Miniconda on system with different architecture, please refer to the [Miniconda documentation](https://docs.anaconda.com/free/miniconda/index.html)

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

### 2.2 Create Conda Environment
Create a conda environment containing necessary dependencies and packages.
```bash
cd path/to/directory
conda env create -f PhyloTrace.yml
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*
>*This process might take a while (depends on system performance).*

### 2.3 Create Desktop Launcher

Run install script to generate a run script and include PhyloTrace desktop icon in the Applications Menu.
 
```bash
cd path/to/directory
bash install_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

### 2.4 Uninstall
To uninstall PhyloTrace from your system, remove the application directory and run the following command to remove the
desktop launcher:
```bash
rm $HOME/.local/share/applications/PhyloTrace.desktop
rm $HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace.png
```

### 2.5 Troubleshooting

There are multiple possible sources for issues with the installation. Common mistakes during the installation are: 
- Change `path/to/directory` in the command chunks with the actual path of the repository containing all PhyloTrace files
- Before installation make sure the whole repository is unzipped to a writable location in your system

If the installation issues persist feel free to contact us via [info@phylotrace.com](mailto:info@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans) or open an issue.

#### Desktop Launcher not Working
If the desktop launcher is not working, the currently set default browser is likely not declared properly. To resolve this, change the R_BROWSER environment variable to the executable name of the desired browser (executable names for popular browsers are listed in the table below). For example to use Google Chrome, execute the following command:
```bash
cd path/to/directory
R_BROWSER=google-chrome
bash run_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

| Browser  | *browser-name*  |
| ------------- | ------------- |
| Google Chrome  | *google-chrome* **or** *google-chrome-stable*  |
| Chromium  | *chromium*  |
| Brave  | *brave-browser*  |
| Opera  | *opera*  |
| Vivaldi  | *vivaldi*  |

## 3 Using PhyloTrace

Start PhyloTrace by using the launcher in Applications Menu. A tab with the app will automatically open in the default browser. 

The **user manual** containing documentation is available at [www.phylotrace.com/user-manual](https://www.phylotrace.com/user-manual). 

**Alternative Launching:**

If launching from desktop does not work, the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory
bash run_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

## 4 Roadmap
PhyloTrace is under active development with new functions, enhancements and innovations to follow. In addition to the points listed on the roadmap, we constantly improve existing features.

✅ ***"Hello World!"*** - Version 1.0.0   
✅ Sophisticated MST Graphs (e.g. Pie Charts as Nodes)  
🔲 Hash-based Decentralized cgMLST  
🔲 MST Graph Clustering  
🔲 Support for additional cgMLST scheme databases (e.g. pubMLST, EnteroBase, Institut Pasteur)  
🔲 Implementation of a NGS Assembly Pipeline  
🔲 Backwards Compatibility with MLST (ST calling)  
🔲 Compatibility with Microsoft Windows  
🔲 Implementation of wgMLST  
🔲 Implementation of a Gene Enrichment Analysis  
🔲 Validation of PhyloTrace for Clinical Use  
