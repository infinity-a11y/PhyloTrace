<picture>
    <source media="(prefers-color-scheme: light)" srcset="www/PhyloTrace_bw.png">
    <source media="(prefers-color-scheme: dark)" srcset="www/PhyloTrace.png">
    <img src= "www/">
</picture>
<br><br><br><br>

PhyloTrace is a platform for fully interactive bacterial typing, to be more precise: core-genome multi locus sequence typing (cgMLST) analysis, and management of a local database. It enables to get insight in the genomic relationship of a set of bacterial isolates and helps to reveal patterns explaining underlying spread dynamics and outbreaks by connecting epidemiologic variables. This tool can be used for research and academic purposes. Note that PhyloTrace is not validated for a clinical use yet, but we are working on achieving that.

<br>

[www.PhyloTrace.com](https://www.phylotrace.com)  |  [info@phylotrace.com](mailto:info@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans)

<br>

<div style="display: flex; justify-content: center; align-items: center; margin-top: 50px; margin-bottom: -30px;">
    <img align="center" src="www/featured.png" alt="Partner/Feature 1" width="70%">
</div>

<sup> Developed in collaboration with Hochschule Furtwangen University (HFU) and Medical University of Graz (MUG). Featured on ShinyConf 2024. </sup> 

<br>
<hr>
<br>

## Table of Content

* [1 Getting Started](#1-getting-started)  
    * [1.1 Gallery](#11-gallery)
    * [1.2 Compatibility](#12-compatibility)
    * [1.3 Citation](#13-citation)
* [2 Installation](#2-installation)
    * [2.1 Install Miniconda](#21-install-miniconda)
    * [2.2 Create Conda Environment](#22-create-conda-environment)
    * [2.3 Create Launcher](#23-create-desktop-launcher)
    * [2.4 Uninstall](#24-uninstall)
    * [2.5 Troubleshooting](#25-troubleshooting)
* [3 Using PhyloTrace](#3-using-phylotrace)
* [4 Roadmap](#4-roadmap)

<br>

## 1 Getting Started
PhyloTrace is a platform for fully interactive bacterial typing, to be more precise: core-genome multi locus sequence typing (cgMLST) analysis, and management of a local database. It enables to get insight in the genomic relationship of a set of bacterial isolates and helps to reveal patterns explaining underlying spread dynamics and outbreaks by connecting epidemiologic variables. This tool can be used for research and academic purposes. Note that PhyloTrace is not validated for a clinical use yet, but we are working on achieving that. 

<p></p>

The **user manual** is available at [www.phylotrace.com/user-manual](https://www.phylotrace.com/user-manual). 

### 1.1 Gallery
PhyloTrace follows a straight-forward workflow. Note, that although these steps seem very simplistic, the app offers extensive customization options. The database can interactively managed, allowing to edit meta data, add custom variables, delete entries and download it to external files. 

**1. Type New Bacterial Assembly**

**2. Add Results to Database**

**3. Visualize Results**

### 1.2 Compatibility

#### System Requirements
PhyloTrace does not require overly powerful hardware.  It was tested with a simple office notebook. However the speed of the typing analysis is dependent on the system capabilities.

#### Operating System
PhyloTrace is compatible for the following Linux distributions:
- Ubuntu
- Debian
- Fedora

#### Browser Compatibility
PhyloTrace is compatible with .... - only browsers, such as:
- Google Chrome
- Chromium
- Brave
- Opera
- Vivaldi
- ...

### 1.3 Citation

If you use PhyloTrace for your paper or publication, please give us credit by citing:

<br>

- *Freisleben, M. & Paskali, F. (2024). PhyloTrace. Zenodo. DOI: 10.1234/zenodo.xxxxxxx.*

<br>

*In Bibtex format:*
```
@software{Freisleben_Paskali_2024,
  author       = {Freisleben, Marian and Paskali, Filip},
  title        = {PhyloTrace},
  year         = {2024},
  publisher    = {Zenodo},
  doi          = {10.1234/zenodo.xxxxxxx},
  url          = {https://doi.org/10.1234/zenodo.xxxxxxx}
}
```

<br>

## 2 Installation

Download the repository as `.zip` and unpack it to a location on your system.

Is ***Miniconda*** or another ***Conda Distribution*** installed on the system?
- No: Run the installation below and initialize conda. 
- Yes: Proceed to [2.2 Create Conda Environment](#22-create-conda-environment).

### 2.1 Install Miniconda
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

### 2.2 Create Conda Environment
Create a conda environment containing necessary dependencies and packages.
```bash
cd path/to/directory
conda env create -f PhyloTrace.yml
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*
>*This process might take a while (depends on system capacities).*

### 2.3 Create Desktop Launcher

Create a run script and an `.desktop` file to create a desktop launcher for PhyloTrace. 
```bash
cd path/to/directory
bash install_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

### 2.4 Uninstall
To remove PhyloTrace from your system, remove the Phylotrace directory and run the following command to remove
Desktop Launcher:
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
If the default browser does not work properly, it can be changed, by changing R_BROWSER environment variable. For example, to use Google Chrome, use the following command:
```bash
R_BROWSER=google-chrome bash run_phylotrace.sh
```

>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

| Browser  | *browser-name*  |
| ------------- | ------------- |
| Google Chrome  | *google-chrome* **or** *google-chrome-stable*  |
| Chromium  | *chromium*  |
| Brave  | *brave-browser*  |
| Opera  | *opera*  |
| Vivaldi  | *vivaldi*  |

<sub>Executable names for some popular browsers.</sub>

If PhyloTrace is still unable to launch from desktop, a missing default browser was likely not the issue. In this case either open PhyloTrace as described in [3 Using PhyloTrace](#3-using-phylotrace) or contact us via [info@phylotrace.com](mailto:info@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans).

<br>

## 3 Using PhyloTrace

Start PhyloTrace by using the launcher in Applications Menu. A tab with the app will automatically open in the default browser. 

The **user manual** containing documentation is available at [www.phylotrace.com/user-manual](https://www.phylotrace.com/user-manual). 

<br>

**Alternative Launching:**

If launching from dekstop does not work, the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory
bash run_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

<br>

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
