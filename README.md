![PhyloTrace](www/PhyloTrace_bw.png#gh-light-mode-only)
![PhyloTrace](www/PhyloTrace.png#gh-dark-mode-only)

PhyloTrace is a platform for bacterial pathogen monitoring on a genomic level. Its components evolve around allele variant typing, to be more precise: core-genome multi locus sequence typing (cgMLST). Complex analyses and computation are wrapped into an appealing and easy-to-handle graphical user interface. This tool can help to reveal patterns explaining outbreak dynamics and events by connecting genomic information with epidemiologic variables. 

**Features**

- Interactive cgMLST analysis also for large datasets comprising multiple genome assemblies
- Managing a custom local database 
- Connecting informative meta data and custom variables
- Visualizing the relationship between bacterial isolates with phylogenetic trees
- Download and report functionalities
- *and more*

We want to make cgMLST analysis and genomic pathogen monitoring accessible to a broad spectrum of individuals and organizations. Therefore our goal is to build an interface with convenient user experience and easy handling that doesn't require you to be a bioinformatician. The app is in active development. To get a stable version download only the newest release and not the GitHub version. This tool can be used for research and academic purposes. Note that PhyloTrace is not validated for a clinical use yet, but we are working on achieving that.

[www.PhyloTrace.com](https://www.phylotrace.com)  |  [info@phylotrace.com](mailto:info@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans)

![PartnerLogos](www/partners_logo_round.svg)

<sup><sup> Developed in collaboration with Hochschule Furtwangen University (HFU) and Medical University of Graz (MUG). Featured on ShinyConf 2024. </sup> </sup>

---

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


## 1 Getting Started

The **user manual** is available at [www.phylotrace.com/user-manual](https://www.phylotrace.com/user-manual). 

### 1.1 Gallery
PhyloTrace follows a straight-forward workflow. Note, that although these steps seem very simplistic, the app offers extensive customization options. The database can interactively managed, allowing to edit meta data, add custom variables, delete entries and download it to external files. 

**1. Type New Bacterial Assembly**

**2. Add Results to Database**

**3. Visualize Results**

### 1.2 Compatibility

#### Minimal System Requirements
- CPU: Intel i3-4005U
- RAM: 8GB
- OS: Debian 12
- Storage: ...

Additionally, PhyloTrace was tested on the following Linux distribution:
- Ubuntu 22.04
- Debian 12
- Fedora 39

#### Browser Compatibility
PhyloTrace is compatible with any modern browser. For best compatibility and user-exerience we recommend one of the following browsers:
- Google Chrome
- Chromium
- Brave
- Opera
- Vivaldi

### 1.3 Citation

If you use PhyloTrace for your paper or publication, please give us credit by citing:

- *Freisleben, M. & Paskali, F. (2024). PhyloTrace. Zenodo. DOI: 10.1234/zenodo.xxxxxxx.*

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

## 2 Installation

Download the repository as `.zip` and unpack it to a location on your system, or use git clone.

Is ***Miniconda*** or another ***Conda Distribution*** installed on the system?
- No: Run the installation below and initialize conda. 
- Yes: Proceed to [2.2 Create Conda Environment](#22-create-conda-environment).

### 2.1 Install Miniconda
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

Run install script to generate a run script and include desktop launcher for PhyloTrace in the Applications Menu.
 
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
cd path/to/directory
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

## 3 Using PhyloTrace

Start PhyloTrace by using the launcher in Applications Menu. A tab with the app will automatically open in the default browser. 

The **user manual** containing documentation is available at [www.phylotrace.com/user-manual](https://www.phylotrace.com/user-manual). 

**Alternative Launching:**

If launching from dekstop does not work, the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory
bash run_phylotrace.sh
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*

## 4 Roadmap
PhyloTrace is under active development with new functions, enhancements and innovations to follow. Next to the points listed on the roadmap we constantly improve existing features.

- [X] ***"Hello World!"*** - Completion Version 1.0.0
- [ ] Hash-Based Variant Calling
- [ ] Support for additional cgMLST scheme databases (e.g. pubMLST, EnteroBase, Institut Pasteur)  
- [ ] MST Graph Clustering
- [ ] Backwards Compatibility with MLST (ST calling)
- [ ] Compatibility with Microsoft Windows
- [ ] Implementation of wgMLST  
- [ ] More sophisticated MST Graphs (e.g. Clustering & Pie Charts as Nodes)
- [ ] Implementation of a NGS Assembly Pipeline
- [ ] Validation of PhyloTrace for Clinical Use
- [ ] Implementation of a Gene Enrichment Analysis 
