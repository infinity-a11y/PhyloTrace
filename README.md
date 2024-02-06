<picture>
    <source media="(prefers-color-scheme: light)" srcset="www/PhyloTrace_bw.png">
    <source media="(prefers-color-scheme: dark)" srcset="www/PhyloTrace.png">    
    <img src= "www/">
</picture>



### Table of Content

* [1 Installation](#1-installation)
    * [1.1 Install System Libraries](#11-install-system-librariespackages)
    * [1.2 Induce Conda Environment](#12-induce-conda-environment)
    * [1.3 Create Desktop Launcher](#13-create-desktop-launcher)
* [2 Running PhyloTrace](#2-running-phylotrace)
* [3 Troubleshooting](#3-troubleshooting)
    * [3.1 General](#31-general)
    * [3.2 Desktop Launcher](#32-desktop-launcher-not-working)
* [4 Roadmap](#4-roadmap)

## 1 Installation

Download the repository as `.zip` and unpack it to a location on your system.  

### 1.1 Install System Libraries/Packages

```bash
sudo apt-get update && sudo apt-get install \
    build-essential \
    libssl-dev \
    libbz2-dev \
    zlib1g-dev \
    liblapack-dev \
    libblas-dev \
    libmkl-rt \
    libopenblas-dev \
    gfortran \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    xdg-utils \
    xorg-dev \
    automake \
    pandoc \
    wget
```

### 1.2 Induce Conda Environment 

Is ***Miniconda*** installed on the system?

- Yes: Run the installation below and initialize conda. 
- No: Skip this code chunk and proceed to the creation of a new environment as the next step.  

```bash
cd ~ \
&& wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh \
&& bash ~/miniconda.sh -b -u \
&& conda init 
```

Create a conda evironment containing necessary dependencies and packages.
```bash
cd path/to/directory \
&& conda env create -f PhyloTrace.yml
```
>*In the command above, replace `path/to/directory` with the actual path linking to the PhyloTrace directory on your system.*
>*This process might take a while (depends on system capacities).*

### 1.3 Create Desktop Launcher

Construct an executable `.desktop` file to create a desktop launcher for PhyloTrace. 
```bash
cd path/to/directory \
&& echo "[Desktop Entry]" > PhyloTrace.desktop \
&& echo "Name=PhyloTrace" >> PhyloTrace.desktop \
&& echo "Exec=run_phylotrace.sh">> PhyloTrace.desktop \
&& echo "Icon=$(pwd)/www/phylo.png" >> PhyloTrace.desktop \
&& echo "Terminal=true" >> PhyloTrace.desktop \
&& echo "Type=Application" >> PhyloTrace.desktop \
&& echo "Categories=Utility;" >> PhyloTrace.desktop \
&& echo "# Run the R script" > run_phylotrace.sh \
&& echo "conda activate" >> run_phylotrace.sh \
&& echo "Rscript $(pwd)/PhyloTrace.R" >> run_phylotrace.sh \
&& sed -i '1s/^/#!\/bin\/bash\n/' run_phylotrace.sh \
&& sudo mv PhyloTrace.desktop /usr/share/applications/ \
&& sudo mv run_phylotrace.sh /usr/bin/ \
&& sudo chmod a+x /usr/share/applications/PhyloTrace.desktop \
&& sudo chmod a+x /usr/bin/run_phylotrace.sh \
&& cd ~
```

>*If this does not work, use the alternative way of starting PhyloTrace as described in # 2 Running PhyloTrace.*

## 2 Running PhyloTrace

Start PhyloTrace by pressing the *Super* key or open *Activities* and search for 'PhyloTrace'. A tab with the app will automatically open in the default browser.

**Alternative:**

If this doesn't work the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory \
&& conda activate PhyloTrace \
&& Rscript PhyloTrace.R 
```
>*In the command above, replace path/to/directory with the actual path linking to the PhyloTrace directory on your system.*

## 3 Troubleshooting

### 3.1 General
There are multiple possible sources for issues with the installation. Common mistakes during the installation are: 
- Change `path/to/directory` in the command chunks with the actual path of the repository containing all PhyloTrace files
- Before installation make sure the whole repository is unzipped to a writable location in your system
- Sometimes system libraries/dependencies needed for PhyloTrace are not installed or updated [1.1 Install System Libraries/Packages](#11-install-system-librariespackages)

If the installation issues persist feel free to contact us via [contact@phylotrace.com](mailto:contact@phylotrace.com?subject=[GitHub]%20Source%20Han%20Sans) or open an issue.

### 3.2 Desktop Launcher not Working
In some cases the default browser is not accessible from shell. In order to manually denote the browser, substitute *browser-name* in the command below with executable name of the browser you want to use. If you are unsure about the correct identifier for your browser consider the table below. 

```bash
cd /usr/bin/ \
&& sudo sed -i '3 s/.*/export R_BROWSER=browser-name/' run_phylotrace.sh \
&& cd ~
```

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






