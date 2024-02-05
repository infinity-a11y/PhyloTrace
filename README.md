<picture>
    <source media="(prefers-color-scheme: light)" srcset="www/PhyloTrace_bw.png">
    <source media="(prefers-color-scheme: dark)" srcset="www/PhyloTrace.png">    
    <img src= "www/">
</picture>

##

# 1 Installation

## 1.1 Install System Libraries/Packages

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

## 1.2 Induce Conda Environment 

Is ***Miniconda*** installed on the system?

- Yes: Run the installation below and initialize conda. 
- No: Skip this code chunk and proceed to the next one.  

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
>*In the command above, replace path/to/directory with the actual path linking to the PhyloTrace directory on your system.*
>*This process might take a while (depends on system capacities).*

## 1.3 Create Desktop Launcher

Construct an executable *.desktop* file to create a desktop launcher for PhyloTrace. 
```bash
echo "[Desktop Entry]" >> PhyloTrace.desktop \
&& echo "Name=PhyloTrace" >> PhyloTrace.desktop \
&& echo "Exec=run_phylotrace.sh">> PhyloTrace.desktop \
&& echo "Icon=$(pwd)/www/phylo.png" >> PhyloTrace.desktop \
&& echo "Terminal=true" >> PhyloTrace.desktop \
&& echo "Type=Application" >> PhyloTrace.desktop \
&& echo "Categories=Utility;" >> PhyloTrace.desktop \
&& echo -e "cd '$(pwd)'\n\n# Run the R script\n~/miniconda3/bin/conda activate PhyloTrace\nRscript $(pwd)/PhyloTrace.R" > run_phylotrace.sh \
&& sed -i '1s/^/#!\/bin\/bash\n/' run_phylotrace.sh \
&& sudo mv PhyloTrace.desktop /usr/share/applications/ \
&& sudo mv run_phylotrace.sh /usr/bin/ \
&& sudo chmod a+x /usr/share/applications/PhyloTrace.desktop \
&& sudo chmod a+x /usr/bin/run_phylotrace.sh \
&& cd ~
```

>*If this does not work, use the alternative way of starting PhyloTrace as described in # 2 Running PhyloTrace.*

# 2 Running PhyloTrace

Start PhyloTrace by pressing the *Super* key or open *Activities* and search for 'PhyloTrace'. A tab with the app will automatically open in the default browser.


**Alternative:**

If this doesn't work the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory \
&& conda activate PhyloTrace \
&& Rscript PhyloTrace.R 
```
>*In the command above, replace path/to/directory with the actual path linking to the PhyloTrace directory on your system.*

