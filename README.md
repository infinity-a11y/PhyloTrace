<picture>
    <source media="(prefers-color-scheme: light)" srcset="www/PhyloTrace_bw.png">
    <source media="(prefers-color-scheme: dark)" srcset="www/PhyloTrace.png">    
    <img src= "www/">
</picture>

##

# 1 Installation

## 1.1 Install System Libraries

Before installing R make sure that you have the following packages installed.
>*This process might take a while (depends on system capacities).*

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
    xorg-dev \
    automake \
    pandoc \
    wget
```


## 1.2 Install R

***>>>  In order for PhyloTree to work the newest R version is absolutely essential  <<<***

Follow the instructions below to install version ***4.3.2***.

```bash
cd ~ \
 && wget https://cran.r-project.org/src/base/R-4/R-4.3.2.tar.gz \
 && tar xvf R-4.3.2.tar.gz \
 && rm R-4.3.2.tar.gz \
 && cd R-4.3.2 \
 && mkdir library \
 && ./configure --prefix=$HOME \
 && make && make install \
 && cd ~
```

## 1.3 Install Miniconda & KMA package 

```bash
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh \
&& bash ~/miniconda.sh -b -u \
&& ~/miniconda3/bin/conda create --name PhyloTrace \
&& source ~/miniconda3/bin/activate PhyloTrace \
&& conda install -n PhyloTrace -c bioconda kma \
&& conda init \
&& cd ~
```

## 1.4 Initialize PhyloTrace

Download the complete Github repository as .zip and extract it to any location on your system.
Follow these instructions to install the required dependencies.

Before using PhyloTrace for the first time some R packages and dependencies need to be installed.
```bash
cd path/to/directory \
&& Rscript init.R 
```
>*In the command above, replace path/to/directory with the actual path where you extracted the repository to.*
>*This process might take a while (depends on system capacities).*

Create an executable .desktop file to create a desktop application-like searchable software. 
```bash
echo "[Desktop Entry]" >> PhyloTrace.desktop \
&& echo "Name=PhyloTrace" >> PhyloTrace.desktop \
&& echo "Exec=run_phylotrace.sh">> PhyloTrace.desktop \
&& echo "Icon=$(pwd)/www/phylo.png" >> PhyloTrace.desktop \
&& echo "Terminal=true" >> PhyloTrace.desktop \
&& echo "Type=Application" >> PhyloTrace.desktop \
&& echo "Categories=Utility;" >> PhyloTrace.desktop \
&& echo -e "cd '$(pwd)'\n\n# Run the R script\nRscript $(pwd)/PhyloTrace.R" > run_phylotrace.sh \
&& sed -i '1s/^/#!\/bin\/bash\n/' run_phylotrace.sh \
&& sudo mv PhyloTrace.desktop /usr/share/applications/ \
&& sudo mv run_phylotrace.sh /usr/bin/ \
&& sudo chmod a+x /usr/share/applications/PhyloTrace.desktop \
&& sudo chmod a+x /usr/bin/run_phylotrace.sh \
&& cd ~
```

>*If this doesn't work, use the alternative way of starting PhyloTrace as described in # 2 Running PhyloTrace.*

# 2 Running PhyloTrace

Start PhyloTrace by pressing the *Super* key or open *Activities* and search for 'PhyloTrace'. A tab with the app will automatically open in the default browser.

**Alternative:**

If this doesn't work the alternative way to run the app is to execute these commands:
```bash
cd path/to/directory \
&& conda activate PhyloTrace \
&& Rscript PhyloTrace.R 
```
>*In the command above, replace path/to/directory with the actual path where you extracted the repository to.*

