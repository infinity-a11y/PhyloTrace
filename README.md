<picture>
    <source media="(prefers-color-scheme: light)" srcset="www/PhyloTrace_bw.png">
    <source media="(prefers-color-scheme: dark)" srcset="www/PhyloTrace.png">    
    <img src= "www/">
</picture>

##

# 1 Installation

## 1.1 Install System Libraries

Before installing R make sure that you have the following packages installed.
>This process might take a while (depends on system capacities).

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

Follow the instructions below to install the newest R Version.

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
&& cd ~
``

## 1.4 Initialize PhyloTrace

Download the complete Github repository as .zip and extract it to any location on your system.
Follow these instructions to install the required dependencies.

Before using PhyloTrace for the first time some R packages and dependencies need to be installed.
```bash
cd path/to/directory \
&& Rscript init.R
```
>Note: In the command above, replace *path/to/directory* with the actual path where you extracted the repository to.
>Note: This process might take a while (depends on system capacities).

## 2 Running PhyloTrace

Navigate to the local directory.
```bash
cd path/to/directory
```
>Note: In the command above, replace *path/to/directory* with the actual path where you extracted the repository to.

```bash
Rscript PhyloTrace.R
```
