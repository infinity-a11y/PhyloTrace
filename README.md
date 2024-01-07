![My Image](www/PhyloTrace_bw.png)

# Installation Instructions for PhyloTrace 1.0.0

Follow these instructions to install and run the Shiny app on a Linux system.

## 1. Install R and RStudio

Before installing the dependencies, ensure that you have R and RStudio installed on your system. Follow the instructions below:

1. Download R for your operating system from [the CRAN website](https://cran.r-project.org/).
2. Install R by executing the downloaded file and following the installation prompts.
3. Download RStudio from [the RStudio website](https://rstudio.com/products/rstudio/download/).
4. Install RStudio by executing the downloaded file and following the installation prompts.

## 2. Install System Libraries

```bash
sudo apt-get update && sudo apt-get install -y \
    zlib1g-dev \
    liblapack-dev \
    libblas-dev \
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
    libz-dev \
    pandoc \
    wget
```


## 3. Install Miniconda

```bash
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /miniconda.sh \
    && bash /miniconda.sh -b -u -p /miniconda3 \
    && rm /miniconda.sh \
    && /miniconda3/bin/conda init bash
```

## 4. Install KMA via Conda

```bash
/miniconda3/bin/conda install -c bioconda kma
```


