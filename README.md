![My Image](www/PhyloTrace_bw.png)


##

Download the complete repository as .zip and extract it to any location on your system.
Follow these instructions to install the required dependencies.

## 1. Install R

Before installing the dependencies, ensure that you have R and RStudio installed on your system. Follow the instructions below:

1. Download the latest version of R for your operating system from [the CRAN website](https://cran.r-project.org/).
2. Install R by executing the downloaded file and following the installation prompts.

[Optional: Download RStudio IDE]

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

## 5 Running PhyloTrace
#### 5.1 Running from Terminal (no RStudio required)


1. Navigate to the local directory
```bash
cd <phylotrace-directory>
```
>Note: In the command above, replace *phylotrace-directory* with the path where you extracted the repository to. 

2. Start the app by running this command from within the directory
```bash
Rscript PhyloTrace_App.R
```

#### 5.1 Running from RStudio

Open PhyloTrace.R from its directory with RStudio and start it by clicking "Run App" (make sure to tick "Run External" to open it in the default browser).

![My Image](www/run_external.png)
