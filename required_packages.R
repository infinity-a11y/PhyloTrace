# List of required packages
required_cran_packages <- c(
  "shiny",
  "R.utils",
  "igraph",
  "shinyWidgets",
  "shinydashboard",
  "dashboardthemes",
  "ggplot2",
  "ggnewscale",
  "ggplotify",
  "grid",
  "gridExtra",
  "ape",
  "tidyverse",
  "rlang",
  "tidytree",
  "shinyFiles",
  "dplyr",
  "downloader",
  "rvest",
  "rmarkdown",
  "knitr",
  "kableExtra",
  "fs",
  "data.table",
  "zoo",
  "ggnetwork",
  "rhandsontable",
  "proxy",
  "phangorn",
  "cowplot",
  "waiter",
  "viridis",
  "RColorBrewer",
  "bslib",
  "bsicons",
  "DT",
  "shinyBS",
  "openssl",
  "shinyjs",
  "logr",
  "BiocManager",
  "remotes"
)

required_bioconductor_packages <- c(
  "treeio",
  "ggtree",
  "ggtreeExtra",
  "Biostrings",
  "pwalign",
  "ComplexHeatmap"
)

required_github_packages <- c(
  "fpaskali/visNetwork"
)

# Install CRAN packages only if they are not already installed
for (package in required_cran_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos="https://cloud.r-project.org/")
  }
}

# Install Bioconductor packages only if they are not already installed
for (package in required_bioconductor_packages) {
  if (!require(package, character.only = TRUE)) {
    BiocManager::install(package)
  }
}

# Install GitHub packages only if they are not already installed
for (package_repo in required_github_packages) {
  package <- strsplit(package_repo, "/")[[1]][2]
  if (!require(package, character.only = TRUE)) {
    remotes::install_github(package)
  }
}

cat("All packages successfully installed!\n")
