# Install and load renv (if not already installed)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", lib = "~/R-4.3.2/library")
}

# Initialize and restore the renv environment
renv::init()
renv::restore()
