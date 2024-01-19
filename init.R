# Install and load renv (if not already installed)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize and restore the renv environment
renv::init()
renv::restore()
