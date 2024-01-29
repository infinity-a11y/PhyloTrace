# Install and load renv (if not already installed)
if (!require("renv", quietly = TRUE, lib.loc = "~/R-4.3.2/library")) {
  install.packages("renv", lib = "~/R-4.3.2/library", dependencies=TRUE,
                   repos = "https://cloud.r-project.org/")
}

library(renv, lib.loc = "~/R-4.3.2/library")

# Initialize and restore the renv environment
renv::init(force = T)
renv::restore()
