# Summarize AMR profile for all isolates
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

source(file.path(args[3], "assets/functions.R"))

output_folder <- file.path(args[1], gsub(" ", "_", args[2]), "AMR_Profile.rds") 

amr_profile <- summarize.AMR(args[1], args[2])

saveRDS(amr_profile, output_folder)
