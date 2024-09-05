# Function to summarize AMR profile for all isolates
args <- commandArgs(trailingOnly = TRUE)

summarize.AMR <- function(database, scheme) {
  isolates_path <- file.path(database, gsub(" ", "_", scheme), "Isolates")
  isolates_full <- list.files(isolates_path, full.names = TRUE)
  available <- sapply(isolates_full, function(isolate) file.exists(file.path(isolate, "amrfinder.out")))
  isolates <- basename(isolates_full[available])
  
  loci <- character()
  amr_profile <- list()
  
  for(i in seq_along(isolates)) {
    amr_profile_path <- file.path(isolates_path, isolates[i], "amrfinder.out")
    if(file.exists(amr_profile_path)) {
      amr_results <- read.delim(amr_profile_path, stringsAsFactors = FALSE)
      gene_symbols <- unique(amr_results$Gene.symbol)  
      amr_profile[[i]] <- gene_symbols  
      loci <- union(loci, gene_symbols) 
    }
  }
  
  df <- matrix(FALSE, nrow = length(isolates), ncol = length(loci))
  colnames(df) <- loci
  rownames(df) <- isolates
  
  for(i in seq_along(amr_profile)) {
    df[i, loci %in% amr_profile[[i]]] <- TRUE
  }
  
  return(as.data.frame(df))
}

output_folder <- file.path(args[1], gsub(" ", "_", args[2]), "AMR_Profile.rds") 
amr_profile <- summarize.AMR(args[1], args[2])
saveRDS(amr_profile, output_folder)
