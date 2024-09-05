# Function to summarize AMR profile for all isolates

args <- commandArgs(trailingOnly = TRUE)

summarize.AMR <- function(database, scheme) {
  isolates_path <- file.path(database, gsub(" ", "_", scheme), "Isolates")
  isolates <- list.files(isolates_path)
  
  loci <- character()
  amr_profile <- vector("list", length(isolates)) 
  
  for(i in seq_along(isolates)) {
    amr_profile_path <- file.path(isolates_path, isolates[i], "resProfile.tsv")
    
    if(file.exists(amr_profile_path)) {
      amr_results <- read.delim(amr_profile_path, stringsAsFactors = FALSE)
      gene_symbols <- unique(amr_results$Gene.symbol)  
      amr_profile[[i]] <- gene_symbols  
      loci <- union(loci, gene_symbols) 
    } else {
      amr_profile[[i]] <- character(0)  
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

summarize.AMR(args[1], args[2])
