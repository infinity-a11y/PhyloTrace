# Get the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Access the first argument
base_path <- args[1]

# Get selected assembly file names
file_names <- list.files(paste0(getwd(), "/selected_genomes"), full.names = T)

# Function to log messages 
log.message <- function(log_file, message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n", file = log_file, append = TRUE)
}

log.message(log_file = paste0(base_path, "/logs/output.log"),
            message = "Initiated multi typing fasta name duplicates check")

# load selected assemblies
assemblies <- lapply(list.files(paste0(getwd(), "/selected_genomes"), full.names = T), readLines)

# loop through every assembly
for(i in 1:length(assemblies)){
  names <- stringr::str_extract(assemblies[[i]][seq(1, length(assemblies[[i]]), by = 3)], "^[^\\s]+")
  
  # Test if there are duplicates
  if(length(names) != length(unique(names))){
    
    log.message(log_file = paste0(base_path, "/logs/output.log"),
                message = paste0("Duplicate(s) present in ", basename(file_names[i])))
    
    # add a number to the duplicates
    for(j in 1:length(names)){
      if(sum(names == names[j]) > 1){
        names[j] <- paste0(names[j], "_", which(names == names[j]))
      }
    }
    
    # substitute the respective lines in the file with the new names 
    for(k in 1:length(names)){
      assemblies[[i]][3*k - 2] <- paste0(names[k])
    }
    
    # save the new assembly 
    writeLines(assemblies[[i]], file_names[i])
  } 
}
