library(logr)
typing_meta <- readRDS(paste0(getwd(), "/single_typing_df.rds"))

# Function to log messages 
log.message <- function(log_file, message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n", file = log_file, append = TRUE)
}

logfile <- file.path(paste0(typing_meta$wd, "/logs/check_duplicate_single.log"))

log <- log_open(logfile, logdir = FALSE)

log_print("Initiated single typing fasta name duplicates check")

assembly <- typing_meta$genome

lines <- readLines(assembly)

names <- stringr::str_extract(lines[seq(1, length(lines), by = 3)], "^[^\\s]+")

# Test if there are duplicates
if(length(names) != length(unique(names))) {
  
  log_print(paste0("Duplicate(s) present in ", basename(assembly)))
  
  # add a number to the duplicates
  for(i in 1:length(names)) {
    if(sum(names == names[i]) > 1) {
      indices <- which(names == names[i])
      names[i] <- paste0(names[i], "_", indices[which(indices == i)])
    }
  }
  
  # substitute the respective lines in the file with the new names 
  for(i in 1:length(names)) {
    lines[3*i - 2] <- paste0(names[i])
  }
  
  # save the new assembly to working directory
  writeLines(lines, paste0(getwd(), "/blat_single/assembly.fasta"))
} else {
  writeLines(lines, paste0(getwd(), "/blat_single/assembly.fasta"))
}

log_close()
