library(stringr)

typing_meta <- readRDS(paste0(getwd(), "/single_typing_df.rds"))

assembly <- typing_meta$genome

lines <- readLines(assembly)

first <- str_extract(lines[seq(1, length(lines), by = 3)], "^[^\\s]+")

# Test if there are duplicates
if(length(first) != length(unique(first))){
  # add a number to the duplicates
  for(i in 1:length(first)){
    if(sum(first == first[i]) > 1){
      first[i] <- paste0(first[i], "_", which(first == first[i]))
    }
  }
  
  # substitute the respective lines in the file with the new names 
  for(i in 1:length(first)){
    lines[3*i - 2] <- paste0(first[i])
  }
  
  # save the new assembly to working directory
  writeLines(lines, paste0(getwd(), "/kma_single/assembly.fasta"))
} else {
  writeLines(lines, paste0(getwd(), "/kma_single/assembly.fasta"))
}
