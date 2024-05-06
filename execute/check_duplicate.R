library(stringr)

typing_meta <- readRDS(paste0(getwd(), "/single_typing_df.rds"))

assembly <- typing_meta$genome

lines <- readLines(assembly)

names <- str_extract(lines[seq(1, length(lines), by = 3)], "^[^\\s]+")

# Test if there are duplicates
if(length(names) != length(unique(names))){
  # add a number to the duplicates
  for(i in 1:length(names)){
    if(sum(names == names[i]) > 1){
      names[i] <- paste0(names[i], "_", which(names == names[i]))
    }
  }
  
  # substitute the respective lines in the file with the new names 
  for(i in 1:length(names)){
    lines[3*i - 2] <- paste0(names[i])
  }
  
  # save the new assembly to working directory
  writeLines(lines, paste0(getwd(), "/kma_single/assembly.fasta"))
} else {
  writeLines(lines, paste0(getwd(), "/kma_single/assembly.fasta"))
}
