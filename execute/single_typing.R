meta_info <- readRDS("meta_info_single.rds")

setwd(meta_info$db_directory)

# List all the .frag.gz files in the folder
frag_files <-
  list.files(paste0(getwd(), "/execute/kma_single/results"),
             pattern = "\\.frag\\.gz$",
             full.names = TRUE)

# List to store data frames
frag_data_list <- list()

# Initialize an empty vector to store the results
allele_vector <- integer(length(frag_files))

for (i in 1:length(frag_files)) {
  # Extract the base filename without extension
  frag_filename <-
    gsub(".frag", "", tools::file_path_sans_ext(basename(frag_files[i])))
  
  # Check if the file is empty
  if (file.info(frag_files[i])$size < 100) {
    # Handle empty file: Insert NA in the allele_vector and create an empty data frame
    allele_vector[i] <- NA
  } else {
    # Read the .frag.gz file into a data table
    frag_data <-
      data.table::fread(frag_files[i], sep = "\t", header = FALSE)
    
    # Extract the third, and seventh columns
    frag_data <- frag_data[, .(V3, V7)]
    
    # Find the row with the highest value in the third field
    max_row <- which.max(frag_data$V3)
    
    # Extract the value from the seventh field in the max row
    allele_vector[i] <- frag_data$V7[max_row]
  }
  
}

allele_vector <- as.integer(allele_vector)

# Find Alleles folder in directory
allele_folder <- list.files(paste0(getwd(), "/Database/", gsub(" ", "_", meta_info$cgmlst_typing)), full.names = TRUE)[grep("_alleles", list.files(paste0(getwd(), "/Database/", gsub(" ", "_", meta_info$cgmlst_typing))))]

# Create Results Data Frame 

if(!any(grepl("Typing", list.files(paste0(getwd(), "/Database/", gsub(" ", "_", meta_info$cgmlst_typing)))))) {
  
  Database <- list(Typing = data.frame())
  
  Typing <-
    data.frame(matrix(
      NA,
      nrow = 0,
      ncol = 12 + length(frag_files)
    ))
  
  metadata <-
    c(
      1,
      TRUE,
      meta_info$assembly_id,
      meta_info$assembly_name,
      meta_info$cgmlst_typing,
      as.character(meta_info$append_isodate),
      meta_info$append_host,
      meta_info$append_country,
      meta_info$append_city,
      as.character(meta_info$append_analysisdate),
      length(allele_vector) - sum(sapply(allele_vector, is.na)),
      sum(sapply(allele_vector, is.na))
    )
  
  new_row <- c(metadata, allele_vector)
  
  Typing <- rbind(Typing, new_row)
  
  
  colnames(Typing) <-
    append(
      c(
        "Index",
        "Include",
        "Assembly ID",
        "Assembly Name",
        "Scheme",
        "Isolation Date",
        "Host",
        "Country",
        "City",
        "Typing Date",
        "Successes",
        "Errors"
      ),
      gsub(".fasta", "", basename(list.files(allele_folder)))
    )
  
  Database[["Typing"]] <- Typing
  
  df2 <- dplyr::mutate_all(dplyr::select(Database$Typing, 13:(12+length(list.files(allele_folder)))), function(x) as.integer(x))
  
  df1 <- dplyr::select(Database$Typing, 1:12)
  df1 <- dplyr::mutate(df1, Include = as.logical(Include))
  
  Typing <- cbind(df1, df2)
  
  Database$Typing <- Typing
  
} else {
  
  Database <- readRDS(paste0(getwd(), "/Database/", gsub(" ", "_", meta_info$cgmlst_typing), "/Typing.rds"))
  
  metadata <-
    data.frame(
      nrow(Database[["Typing"]]) + 1,
      TRUE,
      meta_info$assembly_id,
      meta_info$assembly_name,
      meta_info$cgmlst_typing,
      as.character(meta_info$append_isodate),
      meta_info$append_host,
      meta_info$append_country,
      meta_info$append_city,
      as.character(meta_info$append_analysisdate),
      length(allele_vector) - sum(sapply(allele_vector, is.na)),
      sum(sapply(allele_vector, is.na))
    )
  
  df_profile <- data.frame(matrix(allele_vector, ncol = length(allele_vector)))
  
  merged <- cbind(metadata, df_profile)
  
  colnames(merged) <-
    append(
      c(
        "Index",
        "Include",
        "Assembly ID",
        "Assembly Name",
        "Scheme",
        "Isolation Date",
        "Host",
        "Country",
        "City",
        "Typing Date",
        "Successes",
        "Errors"
      ),
      gsub(".fasta", "", basename(list.files(allele_folder)))
    )
  
  merged <- dplyr::mutate(merged, "Include" = as.logical(Include))
  
  Database$Typing <- rbind(Database$Typing, merged)
  
}

# Save new Entry in Typing Database
saveRDS(Database, paste0(getwd(), "/Database/", gsub(" ", "_", meta_info$cgmlst_typing), "/Typing.rds"))
