meta_info <- readRDS("meta_info.rds")
db_path <- readRDS("multi_typing_df.rds")[, "db_path"]

setwd(meta_info$db_directory)

#Function to check custom variable classes
column_classes <- function(df) {
  sapply(df, function(x) {
    if (class(x) == "numeric") {
      return("(cont.)")
    } else if (class(x) == "character") {
      return("(categ.)")
    } else {
      return(class(x))
    }
  })
}


typing_genome <- tail(list.files(paste0(getwd(), "/execute/kma_multi/results")), n = 1)
typing_genome_full <- tail(list.files(paste0(getwd(), "/execute/kma_multi/results"), full.names = TRUE), n = 1)

# List all the .frag.gz files in the folder
frag_files <-
  list.files(typing_genome_full,
             pattern = "\\.frag\\.gz$",
             full.names = TRUE)

# List to store data frames
frag_data_list <- list()

# Initialize an empty vector to store the results
allele_vector <- integer(length(frag_files))

if(sum(unname(base::sapply(frag_files, file.size)) <= 100) / length(frag_files) < 0.5) {
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
  allele_folder <- list.files(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing)), full.names = TRUE)[grep("_alleles", list.files(paste0(getwd(), "/Database/", gsub(" ", "_", meta_info$cgmlst_typing))))]
  
  # Create Results Data Frame 
  
  if(!any(grepl("Typing", list.files(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing)))))) {
    
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
        typing_genome,
        typing_genome,
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
    
    Database <- readRDS(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing), "/Typing.rds"))
    
    metadata <-
      data.frame(
        nrow(Database[["Typing"]]) + 1,
        TRUE,
        typing_genome,
        typing_genome,
        meta_info$cgmlst_typing,
        as.character(meta_info$append_isodate),
        meta_info$append_host,
        meta_info$append_country,
        meta_info$append_city,
        as.character(meta_info$append_analysisdate),
        length(allele_vector) - sum(sapply(allele_vector, is.na)),
        sum(sapply(allele_vector, is.na))
      )
    
    if ((ncol(Database$Typing)-12) != length(allele_vector)) {
      
      cust_var <- dplyr::select(Database$Typing, 13:(ncol(Database$Typing) - length(allele_vector)))
      cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
      
      class_df <- data.frame()
      for (i in 1:nrow(cust_var)) {
        if(cust_var$Type[i] == "(categ.)") {
          class_df[1, i] <- ""
        } else {
          class_df[1, i] <- 0
        }
      }
      
      metadata <- cbind(metadata, class_df)
    } 
    
    df_profile <- data.frame(matrix(allele_vector, ncol = length(allele_vector)))
    
    merged <- cbind(metadata, df_profile)
    
    if ((ncol(Database$Typing)-12) != length(allele_vector)) {
      names_vec <- character(0)
      # Add new columns to df1
      for (i in 1:nrow(cust_var)) {
        names_vec[i]<- cust_var$Variable[i]
      }
      
      colnames(merged) <-
        c(
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
          names_vec,
          gsub(".fasta", "", basename(list.files(allele_folder)))
        )
    } else {
      colnames(merged) <-
        c(
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
    }
    
    merged <- dplyr::mutate(merged, "Include" = as.logical(Include))
    
    Database$Typing <- rbind(Database$Typing, merged)
    
  }
  
  # Save new Entry in Typing Database
  saveRDS(Database, paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing), "/Typing.rds"))
  
  multi_user_fb <- paste0(
    "#!/bin/bash\n",
    'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
    '# Function to log messages to the file', '\n',
    'log_message() {', '\n',
    '    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"', '\n',
    '}', '\n',
    'log_message "Successful typing of "', shQuote(typing_genome)
  )
  
  # Specify the path to save the script
  multi_user_fb_path <- paste0(getwd(), "/execute/multi_user_fb.sh")
  
  # Write the script to a file
  cat(multi_user_fb, file = multi_user_fb_path)
  
  # Make the script executable
  system(paste("chmod +x", multi_user_fb_path))
  
  # Execute the script
  system(paste(multi_user_fb_path), wait = FALSE)
  
} else {
  
  failures <- sum(unname(base::sapply(frag_files, file.size)) <= 100) / length(frag_files) * 100
  
  multi_user_fb <- paste0(
    "#!/bin/bash\n",
    'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
    '# Function to log messages to the file', '\n',
    'log_message() {', '\n',
    '    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"', '\n',
    '}', '\n',
    'log_message ', shQuote(paste0("Assembly typing failed for ", typing_genome)), '\n',
    shQuote(paste0(failures, "% of loci not typed."))
  )
  
  # Specify the path to save the script
  multi_user_fb_path <- paste0(getwd(), "/execute/multi_user_fb.sh")
  
  # Write the script to a file
  cat(multi_user_fb, file = multi_user_fb_path)
  
  # Make the script executable
  system(paste("chmod +x", multi_user_fb_path))
  
  # Execute the script
  system(paste(multi_user_fb_path), wait = FALSE)
}


