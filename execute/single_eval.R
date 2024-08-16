library(logr)

# Hand over variables
meta_info <- readRDS("meta_info_single.rds")
db_path <- readRDS("single_typing_df.rds")[, "db_path"]
save_assembly <- readRDS("single_typing_df.rds")[, "save"]
file_list <- list.files(paste0(meta_info$db_directory, "/execute/blat_single"), 
                        full.names = TRUE)
assembly <- file_list[which(list.files(paste0(meta_info$db_directory, "/execute/blat_single")) != "results")]

source("variant_validation.R")

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

# Function to log messages to the file
log.message <- function(log_file, message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- ", message, "\n", file = log_file, append = TRUE)
}

logfile <- file.path(paste0(getwd(), "/logs/single_eval.log"))

log <- log_open(logfile, logdir = FALSE)

log_print("Attaching initiated")

# Define start and stop codons
start_codons <- c("ATG", "GTG", "TTG")
stop_codons <- c("TAA", "TAG", "TGA")

# Locate Alleles folder in directory
allele_folder <- list.files(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing)), full.names = TRUE)[grep("_alleles", list.files(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing))))]

# Read the template (assembly) sequence
template <- readLines(assembly)

# List all .psl result files from alignment with BLAT
psl_files <- list.files(paste0(meta_info$db_directory, "/execute/blat_single/results"), pattern = "\\.psl$", full.names = TRUE)

# Initialize an empty vector to store the results
allele_vector <- character(length(psl_files))

event_df <- data.frame(Locus = character(0), Event = character(0), Value = character(0))

# if more than 5 % of loci are not typed, assembly typing is considered failed
if(sum(unname(base::sapply(psl_files, file.size)) <= 427) / length(psl_files) <= 0.05) {
  
  for (i in seq_along(psl_files)) {
    
    # Extract the base filename without extension
    allele_index <- gsub(".psl", "", tools::file_path_sans_ext(basename(psl_files[i])))
    
    # Check if the file is empty
    if (file.info(psl_files[i])$size <= 427) {
      
      # Handle empty file: Insert NA in the allele_vector
      allele_vector[[i]] <- NA
      
      event_df <- rbind(event_df, data.frame(Locus = allele_index, Event = "Locus Not Found", Value = "NA"))
      
    } else {
      
      matches <- data.table::fread(psl_files[i], select = c(1, 5, 6, 7, 8, 10, 11, 14, 16, 17), header = FALSE)
      
      if(any(matches$V1 == matches$V11 & (matches$V5 + matches$V7) == 0)) {
        
        perf_match <- matches[which(matches$V1 == matches$V11)]
        
        if(sum((perf_match$V5 + perf_match$V7) == 0) > 1) {
          
          cat(paste0(allele_index, " has multiple hits.\n"))
          allele_vector[[i]] <- NA
          event_df <- rbind(event_df, data.frame(Locus = allele_index, Event = "Multiple Hits", Value = "NA"))
          
        } else {
          allele_vector[[i]] <- perf_match$V10[which((perf_match$V5 + perf_match$V7) == 0)]
        }
      } else {
        
        # select allele fasta file to get present variants in scheme
        locus_file <- list.files(allele_folder, full.names = TRUE)[which(sub("\\.f(na|a|asta)$", "", list.files(allele_folder)) == allele_index)]
        
        variants <- readLines(locus_file)
        
        ### new variant validation ### 
        
        # sort by i) score, ii) number of gaps, iii) number of nt in the gaps
        matches <- dplyr::arrange(matches, desc(V1), desc(V5 + V7), desc(V6 + V7))
        
        # check which reference sequences have different alignment positions with the template
        unique_template_seq <- matches[which(!(duplicated(matches$V16) & duplicated(matches$V17)))]
        
        # loop over all unique template alignments (regarding position)
        variant_valid <- variant_validation(references = unique_template_seq, 
                                            start_codons = start_codons, stop_codons = stop_codons)
        
        # if valid variant found 
        if(variant_valid == "Ambigous Nucleotides") {
          allele_vector[[i]] <- NA
          event_df <- rbind(event_df, data.frame(Locus = allele_index, Event = "Ambigous Nucleotides", Value = "NA"))
          cat(paste0(allele_index, " Invalid - Ambigous Nucleotides.\n"))
          
        } else if(variant_valid != FALSE) {
          
          hashed_variant <- as.character(openssl::sha256(variant_valid))
          
          # Append new variant number to allele fasta file
          cat(paste0("\n>", hashed_variant), file = locus_file, append = TRUE)
          
          # Append new variant sequence to allele fasta file
          cat(paste0("\n", variant_valid, "\n"), file = locus_file, append = TRUE)
          
          # Entry in results data frame
          event_df <- rbind(event_df, data.frame(Locus = allele_index, Event = "New Variant", Value = hashed_variant))
          
          allele_vector[[i]] <- hashed_variant
          
          cat(paste0(allele_index, " has new variant.\n"))
          
        } else {
          
          #no valid variant found
          allele_vector[[i]] <- NA
          
          # Entry in results data frame
          event_df <- rbind(event_df, data.frame(Locus = allele_index, Event = "Invalid Sequence", Value = "NA"))
          
          cat(paste0(allele_index, " has invalid sequence.\n"))
        }
      }
    }
  }
  
  saveRDS(event_df, "execute/event_df.rds")
  
  # Create Results Data Frame 
  if(!any(grepl("Typing", list.files(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing)))))) {
    
    Database <- list(Typing = data.frame())
    
    Typing <- data.frame(matrix(NA, nrow = 0, ncol = 13 + length(psl_files)))
    
    if(!save_assembly) {screen <- "NA"} else {screen <- "No"}
    
    metadata <- c(
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
      sum(sapply(allele_vector, is.na)),
      screen
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
          "Errors",
          "Screened"
        ),
        gsub(".fasta", "", basename(list.files(allele_folder)))
      )
    
    Database[["Typing"]] <- Typing
    
    df2 <- dplyr::mutate_all(dplyr::select(Database$Typing, 14:(13+length(list.files(allele_folder)))), function(x) as.character(x))
    
    df1 <- dplyr::select(Database$Typing, 1:13)
    df1 <- dplyr::mutate(df1, Include = as.logical(Include))
    
    Typing <- cbind(df1, df2)
    
    Database$Typing <- Typing
    
  } else {
    
    Database <- readRDS(paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing), "/Typing.rds"))
    
    if(!save_assembly) {screen <- "NA"} else {screen <- "No"}
    
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
        sum(sapply(allele_vector, is.na)),
        screen
      )
    
    if ((ncol(Database$Typing)-13) != length(allele_vector)) {
      
      cust_var <- dplyr::select(Database$Typing, 14:(ncol(Database$Typing) - length(allele_vector)))
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
    
    if ((ncol(Database$Typing)-13) != length(allele_vector)) {
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
            "Errors",
            "Screened"
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
            "Errors",
            "Screened"
          ),
          gsub(".fasta", "", basename(list.files(allele_folder)))
        )
    }
    
    merged <- dplyr::mutate(merged, "Include" = as.logical(Include))
    
    Database$Typing <- rbind(Database$Typing, merged)
    
  }
  
  # Save new Entry in Typing Database
  saveRDS(Database, paste0(db_path, "/", gsub(" ", "_", meta_info$cgmlst_typing), "/Typing.rds"))
  
  # Logging successes
  log.message(log_file = paste0(getwd(), "/logs/single_typing_log.txt"), 
              message = paste0("Successful typing of ", meta_info$assembly_name))
  log_print(paste0("Successful typing of ", meta_info$assembly_name))
  
  # Save assembly file if TRUE
  if(save_assembly) {
    
    isolate_dir <- file.path(db_path, gsub(" ", "_", meta_info$cgmlst_typing), "Isolates")
    
    if(dir.exists(isolate_dir)) {
      
      # Create folder for new isolate
      dir.create(file.path(isolate_dir, meta_info$assembly_id))
      
      # Copy assembly file in isolate directory
      file.copy(assembly, file.path(isolate_dir, meta_info$assembly_id))
      
      setwd(file.path(isolate_dir, meta_info$assembly_id))
      
      zip(zipfile = paste0(meta_info$assembly_id, ".zip"),
          files = basename(assembly),
          zip = "zip") 
      
      file.remove(basename(assembly))
      
      log_print(paste0("Saved assembly of ", meta_info$assembly_id))
      
    } else {
      
      log_print("No isolate folder present yet. Isolate directory created.")
      
      # Create isolate filder for species
      dir.create(isolate_dir)
      
      # Create folder for new isolate
      dir.create(file.path(isolate_dir, meta_info$assembly_id))
      
      # Copy assembly file in isolate directory
      file.copy(assembly, file.path(isolate_dir, meta_info$assembly_id))
      
      setwd(file.path(isolate_dir, meta_info$assembly_id))
      
      zip(zipfile = paste0(meta_info$assembly_id, ".zip"),
          files = basename(assembly),
          zip = "zip") 
      
      file.remove(basename(assembly))
       
      log_print(paste0("Saved assembly of ", meta_info$assembly_id))
    }
  }
  
} else {
  
  failures <- sum(unname(base::sapply(psl_files, file.size)) <= 100) / length(psl_files) * 100
  
  # Logging failures
  log.message(log_file = paste0(getwd(), "/logs/single_typing_log.txt"), 
              message = paste0("Assembly typing of ", meta_info$assembly_name, " failed. ", failures, "% of loci not typed."))
  log_print(paste0("Assembly typing of ", meta_info$assembly_name, " failed. ", failures, "% of loci not typed."))
}

log_close()
