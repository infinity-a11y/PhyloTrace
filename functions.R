# Function to read and format FASTA sequences
format_fasta <- function(filepath) {
  fasta <- readLines(filepath)
  formatted_fasta <- list()
  current_sequence <- ""
  
  for (line in fasta) {
    if (startsWith(line, ">")) {
      if (current_sequence != "") {
        formatted_fasta <- append(formatted_fasta, list(current_sequence))
        current_sequence <- ""
      }
      formatted_fasta <- append(formatted_fasta, list(line))
    } else {
      current_sequence <- paste0(current_sequence, line)
    }
  }
  if (current_sequence != "") {
    formatted_fasta <- append(formatted_fasta, list(current_sequence))
  }
  
  formatted_fasta
}

# Function to color-code the bases in a sequence
color_sequence <- function(sequence) {
  sequence <- gsub("A", "<span class='base-a'>A</span>", sequence)
  sequence <- gsub("T", "<span class='base-t'>T</span>", sequence)
  sequence <- gsub("G", "<span class='base-g'>G</span>", sequence)
  sequence <- gsub("C", "<span class='base-c'>C</span>", sequence)
  sequence
}

# Function to log messages to logfile
log_message <- function(log_file, message, append = TRUE) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n", file = log_file, append = append)
}

# Modified gheatmap function
gheatmap.mod <- function(p, data, offset=0, width=1, low="green", high="red", color="white",
                         colnames=TRUE, colnames_position="bottom", colnames_angle=0, colnames_level=NULL,
                         colnames_offset_x = 0, colnames_offset_y = 0, font.size=4, family="", hjust=0.5, legend_title = "value",
                         colnames_color = "black") {
  
  colnames_position %<>% match.arg(c("bottom", "top"))
  variable <- value <- lab <- y <- NULL
  
  ## if (is.null(width)) {
  ##     width <- (p$data$x %>% range %>% diff)/30
  ## }
  
  ## convert width to width of each cell
  width <- width * (p$data$x %>% range(na.rm=TRUE) %>% diff) / ncol(data)
  
  isTip <- x <- y <- variable <- value <- from <- to <- NULL
  
  ## handle the display of heatmap on collapsed nodes
  ## https://github.com/GuangchuangYu/ggtree/issues/242
  ## extract data on leaves (& on collapsed internal nodes) 
  ## (the latter is extracted only when the input data has data on collapsed
  ## internal nodes)
  df <- p$data
  nodeCo <- intersect(df %>% filter(is.na(x)) %>% 
                        select(.data$parent, .data$node) %>% unlist(), 
                      df %>% filter(!is.na(x)) %>% 
                        select(.data$parent, .data$node) %>% unlist())
  labCo <- df %>% filter(.data$node %in% nodeCo) %>% 
    select(.data$label) %>% unlist()
  selCo <- intersect(labCo, rownames(data))
  isSel <- df$label %in% selCo
  
  df <- df[df$isTip | isSel, ]
  start <- max(df$x, na.rm=TRUE) + offset
  
  dd <- as.data.frame(data)
  ## dd$lab <- rownames(dd)
  i <- order(df$y)
  
  ## handle collapsed tree
  ## https://github.com/GuangchuangYu/ggtree/issues/137
  i <- i[!is.na(df$y[i])]
  
  lab <- df$label[i]
  ## dd <- dd[lab, , drop=FALSE]
  ## https://github.com/GuangchuangYu/ggtree/issues/182
  dd <- dd[match(lab, rownames(dd)), , drop = FALSE]
  
  
  dd$y <- sort(df$y)
  dd$lab <- lab
  ## dd <- melt(dd, id=c("lab", "y"))
  dd <- gather(dd, variable, value, -c(lab, y))
  
  i <- which(dd$value == "")
  if (length(i) > 0) {
    dd$value[i] <- NA
  }
  if (is.null(colnames_level)) {
    dd$variable <- factor(dd$variable, levels=colnames(data))
  } else {
    dd$variable <- factor(dd$variable, levels=colnames_level)
  }
  V2 <- start + as.numeric(dd$variable) * width
  mapping <- data.frame(from=dd$variable, to=V2)
  mapping <- unique(mapping)
  
  dd$x <- V2
  dd$width <- width
  dd[[".panel"]] <- factor("Tree")
  if (is.null(color)) {
    p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, inherit.aes=FALSE)
  } else {
    p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, color=color, inherit.aes=FALSE)
  }
  if (is(dd$value,"numeric")) {
    p2 <- p2 + scale_fill_gradient(low=low, high=high, na.value=NA, name = legend_title) # "white")
  } else {
    p2 <- p2 + scale_fill_discrete(na.value=NA, name = legend_title) #"white")
  }
  
  if (colnames) {
    if (colnames_position == "bottom") {
      y <- 0
    } else {
      y <- max(p$data$y) + 1
    }
    mapping$y <- y
    mapping[[".panel"]] <- factor("Tree")
    p2 <- p2 + geom_text(data=mapping, aes(x=to, y = y, label=from), color = colnames_color, size=font.size, family=family, inherit.aes = FALSE,
                         angle=colnames_angle, nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
  }
  
  p2 <- p2 + theme(legend.position="right")
  ## p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))
  
  if (!colnames) {
    ## https://github.com/GuangchuangYu/ggtree/issues/204
    p2 <- p2 + scale_y_continuous(expand = c(0,0))
  }
  
  attr(p2, "mapping") <- mapping
  return(p2)
}

# Function to find columns with varying values
var_alleles <- function(dataframe) {
  
  varying_columns <- c()
  
  for (col in 1:ncol(dataframe)) {
    unique_values <- unique(dataframe[, col])
    
    if (length(unique_values) > 1) {
      varying_columns <- c(varying_columns, col)
    }
  }
  
  return(varying_columns)
}

# Functions to compute hamming distances dependent on missing value handling
hamming.dist <- function(x, y) {
  sum(x != y)
}

hamming.distIgnore <- function(x, y) {
  sum( (x != y) & !is.na(x) & !is.na(y) )
}

hamming.distCategory <- function(x, y) {
  sum((x != y | xor(is.na(x), is.na(y))) & !(is.na(x) & is.na(y)))
}

compute.distMatrix <- function(profile, hamming.method) {
  mat <- as.matrix(profile)
  n <- nrow(mat)
  dist_mat <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dist_mat[i, j] <- hamming.method(x = mat[i, ], y = mat[j, ])
      dist_mat[j, i] <- dist_mat[i, j]
    }
  }
  return(dist_mat)
}

#Function to check custom variable classes
column_classes <- function(df) {
  sapply(df, function(x) {
    if (class(x) == "numeric") {
      return("cont")
    } else if (class(x) == "character") {
      return("categ")
    } else {
      return(class(x))
    }
  })
}

# Function to hash database
hash_database <- function(folder, progress) {
  loci_files <- list.files(folder)
  loci_names <- sapply(strsplit(loci_files, "[.]"), function(x) x[1])
  loci_paths <- file.path(folder, loci_files)
  count <- length(loci_files)
  hashes <- sapply(loci_paths, hash_locus, progress = progress, count = count)
  names(hashes) <- loci_names
  hashes
}

# Function to hash a locus
hash_locus <- function(locus_path, progress, count) {
  locus_file <- readLines(locus_path)
  seq_list <- locus_file[seq(2, length(locus_file), 3)]
  seq_hash <- sha256(seq_list)
  seq_idx <- paste0(">", seq_hash)
  
  locus_file[seq(1, length(locus_file), 3)] <- seq_idx
  writeLines(locus_file, locus_path)
  if(!is.null(progress) & !is.null(count)) {
    progress$inc(1 / (count * 2), 
                 detail = paste("Hashed", basename(locus_path)),
                 message = "Hashing alleles")
  }
  
  seq_hash
}

# Get locus hashes
get_locus_hashes <- function(locus_path) {
  locus_file <- readLines(locus_path)
  hash_list <- locus_file[seq(1, length(locus_file), 3)]
  hash_list <- sapply(strsplit(hash_list, "[>]"), function(x) x[2])
}

extract_seq <- function(locus_path, hashes) {
  locus_file <- readLines(locus_path)
  hash_list <- sapply(strsplit(locus_file[seq(1, length(locus_file), 3)], "[>]"), function(x) x[2])
  seq_list <- locus_file[seq(2, length(locus_file), 3)]
  seq_idx <- hash_list %in% hashes
  
  list(
    idx = hash_list[seq_idx],
    seq = seq_list[seq_idx]
  )
}

add_new_sequences <- function(locus_path, sequences, progress) {
  locus_file <- file(locus_path, open = "a+")
  for (i in seq_along(sequences$idx)) {
    writeLines(c("", paste0(">", sequences$idx[i]), sequences$seq[i]), locus_file)
  }
  close(locus_file)
}

# Compute clusters to use in visNetwork
compute_clusters <- function(nodes, edges, threshold) {
  groups <- rep(0, length(nodes$id))
  edges_groups <- rep(0, length(edges$from))
  
  edges_table <- data.frame(
    from = edges$from,
    to = edges$to,
    weight = edges$weight
  )
  
  count <- 0
  while (any(groups == 0)) {
    group_na <- groups == 0
    labels <- nodes$id[group_na]
    
    cluster <- nodes$id[group_na][1] # Initialize with 1 label
    while (!is_empty(labels)) {
      sub_tb <- edges_table[(edges_table$from %in% cluster | edges_table$to %in% cluster) & edges_table$weight <= threshold,]
      
      if (nrow(sub_tb) == 0 | length(unique(c(sub_tb$from, sub_tb$to))) == length(cluster)) {
        count <- count + 1
        groups[nodes$id %in% cluster] <- paste("Group", count)
        edges_groups[edges$from %in% cluster & edges$to %in% cluster] <- paste("Group", count)
        break
      } else {
        cluster <- unique(c(sub_tb$from, sub_tb$to))
      }
    }
  }
  list(groups = groups,
       edges = edges_groups)
}

# Check gene screening status
check_status <- function(isolate, database, scheme) {
  iso_name <- gsub(".zip", "", basename(isolate))
  if(file.exists(file.path(database, gsub(" ", "_", scheme),
                           "Isolates", iso_name, "status.txt"))) {
    if(str_detect(readLines(file.path(database, gsub(" ", "_", scheme),
                                      "Isolates", iso_name, "status.txt"))[1], 
                  "successfully")) {
      return("success")
    } else {
      return("fail")
    }
  } else {return("unfinished")}
}

# Reset gene screening status
remove.screening.status <- function(isolate, database, scheme) {
  if(file.exists(file.path(database, 
                           gsub(" ", "_", scheme),
                           "Isolates",
                           isolate,
                           "status.txt"))) {
    file.remove(
      file.path(database, 
                gsub(" ", "_", scheme),
                "Isolates",
                isolate,
                "status.txt")
    )
  }
}

# Truncate hashes
truncHash <- function(hash) {
  if(!is.na(hash)) {
    paste0(str_sub(hash, 1, 4), "...", str_sub(hash, nchar(hash) - 3, nchar(hash))) 
  } else {NA}
}

# Function to check for HTTP errors and parse content
safe.api.call <- function(endpoint) {
  response <- tryCatch(
    httr::GET(endpoint),
    error = function(e) {
      message("Error in API call: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(response)) {
    return(NULL)
  }
  
  if (httr::http_error(response)) {
    message("HTTP error: ", httr::status_code(response))
    return(NULL)
  }
  
  content <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) {
      message("Error in parsing response content: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(content)) {
    return(NULL)
  }
  
  parsed_content <- tryCatch(
    jsonlite::fromJSON(content, flatten = TRUE),
    error = function(e) {
      message("Error in parsing JSON content: ", e$message)
      return(NULL)
    }
  )
  
  return(parsed_content)
}

# Function to fetch species data from NCBI
fetch.species.data <- function(species) {
  
  if(species == "Borrelia spp") {
    species <- "Borrelia"
  } else if(species == "Brucella spp") {
    species <- "Brucella"
  } else if(species == "Burkholderia mallei FLI" | species == "Burkholderia mallei RKI") {
    species <- "Burkholderia mallei"
  } else if(species == "Campylobacter jejuni coli v1" | 
            species == "Campylobacter jejuni coli v2" |
            species == "Campylobacter jejuni coli") {
    species <- c("Campylobacter jejuni", "Campylobacter coli")
  } else if(species == "Cronobacter sakazakii malonaticus") {
    species <- c("Cronobacter sakazakii", "Cronobacter malonaticus")
  } else if(species == "Human-restricted Neisseria v1" | 
            species == "Neisseria L3" |
            species == "Neisseria L44") {
    species <- "Neisseria"
  } else if(species == "Neisseria gonorrhoeae v1" | 
            species == "Neisseria gonorrhoeae v2") {
    species <- "Neisseria gonorrhoeae"
  } else if(species == "Neisseria meningitidis v1" | 
            species == "Neisseria meningitidis v2" |
            species == "Neisseria meningitidis v3") {
    species <- "Neisseria meningitidis"
  } else if(species == "Salmonella v1" | 
             species == "Salmonella v2 enterobase") {
    species <- "Salmonella"
  } else if(species == "Klebsiella oxytoca sensu lato") {
    species <- paste("Klebsiella", c("oxytoca", "grimontii", "michiganensis", "pasteurii"))
  } else if(species == "Klebsiella pneumoniae sensu lato") {
    species <- paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae"))
  } else if(species == "Mycobacterium tuberculosis complex") {
    species <- paste("Mycobacterium", c("tuberculosis", "tuberculosis variant bovis", "", "canetti"))
  }  
  
  if(length(species > 1)) {
    multiple <- list()
    for(i in seq_along(species)) {
      command <- paste0("datasets summary taxonomy taxon '", species[i], "'")
      
      tryCatch(
        result <- system(command, intern = TRUE),
        error = function(e) {
          message("Error in NCBI Datasets call: ", e$message)
          return(NULL)
        }
      )
      
      if (length(result) < 1) {
        message(paste("Error: ", species, " not available on NCBI."))
        return(NULL)
      } else {
        tryCatch(
          content <- jsonlite::fromJSON(paste(result, collapse = "")),
          error = function(e) {
            message("Error in NCBI Datasets call: ", e$message)
            return(NULL)
          }
        )
        
        if (!is.null(content$reports)) {
          species_data <- content$reports$taxonomy
          
          message("Fetched taxonomy")
          
          multiple[[gsub(" ", "_", species[i])]] <- list(
            Name = species_data$current_scientific_name,
            ID = species_data$tax_id,
            Classification = species_data$classification,
            Group = species_data$group_name,
            Image = paste0("https://api.ncbi.nlm.nih.gov/datasets/v2alpha/taxonomy/taxon/", species_data$tax_id, "/image")
          )
        } else {
          message("Empty taxonomy data")
          return(NULL)
        }
      }
    }
    
    if (!is.null(multiple)) {
      return(multiple)
    } else {
      message("Empty taxonomy data")
      return(NULL)
    }
  } else {
    command <- paste0("datasets summary taxonomy taxon '", species, "'")
    
    tryCatch(
      result <- system(command, intern = TRUE),
      error = function(e) {
        message("Error in NCBI Datasets call: ", e$message)
        return(NULL)
      }
    )
    
    if (!is.null(content$reports)) {
      species_data <- content$reports$taxonomy
      
      message("Fetched taxonomy")
      
      return(list(
        Name = species_data$current_scientific_name,
        ID = species_data$tax_id,
        Classification = species_data$classification,
        Group = species_data$group_name,
        Image = paste0("https://api.ncbi.nlm.nih.gov/datasets/v2alpha/taxonomy/taxon/", species_data$tax_id, "/image")
      ))
    } else {
      message("Empty taxonomy data")
      return(NULL)
    }
  }
}

# Function to retrieve cgmlst scheme information
get.schemeinfo <- function(url_link) {
  endpoint <- paste0(url_link, "?return_all=1")
  scheme_info <- safe.api.call(endpoint)
  
  if (is.null(scheme_info)) {
    message("Failed to retrieve scheme information.")
    return(NULL)
  }
  
  return(scheme_info)
}

# Function to download all alleles of each loci of selected scheme
download.alleles2.PM <- function(url_link, database, folder_name, progress) {
  
  # Make scheme directory
  directory <- file.path(database, folder_name)
  if (!dir.exists(directory)) {
    tryCatch(
      {
        dir.create(directory, recursive = TRUE)
        message("Directory created: ", directory)
      },
      error = function(e) {
        stop("Failed to create directory: ", directory, "\nError: ", e$message)
      }
    )
  }
  
  # retrieve and save scheme info
  scheme_info <- get.schemeinfo(url_link)
  if (is.null(scheme_info$loci) || length(scheme_info$loci) == 0) {
    stop("No loci found in scheme_info.")
  } else {
    if(!is.null(scheme_info[["last_updated"]])) {
      last_scheme_change <- scheme_info[["last_updated"]]
      last_file_change <- format(
        file.info(file.path(database, ".downloaded_schemes",
                            paste0(folder_name, ".zip")))$mtime, "%Y-%m-%d %H:%M %p")
    } else {
      last_scheme_change <- "Not Available"
      last_file_change <- NULL
    }
    
    if(!is.null(scheme_info[["description"]])) {
      description <- scheme_info[["description"]]
    } else {
      description <- "Not Available"
    }
    
    scheme_overview <- data.frame(x1 = c("Scheme", "Database", "URL", "Version", "Locus Count", "Last Change"),
                                  x2 = c(gsub("_", " ", folder_name),
                                         "pubMLST",
                                         paste0('<a href="', 
                                                paste0("https://www.pubmlst.org/bigsdb?db=",
                                                       basename(dirname(dirname(url_link)))), 
                                                '" target="_blank">', 
                                                paste0("https://www.pubmlst.org/bigsdb?db=",
                                                       basename(dirname(dirname(url_link)))), 
                                                '</a>'),
                                         description,
                                         scheme_info[["locus_count"]], 
                                         last_scheme_change))
    
    names(scheme_overview) <- NULL
    
    saveRDS(scheme_overview, file.path(directory, "scheme_info.rds"))  
    
    message("Scheme info downloaded")
  }
  
  ### Download alleles 
  
  # Initialize vector to store the paths of downloaded files
  downloaded_files <- vector("character", length(scheme_info$loci))
  
  # Make output folder
  output_folder <- file.path(database, ".downloaded_schemes", 
                             paste0(folder_name))
  
  if(!dir.exists(output_folder)) {
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop("Failed to create directory: ", output_folder, "\nError: ", e$message)
      }
    )
  }
  
  for (i in seq_along(scheme_info$loci)) {
    locus_url <- scheme_info$loci[i]
    endpoint <- paste0(locus_url, "/alleles_fasta?return_all=1")
    
    response <- httr::GET(endpoint)
    
    if (httr::http_error(response)) {
      stop("Failed to retrieve data for locus: ", basename(locus_url))
    }
    
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    if (is.null(content)) {
      stop("Failed to parse content for locus: ", basename(locus_url))
    }
    
    # Insert an empty line between sequences
    formatted_content <- gsub("\n>", "\n\n>", content)
    
    # Write to file
    output_file <- file.path(output_folder,
                             paste0(basename(locus_url), ".fasta"))
    
    writeLines(formatted_content, con = output_file)
    
    downloaded_files[i] <- output_file
    
    # Increment the progress bar
    progress$inc(1/ (2 * length(seq_along(scheme_info$loci))), 
                 detail = paste("Saved", basename(locus_url)))
    
    logr::log_print("Saved fasta file for locus: ", basename(locus_url))
  }
  
  # Zip folder
  system(paste0("zip -r -j ", output_folder, ".zip ", output_folder, "/"))
  unlink(output_folder, recursive = TRUE)
  
  # Final check to ensure all files are non-empty and expected count matches
  empty_files <- downloaded_files[file.info(downloaded_files)$size == 0]
  if (length(empty_files) > 0) {
    stop("Some files are empty: ", paste(basename(empty_files), collapse = ", "))
  }
  
  if (length(downloaded_files) != length(scheme_info$loci)) {
    stop("Mismatch in the number of downloaded files. Expected: ", length(scheme_info$loci), 
         " but got: ", length(downloaded_files))
  }
  
  logr::log_print("All files downloaded successfully and are non-empty.")
}

# Function to download all alleles of each loci of selected scheme
download.alleles.PM <- function(url_link, database, folder_name, progress) {
  
  # Make scheme directory
  directory <- file.path(database, folder_name)
  if (!dir.exists(directory)) {
    tryCatch(
      {
        dir.create(directory, recursive = TRUE)
        message("Directory created: ", directory)
      },
      error = function(e) {
        stop("Failed to create directory: ", directory, "\nError: ", e$message)
      }
    )
  }
  
  # retrieve and save scheme info
  scheme_info <- get.schemeinfo(url_link)
  if (is.null(scheme_info$loci) || length(scheme_info$loci) == 0) {
    stop("No loci found in scheme_info.")
  } else {
    if(!is.null(scheme_info[["last_updated"]])) {
      last_scheme_change <- scheme_info[["last_updated"]]
      last_file_change <- format(
        file.info(file.path(database, ".downloaded_schemes",
                            paste0(folder_name, ".zip")))$mtime, "%Y-%m-%d %H:%M %p")
    } else {
      last_scheme_change <- "Not Available"
      last_file_change <- NULL
    }
    
    if(!is.null(scheme_info[["description"]])) {
      description <- scheme_info[["description"]]
    } else {
      description <- "Not Available"
    }
    
    scheme_overview <- data.frame(x1 = c("Scheme", "Database", "URL", "Version", "Locus Count", "Last Change"),
                                  x2 = c(gsub("_", " ", folder_name),
                                         "pubMLST",
                                         paste0('<a href="', 
                                                paste0("https://www.pubmlst.org/bigsdb?db=",
                                                       basename(dirname(dirname(url_link)))), 
                                                '" target="_blank">', 
                                                paste0("https://www.pubmlst.org/bigsdb?db=",
                                                       basename(dirname(dirname(url_link)))), 
                                                '</a>'),
                                         description,
                                         scheme_info[["locus_count"]], 
                                         last_scheme_change))
    
    names(scheme_overview) <- NULL
    
    saveRDS(scheme_overview, file.path(directory, "scheme_info.rds"))  
    
    message("Scheme info downloaded")
  }
  
  ### Download alleles 
  
  # Make output folder
  output_folder <- file.path(database, ".downloaded_schemes", 
                             paste0(folder_name))
  
  if(!dir.exists(output_folder)) {
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop("Failed to create directory: ", output_folder, "\nError: ", e$message)
      }
    )
  } else {
    unlink(output_folder, recursive = TRUE, force = TRUE)
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop("Failed to create directory: ", output_folder, "\nError: ", e$message)
      }
    )
  }
  
  # Initialize vector to store the paths of downloaded files
  downloaded_files <- vector("character", length(scheme_info$loci))
  
  for (i in seq_along(scheme_info$loci)) {
    locus_url <- scheme_info$loci[i]
    endpoint <- paste0(locus_url, "/alleles_fasta?return_all=1")
    
    response <- httr::GET(endpoint)
    
    if (httr::http_error(response)) {
      stop("Failed to retrieve data for locus: ", basename(locus_url))
    }
    
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    if (is.null(content)) {
      stop("Failed to parse content for locus: ", basename(locus_url))
    }
    
    # Insert an empty line between sequences
    formatted_content <- gsub("\n>", "\n\n>", content)
    
    # Write to file
    output_file <- file.path(output_folder,
                             paste0(basename(locus_url), ".fasta"))
    
    writeLines(formatted_content, con = output_file)
    
    downloaded_files[i] <- output_file
    
    # Increment the progress bar
    progress$inc(1/ (2 * length(seq_along(scheme_info$loci))),
                detail = paste("Saved", basename(locus_url)))

    message("Saved fasta file for locus: ", basename(locus_url))
  }
  
  # Zip folder
  system(paste0("zip -r -j ", output_folder, ".zip ", output_folder, "/"))
  unlink(output_folder, recursive = TRUE)
  
  # Final check to ensure all files are non-empty and expected count matches
  empty_files <- downloaded_files[file.info(downloaded_files)$size == 0]
  if (length(empty_files) > 0) {
    stop("Some files are empty: ", paste(basename(empty_files), collapse = ", "))
  }
  
  if (length(downloaded_files) != length(scheme_info$loci)) {
    stop("Mismatch in the number of downloaded files. Expected: ", length(scheme_info$loci), 
         " but got: ", length(downloaded_files))
  }
  
  logr::log_print("All files downloaded successfully and are non-empty.")
}


multi_download <- function(file_remote, 
                           file_local,
                           total_con = 1000L, 
                           host_con  = 1000L,
                           print = TRUE) {
  
  # check for duplication
  dups <- duplicated(file_remote) | duplicated(file_local)
  file_remote <- file_remote[!dups]
  file_local <- file_local[!dups]
  
  # create pool
  pool <- curl::new_pool(total_con = total_con,
                         host_con = host_con)
  
  # function performed on successful request
  save_download <- function(req) {
    writeBin(req$content, file_local[file_remote == req$url])
  }
  
  # setup async calls
  invisible(
    lapply(
      file_remote, function(f) 
        curl::curl_fetch_multi(f, done = save_download, pool = pool)
    )
  )
  
  # all created requests are performed here
  out <- curl::multi_run(pool = pool)
  
  if (print) print(out)
  
}

download.alleles.CM <- function(url_link, database, folder_name, progress) {
  # Make scheme directory
  directory <- file.path(database, folder_name)
  if (!dir.exists(directory)) {
    tryCatch(
      {
        dir.create(directory, recursive = TRUE)
        message("Directory created: ", directory)
      },
      error = function(e) {
        stop("Failed to create directory: ", directory, "\nError: ", e$message)
      }
    )
  }
  
  # Make output folder
  output_folder <- file.path(database, ".downloaded_schemes", 
                             paste0(folder_name))
  
  if(!dir.exists(output_folder)) {
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop("Failed to create directory: ", output_folder, "\nError: ", e$message)
      }
    )
  } else {
    unlink(output_folder, recursive = TRUE, force = TRUE)
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop("Failed to create directory: ", output_folder, "\nError: ", e$message)
      }
    )
  }
  
  loci_path <- file.path(database, folder_name, "targets.csv")
  
  tryCatch(
    {
      download.file(	
        paste0(url_link, "/locus/?content-type=csv"),	
        dest = loci_path,
        mode = "wb"
      )
      message("Loci info downloaded")
    },
    error = function(e) {
      stop("Failed to download loci info: ", "\nError: ", e$message)
    }
  )
  
  if(file.exists(loci_path)) {
    loci <- data.table::fread(loci_path, select = 2, sep = "\t", header = FALSE)[[1]]
    successes <- 0
    count <- length(loci)
    file_local <- file.path(output_folder, paste0(loci, ".fasta"))
    file_remote <- paste0(url_link, "/locus/", loci ,".fasta/")
    
    while(successes < count) {
      res <- multi_download(na.omit(file_remote[1:50]), na.omit(file_local[1:50]),
                           total_con = 50L, host_con = 50L)
      remain <- !file.exists(file_local)
      file_local <- file_local[remain]
      file_remote <- file_remote[remain]
      successes <- successes + res$success
      # Increment the progress bar
      progress$inc(res$success / (2 * count))
    }
    
    # Zip folder
    system(paste0("zip -r -j ", output_folder, ".zip ", output_folder, "/"))
    unlink(output_folder, recursive = TRUE)
    
    # Final check to ensure all files are non-empty and expected count matches
    empty_files <- file_local[file.info(file_local)$size == 0]
    if (length(empty_files) > 0) {
      stop("Some files are empty: ", paste(basename(empty_files), collapse = ", "))
    }
    
    if (length(empty_files) > 0) {
      stop("Mismatch in the number of downloaded files. Expected: ", length(file_local), 
           " but got: ", length(file_local) - length(empty_files))
    }
    
    logr::log_print("All files downloaded successfully and are non-empty.")
  }
}
