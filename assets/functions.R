# Functions

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
  if(length(locus_file) > 0) {
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
  edge_group <- rep(0, length(edges$from))
  
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
        edge_group[edges$from %in% cluster & edges$to %in% cluster] <- paste("Group", count)
        break
      } else {
        cluster <- unique(c(sub_tb$from, sub_tb$to))
      }
    }
  }
  list(groups = groups,
       edge_group = edge_group)
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

parse.names <- function(species) {
  if(species == "Borrelia spp") {
    return("Borrelia")
  } else if(species == "Brucella spp") {
    return("Brucella")
  } else if(species == "Burkholderia mallei FLI" | species == "Burkholderia mallei RKI") {
    return("Burkholderia mallei")
  } else if(species == "Campylobacter jejuni coli v1" | 
            species == "Campylobacter jejuni coli v2" |
            species == "Campylobacter jejuni coli") {
    return(c("Campylobacter jejuni", "Campylobacter coli"))
  } else if(species == "Cronobacter sakazakii malonaticus") {
    return(c("Cronobacter sakazakii", "Cronobacter malonaticus"))
  } else if(species == "Escherichia spp") {
    return("Escherichia")
  } else if(species == "Human-restricted Neisseria v1" | 
            species == "Neisseria L3" |
            species == "Neisseria L44") {
    return("Neisseria")
  } else if(species == "Neisseria gonorrhoeae v1" | 
            species == "Neisseria gonorrhoeae v2") {
    return("Neisseria gonorrhoeae")
  } else if(species == "Neisseria meningitidis v1" | 
            species == "Neisseria meningitidis v2" |
            species == "Neisseria meningitidis v3") {
    return("Neisseria meningitidis")
  } else if(species == "Salmonella v1" | 
            species == "Salmonella v2 enterobase") {
    return("Salmonella")
  } else if(species == "Klebsiella oxytoca sensu lato") {
    return(paste("Klebsiella", c("oxytoca", "grimontii", "michiganensis", "pasteurii")))
  } else if(species == "Klebsiella pneumoniae sensu lato") {
    return(paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae")))
  } else if(species == "Mycobacterium tuberculosis complex") {
    return(paste("Mycobacterium", c("tuberculosis", "tuberculosis variant bovis", "canetti")))
  } else if(species == "Serratia spp") {
    return("Serratia")
  } else if(species == "Leptospira spp") {
    return("Leptospira")
  } else if(species == "Mycobacteroides abscessus complex") {
    return("Mycobacteroides abscessus")
  } else {
    return(species)
  }
}

check.amrfinder.available <- function(selected_scheme, amrfinder_species) {
  scheme_species <- gsub(" (CM|PM)", "", gsub("_", " ", selected_scheme))
  
  parsed_species <- parse.names(scheme_species)
  
  if(length(parsed_species) == 1) {
    
    # Exceptions
    if(parsed_species == "Burkholderia mallei FLI" | 
       parsed_species == "Burkholderia mallei RKI") {
      parsed_species <- "Burkholderia mallei"
    } else if(parsed_species == "Escherichia coli") {
      parsed_species <- "Escherichia"
    } else if(parsed_species == "Salmonella enterica") {
      parsed_species <- "Salmonella"
    }
    
    return(ifelse(any(parsed_species == gsub("_", " ", amrfinder_species)), parsed_species, FALSE))
    
  } else {
    # Exceptions
    if(identical(parsed_species, 
                 paste("Klebsiella", c("oxytoca", "grimontii", "michiganensis", "pasteurii")))) {
      parsed_species <- "Klebsiella oxytoca"
    } else if(identical(parsed_species, 
                        paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae")))) {
      parsed_species <- "Klebsiella pneumoniae"
    } else if(identical(parsed_species, 
                        paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae")))) {
      parsed_species <- "Klebsiella pneumoniae"
    } else if(identical(parsed_species,
                        paste("Campylobacter", c("jejuni", "coli")))) {
      parsed_species <- "Campylobacter"
    }
    
    return(ifelse(any(parsed_species == gsub("_", " ", amrfinder_species)), parsed_species, FALSE))
  }
}

# Function to fetch species data from NCBI
fetch.species.data <- function(species) {
  
  parsed_species <- parse.names(species)
 
  multiple <- list()
  for(i in seq_along(parsed_species)) {
    
    # Serratia exception
    ifelse(parsed_species[i] == "Serratia",
           command <- paste0("datasets summary taxonomy taxon '", 613, "'"),
           command <- paste0("datasets summary taxonomy taxon '", parsed_species[i], "'"))
    
    tryCatch(
      result <- system(command, intern = TRUE),
      error = function(e) {
        message("Error in NCBI Datasets call: ", e$message)
        return(NULL)
      }
    )
    
    if (length(result) < 1) {
      message(paste("Error: ", parsed_species, " not available on NCBI."))
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
        
        multiple[[gsub(" ", "_", parsed_species[i])]] <- list(
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
  
  progress$set(message = "Compressing files", value = 50)
  
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
    
    progress$set(message = "Compressing Files")
    
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

# Function to check and remove duplicate segments
process_string <- function(input_string) {
  
  # Split the string by comma
  segments <- strsplit(input_string, ",")[[1]]
  
  # Ensure that there are two parts to compare
  if (length(segments) == 2) {
    
    # Remove asterisks from both segments
    clean_segment_1 <- gsub("\\*", "", segments[1])
    clean_segment_2 <- gsub("\\*", "", segments[2])
    
    # Check if the cleaned segments are identical
    if (clean_segment_1 == clean_segment_2) {
      
      # If they are identical, return the cleaned version without asterisk
      return(clean_segment_1)
    } else {
      
      # If they are not identical, return the original string
      return(input_string)
    }
  } else {
    
    # If there's no comma or improper format, return the original string
    return(input_string)
  }
}

summarize.AMR <- function(database, scheme) {
  isolates_path <- file.path(database, gsub(" ", "_", scheme), "Isolates")
  isolates_full <- list.files(isolates_path, full.names = TRUE)
  available <- sapply(isolates_full, function(isolate) file.exists(file.path(isolate, "amrfinder.out")))
  isolates <- basename(isolates_full[available])
  
  loci <- character()
  amr_profile <- list()
  amr_matches <- data.frame()
  virulence_matches <- data.frame()
  
  amr_df <- list()
  vir_df <- list()
  
  for(i in seq_along(isolates)) {
    
    amr_profile_path <- file.path(isolates_path, isolates[i])
    
    # amrfinder results
    if(file.exists(file.path(amr_profile_path, "amrfinder.out"))) {
      amr_results <- read.delim(file.path(amr_profile_path, "amrfinder.out"), stringsAsFactors = FALSE)
      gene_symbols <- unique(amr_results$Gene.symbol)  
      amr_profile[[i]] <- gene_symbols  
      loci <- union(loci, gene_symbols) 
    }
    
    # classification antibiotics drug 
    if(file.exists(file.path(amr_profile_path, "summary_matches.txt"))) {
      summary_matches <- read.delim(file.path(amr_profile_path, "summary_matches.txt"), 
                                    stringsAsFactors = FALSE) %>%
        select(-1)
      
      # Process each column
      for (col in colnames(summary_matches)) {
        if (nrow(summary_matches) > 0) {
          # Ensure the column exists in previous data frames
          if (length(amr_df) > 0 && col %in% colnames(amr_df[[1]])) {
            
            # Extract previous data frame
            prev_df <- amr_df[[length(amr_df)]]
            
            if (col %in% colnames(prev_df)) {  
              # Combine the current and previous columns for the specific column
              combined_col <- unique(c(stringr::str_split(prev_df[[col]], ",")[[1]], 
                                       stringr::str_split(summary_matches[[col]], ",")[[1]]))
              
              # Update the column in the previous data frame with the combined values
              prev_df[[col]] <- paste(combined_col, collapse = ",")
            }
            
            # Update the last data frame in the list
            amr_df[[length(amr_df)]] <- prev_df
            
          } else {
            # If the column is new, add it to the previous data frame if it exists
            if (length(amr_df) > 0) {
              amr_df[[length(amr_df)]][[col]] <- summary_matches[[col]]
            } else {
              # If there are no previous data frames, initialize the first one
              amr_df[[1]] <- summary_matches
            }
          }
        }
      }
      
      # Add the current data frame to the list
      amr_df[[length(amr_df) + 1]] <- summary_matches
    }
    
    # classification virulence 
    if(file.exists(file.path(amr_profile_path, "summary_virulence.txt"))) {
      summary_virulence <- read.delim(file.path(amr_profile_path, "summary_virulence.txt"), 
                                      stringsAsFactors = FALSE) %>%
        select(-1)
      
      # Process each column
      for (col in colnames(summary_virulence)) {
        if (nrow(summary_virulence) > 0) {
          # Ensure the column exists in previous data frames
          if (length(vir_df) > 0 && col %in% colnames(vir_df[[1]])) {
            
            # Extract previous data frame
            prev_df <- vir_df[[length(vir_df)]]
            
            if (col %in% colnames(prev_df)) {
              
              # Combine the current and previous columns for the specific column
              combined_col <- unique(c(stringr::str_split(prev_df[[col]], ",")[[1]], 
                                       stringr::str_split(summary_virulence[[col]], ",")[[1]]))
              
              # Update the column in the previous data frame with the combined values
              prev_df[[col]] <- paste(combined_col, collapse = ",")
            }
            
            # Update the last data frame in the list
            vir_df[[length(vir_df)]] <- prev_df
            
          } else {
            # If the column is new, add it to the previous data frame if it exists
            if (length(vir_df) > 0) {
              vir_df[[length(vir_df)]][[col]] <- summary_virulence[[col]]
            } else {
              # If there are no previous data frames, initialize the first one
              vir_df[[1]] <- summary_virulence
            }
          }
        }
      }
      
      # Add the current data frame to the list
      vir_df[[length(vir_df) + 1]] <- summary_virulence
    }
  }
  
  # collate amr data frames
  final_df_amr <- dplyr::bind_rows(amr_df, .id = "source")
  
  amr_class_conc <- final_df_amr %>%
    summarise(across(everything(), ~ {
      unique_values <- unique(unlist(stringr::str_split(na.omit(.), ",")))
      paste(unique_values, collapse = ",")
    })) %>%
    select(-1)
  
  amr_class_long <- amr_class_conc %>%
    tidyr::pivot_longer(cols = everything(), 
                 names_to = "Variable", 
                 values_to = "Observation")
  
  amr_class_pre <- amr_class_long %>%
    tidyr::separate_rows(Observation, sep = ",")
  
  amr_class <- as.data.frame(lapply(amr_class_pre, function(column) {
    sapply(column, process_string)  
  }))
  
  # collate virulence data frames
  final_df_vir <- dplyr::bind_rows(vir_df, .id = "source")
  
  vir_class_conc <- final_df_vir %>%
    summarise(across(everything(), ~ {
      unique_values <- unique(unlist(stringr::str_split(na.omit(.), ",")))
      paste(unique_values, collapse = ",")
    })) %>%
    select(-1)
  
  vir_class_long <- vir_class_conc %>%
    tidyr::pivot_longer(cols = everything(), 
                 names_to = "Variable", 
                 values_to = "Observation")
  
  vir_class <- vir_class_long %>%
    tidyr::separate_rows(Observation, sep = ",")
  
  amr_class <- as.data.frame(lapply(amr_class_pre, function(column) {
    sapply(column, process_string)}))
    
  # collate results
  df <- matrix(FALSE, nrow = length(isolates), ncol = length(loci))
  colnames(df) <- loci
  rownames(df) <- isolates
  
  for(i in seq_along(amr_profile)) {
    df[i, loci %in% amr_profile[[i]]] <- TRUE
  }
  
  return(list(results = as.data.frame(df),
              AMR_classification = amr_class,
              virulence_classification = vir_class))
}

# function to categorize genes by classes
categorize.gs <- function(names, df) {
  
  value_amr <- df$amr[which(names == rownames(test))]
  value_vir <- df$vir[which(names == rownames(test))]
  
  if (!is.na(value_amr) && value_amr != "") {
    return("AMR")
  } else if (!is.na(value_vir) && value_vir != "") {
    return("Vir")
  } else {
    return("None")
  }
}

# Create a function to get color and symbol based on the category
get_annotation_params <- function(category_index, num_categories, color_palette) {
  # Define color palettes and symbols
  symbol_set <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25)  # First set of symbols
  
  # Determine the set based on the category index
  set_index <- (category_index - 1) %/% 10 + 1
  
  # Calculate the index within the set
  symbol_index <- (category_index - 1) %% 10 + 1
  color_index <- (category_index - 1) %% length(color_palette) + 1
  
  # Return the symbol and color
  list(symbol = symbol_set[symbol_index], color = color_palette[color_index])
}

# function to get gene screening classification meta
get.gsMeta <- function(gene_class, hm_matrix) {
  class_filtered <- gene_class[!duplicated(gsub("\\*", "", gene_class$Observation)),]
  clean_gene_name <- gsub("\\*", "", class_filtered$Observation)
  unison <- colnames(hm_matrix) %in% clean_gene_name

  class_present <- colnames(hm_matrix)[unison]
  no_class <- colnames(hm_matrix)[!unison]

  classes <- character()
  for(i in 1:length(class_present)) {
    classes[i] <- class_filtered$Variable[which(class_present[i] == clean_gene_name)]
  }

  meta <- data.frame(
    gene = c(class_present, no_class),
    class = c(classes, rep(NA, length(no_class)))
  )

  if(nrow(meta) != 0) {
    meta <- meta %>%
      arrange(gene) %>%
      tibble::column_to_rownames(var = "gene")
  }

  meta
}

color_scale_bg_JS <- "var selectedOption = $('#col_scale_id').val();
    
    if (selectedOption === 'Set1') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #E41A1C 0%, #E41A1C 11%, #377EB8 11%, #377EB8 22%, #4DAF4A 22%, #4DAF4A 33%, #984EA3 33%, #984EA3 44%, #FF7F00 44%, #FF7F00 55%, #FFFF33 55%, #FFFF33 66%, #A65628 66%, #A65628 77%, #F781BF 77%, #F781BF 88%, #999999 88%, #999999 100%)',
        'color': 'black'
      });
    } else if (selectedOption === 'Set2') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #66C2A5 0%, #66C2A5 12.5%, #FC8D62 12.5%, #FC8D62 25%, #8DA0CB 25%, #8DA0CB 37.5%, #E78AC3 37.5%, #E78AC3 50%, #A6D854 50%, #A6D854 62.5%, #FFD92F 62.5%, #FFD92F 75%, #E5C494 75%, #E5C494 87.5%, #B3B3B3 87.5%, #B3B3B3 100%)',
        'color': 'black'
      });
    } else if (selectedOption === 'Set3') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #8DD3C7 0%, #8DD3C7 8.33333%, #FFFFB3 8.33333%, #FFFFB3 16.6667%, #BEBADA 16.6667%, #BEBADA 25%, #FB8072 25%, #FB8072 33.3333%, #80B1D3 33.3333%, #80B1D3 41.6667%, #FDB462 41.6667%, #FDB462 50%, #B3DE69 50%, #B3DE69 58.3333%, #FCCDE5 58.3333%, #FCCDE5 66.6667%, #D9D9D9 66.6667%, #D9D9D9 75%, #BC80BD 75%, #BC80BD 83.3333%, #CCEBC5 83.3333%, #CCEBC5 91.6667%, #FFED6F 91.6667%, #FFED6F 100%)',
        'color': 'black'
      });
    } else if (selectedOption === 'Pastel1') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FBB4AE 0%, #FBB4AE 11.1111%, #B3CDE3 11.1111%, #B3CDE3 22.2222%, #CCEBC5 22.2222%, #CCEBC5 33.3333%, #DECBE4 33.3333%, #DECBE4 44.4444%, #FED9A6 44.4444%, #FED9A6 55.5556%, #FFFFCC 55.5556%, #FFFFCC 66.6667%, #E5D8BD 66.6667%, #E5D8BD 77.7778%, #FDDAEC 77.7778%, #FDDAEC 88.8889%, #F2F2F2 88.8889%, #F2F2F2 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Pastel2') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #B3E2CD 0%, #B3E2CD 12.5%, #FDCDAC 12.5%, #FDCDAC 25%, #CBD5E8 25%, #CBD5E8 37.5%, #F4CAE4 37.5%, #F4CAE4 50%, #E6F5C9 50%, #E6F5C9 62.5%, #FFF2AE 62.5%, #FFF2AE 75%, #F1E2CC 75%, #F1E2CC 87.5%, #CCCCCC 87.5%, #CCCCCC 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Paired') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #A6CEE3 0%, #A6CEE3 8.33333%, #1F78B4 8.33333%, #1F78B4 16.6667%, #B2DF8A 16.6667%, #B2DF8A 25%, #33A02C 25%, #33A02C 33.3333%, #FB9A99 33.3333%, #FB9A99 41.6667%, #E31A1C 41.6667%, #E31A1C 50%, #FDBF6F 50%, #FDBF6F 58.3333%, #FF7F00 58.3333%, #FF7F00 66.6667%, #CAB2D6 66.6667%, #CAB2D6 75%, #6A3D9A 75%, #6A3D9A 83.3333%, #FFFF99 83.3333%, #FFFF99 91.6667%, #B15928 91.6667%, #B15928 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Dark2') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #1B9E77 0%, #1B9E77 12.5%, #D95F02 12.5%, #D95F02 25%, #7570B3 25%, #7570B3 37.5%, #E7298A 37.5%, #E7298A 50%, #66A61E 50%, #66A61E 62.5%, #E6AB02 62.5%, #E6AB02 75%, #A6761D 75%, #A6761D 87.5%, #666666 87.5%, #666666 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Accent') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #7FC97F 0%, #7FC97F 12.5%, #BEAED4 12.5%, #BEAED4 25%, #FDC086 25%, #FDC086 37.5%, #FFFF99 37.5%, #FFFF99 50%, #386CB0 50%, #386CB0 62.5%, #F0027F 62.5%, #F0027F 75%, #BF5B17 75%, #BF5B17 87.5%, #666666 87.5%, #666666 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'YlOrRd') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFCC 0%, #FFFFCC 11.1111%, #FFEDA0 11.1111%, #FFEDA0 22.2222%, #FED976 22.2222%, #FED976 33.3333%, #FEB24C 33.3333%, #FEB24C 44.4444%, #FD8D3C 44.4444%, #FD8D3C 55.5556%, #FC4E2A 55.5556%, #FC4E2A 66.6667%, #E31A1C 66.6667%, #E31A1C 77.7778%, #BD0026 77.7778%, #BD0026 88.8889%, #800026 88.8889%, #800026 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'YlOrBr') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFE5 0%, #FFFFE5 11.1111%, #FFF7BC 11.1111%, #FFF7BC 22.2222%, #FEE391 22.2222%, #FEE391 33.3333%, #FEC44F 33.3333%, #FEC44F 44.4444%, #FE9929 44.4444%, #FE9929 55.5556%, #EC7014 55.5556%, #EC7014 66.6667%, #CC4C02 66.6667%, #CC4C02 77.7778%, #993404 77.7778%, #993404 88.8889%, #662506 88.8889%, #662506 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'YlGnBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFD9 0%, #FFFFD9 11.1111%, #EDF8B1 11.1111%, #EDF8B1 22.2222%, #C7E9B4 22.2222%, #C7E9B4 33.3333%, #7FCDBB 33.3333%, #7FCDBB 44.4444%, #41B6C4 44.4444%, #41B6C4 55.5556%, #1D91C0 55.5556%, #1D91C0 66.6667%, #225EA8 66.6667%, #225EA8 77.7778%, #253494 77.7778%, #253494 88.8889%, #081D58 88.8889%, #081D58 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'YlGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFE5 0%, #FFFFE5 11.1111%, #F7FCB9 11.1111%, #F7FCB9 22.2222%, #D9F0A3 22.2222%, #D9F0A3 33.3333%, #ADDD8E 33.3333%, #ADDD8E 44.4444%, #78C679 44.4444%, #78C679 55.5556%, #41AB5D 55.5556%, #41AB5D 66.6667%, #238443 66.6667%, #238443 77.7778%, #006837 77.7778%, #006837 88.8889%, #004529 88.8889%, #004529 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Reds') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF5F0 0%, #FFF5F0 11.1111%, #FEE0D2 11.1111%, #FEE0D2 22.2222%, #FCBBA1 22.2222%, #FCBBA1 33.3333%, #FC9272 33.3333%, #FC9272 44.4444%, #FB6A4A 44.4444%, #FB6A4A 55.5556%, #EF3B2C 55.5556%, #EF3B2C 66.6667%, #CB181D 66.6667%, #CB181D 77.7778%, #A50F15 77.7778%, #A50F15 88.8889%, #67000D 88.8889%, #67000D 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'RdPu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7F3 0%, #FFF7F3 11.1111%, #FDE0DD 11.1111%, #FDE0DD 22.2222%, #FCC5C0 22.2222%, #FCC5C0 33.3333%, #FA9FB5 33.3333%, #FA9FB5 44.4444%, #F768A1 44.4444%, #F768A1 55.5556%, #DD3497 55.5556%, #DD3497 66.6667%, #AE017E 66.6667%, #AE017E 77.7778%, #7A0177 77.7778%, #7A0177 88.8889%, #49006A 88.8889%, #49006A 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Purples') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FCFBFD 0%, #FCFBFD 11.1111%, #EFEDF5 11.1111%, #EFEDF5 22.2222%, #DADAEB 22.2222%, #DADAEB 33.3333%, #BCBDDC 33.3333%, #BCBDDC 44.4444%, #9E9AC8 44.4444%, #9E9AC8 55.5556%, #807DBA 55.5556%, #807DBA 66.6667%, #6A51A3 66.6667%, #6A51A3 77.7778%, #54278F 77.7778%, #54278F 88.8889%, #3F007D 88.8889%, #3F007D 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'PuRd') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7F4F9 0%, #F7F4F9 11.1111%, #E7E1EF 11.1111%, #E7E1EF 22.2222%, #D4B9DA 22.2222%, #D4B9DA 33.3333%, #C994C7 33.3333%, #C994C7 44.4444%, #DF65B0 44.4444%, #DF65B0 55.5556%, #E7298A 55.5556%, #E7298A 66.6667%, #CE1256 66.6667%, #CE1256 77.7778%, #980043 77.7778%, #980043 88.8889%, #67001F 88.8889%, #67001F 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'PuBuGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7FB 0%, #FFF7FB 11.1111%, #ECE2F0 11.1111%, #ECE2F0 22.2222%, #D0D1E6 22.2222%, #D0D1E6 33.3333%, #A6BDDB 33.3333%, #A6BDDB 44.4444%, #67A9CF 44.4444%, #67A9CF 55.5556%, #3690C0 55.5556%, #3690C0 66.6667%, #02818A 66.6667%, #02818A 77.7778%, #016C59 77.7778%, #016C59 88.8889%, #014636 88.8889%, #014636 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'PuBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7FB 0%, #FFF7FB 11.1111%, #ECE7F2 11.1111%, #ECE7F2 22.2222%, #D0D1E6 22.2222%, #D0D1E6 33.3333%, #A6BDDB 33.3333%, #A6BDDB 44.4444%, #74A9CF 44.4444%, #74A9CF 55.5556%, #3690C0 55.5556%, #3690C0 66.6667%, #0570B0 66.6667%, #0570B0 77.7778%, #045A8D 77.7778%, #045A8D 88.8889%, #023858 88.8889%, #023858 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'OrRd') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7EC 0%, #FFF7EC 11.1111%, #FEE8C8 11.1111%, #FEE8C8 22.2222%, #FDD49E 22.2222%, #FDD49E 33.3333%, #FDBB84 33.3333%, #FDBB84 44.4444%, #FC8D59 44.4444%, #FC8D59 55.5556%, #EF6548 55.5556%, #EF6548 66.6667%, #D7301F 66.6667%, #D7301F 77.7778%, #B30000 77.7778%, #B30000 88.8889%, #7F0000 88.8889%, #7F0000 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Oranges') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF5EB 0%, #FFF5EB 11.1111%, #FEE6CE 11.1111%, #FEE6CE 22.2222%, #FDD0A2 22.2222%, #FDD0A2 33.3333%, #FDAE6B 33.3333%, #FDAE6B 44.4444%, #FD8D3C 44.4444%, #FD8D3C 55.5556%, #F16913 55.5556%, #F16913 66.6667%, #D94801 66.6667%, #D94801 77.7778%, #A63603 77.7778%, #A63603 88.8889%, #7F2704 88.8889%, #7F2704 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Greys') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFFF 0%, #FFFFFF 11.1111%, #F0F0F0 11.1111%, #F0F0F0 22.2222%, #D9D9D9 22.2222%, #D9D9D9 33.3333%, #BDBDBD 33.3333%, #BDBDBD 44.4444%, #969696 44.4444%, #969696 55.5556%, #737373 55.5556%, #737373 66.6667%, #525252 66.6667%, #525252 77.7778%, #252525 77.7778%, #252525 88.8889%, #000000 88.8889%, #000000 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Greens') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCF5 0%, #F7FCF5 11.1111%, #E5F5E0 11.1111%, #E5F5E0 22.2222%, #C7E9C0 22.2222%, #C7E9C0 33.3333%, #A1D99B 33.3333%, #A1D99B 44.4444%, #74C476 44.4444%, #74C476 55.5556%, #41AB5D 55.5556%, #41AB5D 66.6667%, #238B45 66.6667%, #238B45 77.7778%, #006D2C 77.7778%, #006D2C 88.8889%, #00441B 88.8889%, #00441B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'GnBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCF0 0%, #F7FCF0 11.1111%, #E0F3DB 11.1111%, #E0F3DB 22.2222%, #CCEBC5 22.2222%, #CCEBC5 33.3333%, #A8DDB5 33.3333%, #A8DDB5 44.4444%, #7BCCC4 44.4444%, #7BCCC4 55.5556%, #4EB3D3 55.5556%, #4EB3D3 66.6667%, #2B8CBE 66.6667%, #2B8CBE 77.7778%, #0868AC 77.7778%, #0868AC 88.8889%, #084081 88.8889%, #084081 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'BuPu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCFD 0%, #F7FCFD 11.1111%, #E0ECF4 11.1111%, #E0ECF4 22.2222%, #BFD3E6 22.2222%, #BFD3E6 33.3333%, #9EBCDA 33.3333%, #9EBCDA 44.4444%, #8C96C6 44.4444%, #8C96C6 55.5556%, #8C6BB1 55.5556%, #8C6BB1 66.6667%, #88419D 66.6667%, #88419D 77.7778%, #810F7C 77.7778%, #810F7C 88.8889%, #4D004B 88.8889%, #4D004B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'BuGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCFD 0%, #F7FCFD 11.1111%, #E5F5F9 11.1111%, #E5F5F9 22.2222%, #CCECE6 22.2222%, #CCECE6 33.3333%,   #99D8C9 33.3333%, #99D8C9 44.4444%, #66C2A4 44.4444%, #66C2A4 55.5556%, #41AE76 55.5556%, #41AE76 66.6667%, #238B45 66.6667%, #238B45 77.7778%, #006D2C 77.7778%, #006D2C 88.8889%, #00441B 88.8889%, #00441B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Blues') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FBFF 0%, #F7FBFF 11.1111%, #DEEBF7 11.1111%, #DEEBF7 22.2222%, #C6DBEF 22.2222%, #C6DBEF 33.3333%,   #9ECAE1 33.3333%, #9ECAE1 44.4444%, #6BAED6 44.4444%, #6BAED6 55.5556%, #4292C6 55.5556%, #4292C6 66.6667%, #2171B5 66.6667%, #2171B5 77.7778%, #08519C 77.7778%, #08519C 88.8889%, #08306B 88.8889%, #08306B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'magma') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #000004FF, #07071DFF, #160F3BFF, #29115AFF, #400F73FF, #56147DFF, #6B1D81FF, #802582FF, #952C80FF, #AB337CFF, #C03A76FF, #D6456CFF, #E85362FF, #F4685CFF, #FA815FFF, #FD9A6AFF, #FEB37BFF, #FECC8FFF, #FDE4A6FF, #FCFDBFFF)',
      'color': 'white'
      })
    } else if (selectedOption === 'inferno') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #000004FF, #08051EFF, #190C3EFF, #300A5BFF, #460B6AFF, #5C126EFF, #711A6EFF, #87216BFF, #9C2964FF, #B1325AFF, #C43C4EFF, #D64B40FF, #E55C30FF, #F17020FF, #F8870EFF, #FCA007FF, #FBB91FFF, #F7D340FF, #F1ED6FFF, #FCFFA4FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'plasma') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #0D0887FF, #2D0594FF, #44039EFF, #5901A5FF, #6F00A8FF, #8305A7FF, #9512A1FF, #A72197FF, #B6308BFF, #C5407EFF, #D14E72FF, #DD5E66FF, #E76E5BFF, #EF7F4FFF, #F79044FF, #FBA238FF, #FEB72DFF, #FDCB26FF, #F7E225FF, #F0F921FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'viridis') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #440154FF, #481568FF, #482677FF, #453781FF, #3F4788FF, #39558CFF, #32648EFF, #2D718EFF, #287D8EFF, #238A8DFF, #1F968BFF, #20A386FF, #29AF7FFF, #3CBC75FF, #56C667FF, #74D055FF, #94D840FF, #B8DE29FF, #DCE318FF, #FDE725FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'cividis') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #00204DFF, #002A64FF, #00336FFF, #1F3C6DFF, #35466BFF, #444F6BFF, #53596CFF, #5F636EFF, #6B6C71FF, #777776FF, #838079FF, #908B79FF, #9D9677FF, #ABA074FF, #B9AC70FF, #C7B76BFF, #D7C463FF, #E5D05AFF, #F5DD4DFF, #FFEA46FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'rocket') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #03051AFF, #150E26FF, #281535FF, #3C1A42FF, #511E4DFF, #661F54FF, #7C1F5AFF, #931C5BFF, #AA185AFF, #C11754FF, #D3214BFF, #E33541FF, #ED4E3EFF, #F26847FF, #F4815AFF, #F5986FFF, #F6AE86FF, #F7C2A2FF, #F8D7BFFF, #FAEBDDFF)',
      'color': 'white'
      })
    } else if (selectedOption === 'mako') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #0B0405FF, #190E19FF, #27182DFF, #312142FF, #3A2C59FF, #3F3770FF, #414388FF, #3C5397FF, #38639DFF, #3573A1FF, #3482A4FF, #3491A8FF, #35A0ABFF, #3AAEADFF, #46BEADFF, #5ACCADFF, #7ED7AFFF, #A4E0BBFF, #C3E9CEFF, #DEF5E5FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'turbo') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #30123BFF, #3F3994FF, #455ED2FF, #4681F7FF, #3AA2FCFF, #23C3E4FF, #18DEC1FF, #2CF09EFF, #5BFB72FF, #8EFF49FF, #B5F836FF, #D6E635FF, #EFCD3AFF, #FCB036FF, #FD8A26FF, #F36215FF, #E14209FF, #C82803FF, #A51301FF, #7A0403FF)',
      'color': 'black'
      })
    } else if (selectedOption === 'Spectral') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #9E0142, #D53E4F, #F46D43, #FDAE61, #FEE08B, #FFFFBF, #E6F598, #ABDDA4, #66C2A5, #3288BD, #5E4FA2)',
      'color': 'black'
      })
    } else if (selectedOption === 'RdYlGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #A50026, #D73027, #F46D43, #FDAE61, #FEE08B, #FFFFBF, #D9EF8B, #A6D96A, #66BD63, #1A9850, #006837)',
      'color': 'black'
      })
    } else if (selectedOption === 'RdGy') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #67001F, #B2182B, #D6604D, #F4A582, #FDDBC7, #FFFFFF, #E0E0E0, #BABABA, #878787, #4D4D4D, #1A1A1A)',
      'color': 'black'
      })
    } else if (selectedOption === 'RdBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #67001F, #B2182B, #D6604D, #F4A582, #FDDBC7, #F7F7F7, #D1E5F0, #92C5DE, #4393C3, #2166AC, #053061)',
      'color': 'black'
      })
    } else if (selectedOption === 'PuOr') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #7F3B08, #B35806, #E08214, #FDB863, #FEE0B6, #F7F7F7, #D8DAEB, #B2ABD2, #8073AC, #542788, #2D004B)',
      'color': 'black'
      })
    } else if (selectedOption === 'PRGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #40004B, #762A83, #9970AB, #C2A5CF, #E7D4E8, #F7F7F7, #D9F0D3, #A6DBA0, #5AAE61, #1B7837, #00441B)',
      'color': 'black'
      })
    } else if (selectedOption === 'PiYG') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #8E0152, #C51B7D, #DE77AE, #F1B6DA, #FDE0EF, #F7F7F7, #E6F5D0, #B8E186, #7FBC41, #4D9221, #276419)',
      'color': 'black'
      })
    } else if (selectedOption === 'BrBG') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #543005, #8C510A, #BF812D, #DFC27D, #F6E8C3, #F5F5F5, #C7EAE5, #80CDC1, #35978F, #01665E, #003C30)',
      'color': 'black'
      })
    }"

# function for matthew correlation coefficient
mcc_dist <- function(x, y) {
  TP <- sum(x == 1 & y == 1)
  TN <- sum(x == 0 & y == 0)
  FP <- sum(x == 0 & y == 1)
  FN <- sum(x == 1 & y == 0)
  
  numerator <- (TP * TN) - (FP * FN)
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  if (denominator == 0) return(0)  
  
  mcc <- numerator / denominator
  return(1 - mcc)  
}

# function for hamming distances
hamming_distance <- function(x, y) {
  return(sum(x != y))
}

# Function to compute Hamming distance matrix for rows/columns
hamming_dist_matrix <- function(m) {
  n <- nrow(m)  # Number of rows
  dist_mat <- matrix(0, n, n)  # Initialize a square matrix for distances
  
  for (i in 1:n) {
    for (j in i:n) {
      dist_mat[i, j] <- hamming_distance(m[i, ], m[j, ])
      dist_mat[j, i] <- dist_mat[i, j]  # Ensure symmetry
    }
  }
  
  return(as.dist(dist_mat))  # Convert to 'dist' object
}

# Function to compute Hamming distance matrix for rows/columns
mcc_dist_matrix <- function(m) {
  n <- nrow(m)  # Number of rows
  dist_mat <- matrix(0, n, n)  # Initialize a square matrix for distances
  
  for (i in 1:n) {
    for (j in i:n) {
      dist_mat[i, j] <- mcc_dist(m[i, ], m[j, ])
      dist_mat[j, i] <- dist_mat[i, j]  # Ensure symmetry
    }
  }
  
  return(as.dist(dist_mat))  # Convert to 'dist' object
}
