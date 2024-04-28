# Load Biostrings package
library(Biostrings)

# Function to check for length and frameshift of new variant sequence
check_length_frameshift <- function(seq, ref_seq) {
  
  # Absolute number of new nucleotides with regard to reference variant/sequence
  len_nt <- abs(nchar(seq) - nchar(ref_seq))
  
  # Check if new variant seq is too long or has frameshift
  
  if((len_nt <= 9) & (len_nt %% 3 == 0)) return(TRUE) else return(FALSE)
}

# Function to check for internal stop codons
internal_stop_codon <- function(seq, stop_codons) {
  for (codon in stop_codons) {
    stop_match <- matchPattern(codon, seq)
    
    # Check if found internal stop codons are in the correct frame
    if(any(start(stop_match) %% 3 == 1)) {
      
      # Filter stop codons in the correct frame
      stop_match_in_frame <- stop_match[which(start(stop_match) %% 3 == 1)]
      
      # If internal stop codons are not the last triplet return TRUE
      if (!all(end(stop_match_in_frame) == length(seq))) {
        return(TRUE)
      }
    }
  }
  # No internal stop codons return FALSE
  return(FALSE)
}

# Function to validate start and stop codon position and check for internal stop codons
validate_start_stop <- function(seq, start_codons, stop_codons) {
  
  seq <- DNAString(seq)
  
  # Extract first and last 3 nucleotides
  seq_for_start <- substring(seq, first = 1, last = 3)
  seq_for_end <- substring(seq, first = nchar(seq) - 2, last = nchar(seq))
  
  # Define reverse complement of first and last 3 nucleotides
  seq_rev_start <- reverseComplement(seq_for_end)
  seq_rev_end <- reverseComplement(seq_for_start)
  
  for (codon in start_codons) {
    forward_match <- matchPattern(codon, seq_for_start)
    reverse_match <- matchPattern(codon, seq_rev_start)
    
    # Check if forward sequence has start codon
    if (length(forward_match@ranges) != 0) {
      
      # Verify if stop codon is at other end
      for (stop_codon in stop_codons) {
        stop_end <- matchPattern(stop_codon, seq_for_end)
        
        # If stop codon is found, check for internal stop codons
        if (length(stop_end@ranges) != 0) {
          
          # If no internal stop codons are found return TRUE
          if(!internal_stop_codon(seq = seq, stop_codons = stop_codons)) {
            return(TRUE)
            break
          }
        }
      }
    
    # Check if reverse sequence has start codon    
    } else if (length(reverse_match@ranges) != 0) {
      
      # Verify if stop codon is at other end
      for (stop_codon in stop_codons) {
        stop_end <- matchPattern(stop_codon, seq_rev_end)
        
        # If stop codon is found, check for internal stop codons
        if (length(stop_end@ranges) != 0) {
          
          # If no internal stop codons are found return TRUE
          if(!internal_stop_codon(seq = reverseComplement(seq), stop_codons = stop_codons)) {
            return(TRUE)
            break
          }
        }
      }
    }
  }
  # If no start codon and respective stop codon is found  
  return(FALSE)
}

# overall variant validation combining the validation functions
variant_validation <- function(references, start_codons, stop_codons) {
  for(i in 1:nrow(references)){
    
    seq <- substring(template, references$V16[i] + 1, references$V17[i])
    
    ref_seq_index <- grep(paste0("^>", references$V10[i], "$"), readLines(locus_file)) + 1
    
    ref_seq <- variants[ref_seq_index]
    
    if(!check_length_frameshift(seq = seq, ref_seq = ref_seq)) {
      next
    }
    
    if(!validate_start_stop(seq = seq, 
                            start_codons = start_codons, 
                            stop_codons = stop_codons)) {
      next
    } 
    # valid variant found 
    return(TRUE)
  }
  # no valid variant found 
  return(FALSE)
}
