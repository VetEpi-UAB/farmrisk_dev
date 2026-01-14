#' GID code to region name
#' 
#' @param region_code values, as string
#'
#' @return region_name values, as string
#' 
#' @examples
#' gid_to_name(region_code)


#FAO gadm codes (https://data.apps.fao.org/catalog/dataset/gadm-codes/resource/478df03d-cf9d-4da9-8fa8-652830c56109)
#look for local file (filtered for EU members), if it is not found, download from url
administrative_levels<-tryCatch(read.csv("input_files/admin/administrative_levels.csv", sep=";" ,stringsAsFactors=FALSE),
                                error=function(e){
                                  #download from url
                                  url<-"https://data.apps.fao.org/catalog/dataset/d4c071c1-3e5a-46f7-b335-864d94cf3677/resource/478df03d-cf9d-4da9-8fa8-652830c56109/download/gadm36_3_fid.csv"
                                  administrative_levels<-read.csv(url)
                                })



gid_to_name<-function(value){
  gid<-get_region_level(value)
  name<-gsub("GID","NAME",gid)
  region_name<-c()
  
  for(i in 1:length(value)){
    index<-as.character(administrative_levels[[gid[i]]])==as.character(value[i])
    region_name_i<-as.character(unique(administrative_levels[[name[i]]][index]))
    region_name<-c(region_name,region_name_i)
  }
  region_name<-region_name[!is.na(region_name)]
  return(region_name)
}


#' Convert keys to literal descriptions
#'
#' @param value A vector of keys to be converted to literal descriptions
#' @param factor Optional. The factor to use in case of ambiguity
#'
#' @return A vector of literal descriptions corresponding to the input keys
#'
#' @examples
#' key_to_lit(c("cattleunder8", "T3"))
#' key_to_lit(c("cattleunder8", "T3"), factor = "health_status")
#'
#' @export
key_to_lit <- function(value, factor = NULL) {
  lit <- character(length(value))
  
  for (i in seq_along(value)) {
    if (!is.null(factor)) {
      lit_i <- key_dictionary$lit[as.character(key_dictionary$key) == as.character(value[i]) & 
                                    key_dictionary$factor == factor]
    } else {
      lit_i <- key_dictionary$lit[as.character(key_dictionary$key) == as.character(value[i])]
    }
    
    # If key is found, use the first match; otherwise, return the original key
    lit[i] <- if (length(lit_i) > 0) as.character(lit_i[1]) else as.character(value[i])
  }
  
  return(lit)
}

#' Add Line Breaks to Text While Preserving Word Integrity
#' 
#' This function takes a text string and adds line breaks to ensure each line
#' does not exceed a specified number of characters, while keeping words intact.
#' 
#' @param text A character string containing the text to be formatted
#' @param n_char An integer specifying the maximum number of characters per line
#' 
#' @return A character string with added line breaks
#' 
#' @examples
#' text <- "This is a very long sentence that needs to be broken into multiple lines"
#' result <- line_breaks(text, 20)
#' cat(result)
#' 
#' @export
line_breaks <- function(text, n_char) {
  # Vectorized function to add line breaks
  vapply(text, function(text) {
    # Split text into words
    words <- strsplit(text, " ")[[1]]
    
    # Initialize variables
    current_line <- ""
    result <- ""
    
    # Process each word
    for (word in words) {
      # Check if adding the next word exceeds n_char
      if (nchar(current_line) + nchar(word) > n_char) {
        # Add current line to result and start new line
        result <- paste0(result, current_line, "\n")
        current_line <- word
      } else {
        # Add word to current line
        current_line <- if (current_line == "") word else paste(current_line, word)
      }
    }
    
    # Add the last line
    result <- paste0(result, current_line)
    
    return(result)
  }, character(1))
}
