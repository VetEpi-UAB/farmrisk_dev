#' @title Regional Code Management Functions
#' @description A collection of functions for handling regional codes and hierarchical joins
#' 
#' Get the lowest level (more detailed) region code
#'
#' @param data dataframe with gid0, gid1, gid2 and gid3 data
#' @param pathway Optional pathway filter
#' @param module Optional module filter
#' @return A dataframe with region_code column
#' @export
#' @examples
#' get_region_code(data = mov, module = "purchase")
get_region_code <- function(data, module = NULL, pathway = NULL) {
  module <- ifelse(is.null(module), "", module)
  pathway <- ifelse(is.null(pathway), "", pathway)
  key <- ifelse(module == "" & pathway == "", "", paste(pathway, module, sep = "_"))
  
  data %>%
    select(matches(paste0(".*", key, ".*gid[0-3]")), ends_with("_id")) %>%
    pivot_longer(
      cols = matches(paste0(".*", key, ".*gid[0-3]")),
      names_to = "gid_level",
      values_to = "value"
    ) %>%
    mutate(gid_level = factor(gid_level, ordered = TRUE, levels = sort(unique(gid_level)))) %>%
    group_by(across(c(-gid_level, -value))) %>%
    filter(!is.na(value)) %>%
    reframe(region_code = value[which.max(gid_level)])
}

#' Get region level of region_code
#'
#' @param data dataframe or vector containing region codes
#' @param key column name containing region codes if data is a dataframe
#' @return Region code GID level
#' @export
#' @examples
#' get_region_level(value = "ESP.2_1")
get_region_level <- function(data, key = "region_code") {
  value <- if (is.data.frame(data)) data[[key]] else data
  
  ifelse(grepl("[A-Z]{3}.*", value),  # check GID format (starts with ISO 3)
         paste0("GID_", nchar(gsub("[^.]", "", value))),  # if true: GID level based on number of points
         value  # else, return literal value, e.g., "EU" or "NA"
  )
}

#' Get all region levels of region_code
#'
#' @param data dataframe or vector with region codes in GID format
#' @param key column name containing region codes if data is a dataframe
#' @param only_higher logical, whether to return only higher level codes
#' @return List of region code GID levels
#' @export
#' @examples
#' get_all_region_levels(value = "ESP.2_1")
gid_levels <- function(data, key = "region_code", only_higher = FALSE) {
  value_list <- if (is.data.frame(data)) data[[key]] else data
  gid_n <- paste0("GID_", 0:3)
  gid_levels_list <- vector("list", 4)
  names(gid_levels_list) <- gid_n
  
  if (!length(value_list) > 0) {
    return(gid_levels_list)
  }
  
  for (i in seq_along(value_list)) {
    value <- value_list[i]
    higher_levels <- unique(c(
      value,
      gsub("\\.\\d*_", "_", value),  # gid_minus1
      gsub("\\.\\d*.\\d*_", "_", value),  # gid_minus2
      gsub("\\.\\d*.\\d*.\\d*_\\d*", "", value)  # gid_minus3
    ))
    
    # Clean "_d" in GID3 (ISO3)
    higher_levels <- unique(ifelse(grepl("\\.", higher_levels), 
                                   higher_levels, 
                                   gsub("_\\d", "", higher_levels)))
    
    names(higher_levels) <- get_region_level(higher_levels)
    
    gid_levels <- if (only_higher) {
      higher_levels
    } else {
      levels <- rep(NA, length(gid_n))
      names(levels) <- gid_n
      levels[intersect(names(higher_levels), names(levels))] <- 
        higher_levels[intersect(names(higher_levels), names(levels))]
      levels
    }
    
    for (level in gid_n) {
      gid_levels_list[[level]] <- c(gid_levels_list[[level]], gid_levels[level])
    }
  }
  
  return(gid_levels_list)
}

#' Hierarchical Join by region_code
#'
#' Based on: Barks P (2022). hmatch: Tools for Cleaning and Matching 
#' Hierarchically-Structured Data.
#'
#' @param x,y A pair of data frames to merge with region GID codes
#' @param by Column names to join by (NULL for natural join)
#' @param keys Additional keys for grouping
#' @param add_keys Extra keys to include
#' @return Merged dataframe
#' @export
#' @examples
#' hjoin_region(x = visit_data, y = visit_region)
hjoin_region <- function(x, y, by = NULL, keys = NULL, add_keys = NULL, data_keys=NULL) {
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
    message("Hierarchical Join by ", paste(by, collapse = ", "))
  }
  
  if (is.null(keys)) {
    if(is.null(data_keys)){
      data_keys<-mcmodule::set_data_keys()
    }
    keys_x <- names(select(x, ends_with("_id")))
    message(" Keys x: ", paste(keys_x, collapse = ", "))
    
    y_name <- deparse(substitute(y))
    keys_y <- data_keys[[y_name]]$keys
    message(" Keys y: ", paste(keys_y, collapse = ", "))
    
    if (!is.null(add_keys)) {
      message(" Added Keys: ", paste(add_keys, collapse = ", "))
    }
    keys <- unique(c(add_keys, keys_x, keys_y))
  }
  
  # Ensure region_code exists and is character
  if (!("region_code" %in% names(x))) {
    x$region_code <- get_region_code(x)$region_code
  }
  if (!("region_code" %in% names(y))) {
    y$region_code <- get_region_code(y)$region_code
  }
  
  x <- x %>% mutate(region_code = as.character(region_code))
  y <- y %>% mutate(region_code = as.character(region_code))
  
  #### Down -> up loop ####
  xy_join_down <- x %>% semi_join(y, by = by)
  x_gid_pre <- x
  
  for (i in 3:1) {
    x_gid_post <- x_gid_pre %>%
      anti_join(y, by = by) %>%
      mutate(
        region_code_next = gid_levels(region_code)[[i]],
        region_code = tryCatch(
          ifelse(is.na(region_code_next), region_code, region_code_next),
          error = function(e) return(region_code)
        ),
        region_code_next = NULL
      )
    
    xy_join_down <- x_gid_post %>%
      semi_join(y, by = by) %>%
      bind_rows(xy_join_down)
    
    x_gid_pre <- x_gid_post
  }
  
  #### Up -> down loop ####
  xy_join_up <- x %>% semi_join(y, by = by)
  x_gid_pre <- x
  
  for (i in 1:3) {
    x_gid_post <- x_gid_pre %>%
      anti_join(y, by = by) %>%
      mutate(
        region_code_next = gid_levels(region_code)[[i]],
        region_code = tryCatch(
          ifelse(is.na(region_code_next), region_code, region_code_next),
          error = function(e) return(region_code)
        ),
        region_code_next = NULL
      )
    
    xy_join_up <- x_gid_post %>%
      semi_join(y, by = by) %>%
      bind_rows(xy_join_up)
    
    x_gid_pre <- x_gid_post
  }
  
  #### Final join ####
  if (all(xy_join_down == xy_join_up, na.rm = TRUE)) {
    xy_join <- xy_join_down %>% left_join(y, by = by)
  } else {
    xy_join <- xy_join_down %>%
      bind_rows(xy_join_up) %>%
      left_join(y, by = by) %>%
      group_by_at(keys[!keys %in% "region_code"]) %>%
      mutate(
        n = 1,
        duplicated = cumsum(n) > 1
      ) %>%
      ungroup() %>%
      filter(!duplicated) %>%
      select(-n, -duplicated)
  }
  
  return(xy_join)
}
