#' Convert JSON (as R list) to data frame
#'
#' This function takes a JSON object (parsed as an R list) and converts it to a data frame,
#' handling nested structures and multiple choice questions.
#'
#' @param json A list object (parsed JSON) with named elements
#' @param all_names Logical. If TRUE (default), all names preserved during the recursive unlisting
#'        are kept. If FALSE, only the last name in each path is kept.
#'
#' @return A data frame with no duplicated column names
#'
#' @examples
#' \dontrun{
#' # Example
#' nested_json <- list(
#'   person = list(
#'     name = "John",
#'     details = list(
#'       age = "42",
#'       active = "TRUE"
#'     )
#'   )
#' )
#' json_to_df(nested_json)
#'
#' # From parsed JSON file
#' library(jsonlite)
#' json_data <- fromJSON("data.json", simplifyVector = FALSE)
#' json_to_df(json_data)
#' }

json_to_df <- function(json, all_names = TRUE) {
  # Check input
  if (!is.list(json)) {
    stop("Input must be a list (parsed JSON)")
  }

  if (length(json) == 0) {
    return(data.frame())
  }

  # Get the names of all elements in the unlist operation
  tryCatch(
    {
      json_unlisted <- unlist(json)
      names_json <- names(json_unlisted)
    },
    error = function(e) {
      stop("Failed to unlist JSON: ", e$message)
    }
  )

  if (is.null(names_json) || length(names_json) == 0) {
    stop("JSON structure has no named elements")
  }

  mult_input_names <- c()
  #### Nested structures ####
  # Solve variable name duplications due to JSON nested structure
  if (any(duplicated(names_json))) {
    # Find variables in nested groups
    group_names <- gsub("\\.([^.]*)$", "", names_json)
    var_names <- gsub(".*\\.", "", names_json)
    is_nested <- group_names != var_names

    # Check if there are any nested structures
    if (!any(is_nested)) {
      stop("Duplicated names detected but no nested structure found")
    }

    # Unique names for repeated variables (in nested groups)
    tryCatch(
      {
        n_nest <- unlist(lapply(json[unique(group_names[is_nested])], length))
      },
      error = function(e) {
        stop("Error processing nested structure: ", e$message)
      }
    )

    for (i in 1:length(n_nest)) {
      tryCatch(
        {
          n_subnest <- lengths(lapply(json[[names(n_nest[i])]], unlist))
        },
        error = function(e) {
          stop(paste0(
            "Error processing subnest for '",
            names(n_nest[i]),
            "': ",
            e$message
          ))
        }
      )

      nest_index <- c()
      # If all subnests length is equal 1 it is not a nested variable, but multiple input question
      if (all(n_subnest == 1)) {
        for (j in 1:length(n_subnest)) {
          nest_index <- c(nest_index, names(n_subnest[j]))
        }
        mult_input_names_j <- paste0(names(n_nest[i]), "_", nest_index)
        names_json[group_names %in% names(n_nest[i])] <- mult_input_names_j
        mult_input_names <- c(mult_input_names, mult_input_names_j)
      } else {
        # For panels and matrix: only add an index
        for (j in 1:length(n_subnest)) {
          if (n_subnest[j] <= 0) {
            warning(paste0(
              "Subnest ",
              j,
              " for '",
              names(n_nest[i]),
              "' has zero or negative length"
            ))
          }
          nest_index <- c(nest_index, rep(j, each = n_subnest[j]))
        }
        names_json[group_names %in% names(n_nest[i])] <- paste0(
          names_json[group_names %in% names(n_nest[i])],
          "_",
          nest_index
        )
      }
    }
  }

  # Check duplicated names (including the first occurrence)
  if (any(duplicated(names_json) | duplicated(names_json, fromLast = TRUE))) {
    stop(paste0(
      "Duplicated names: ",
      paste(unique(names_json[duplicated(names_json)]), collapse = ", ")
    ))
  }

  if (!all_names) {
    names_json <- gsub(".*\\.", "", names_json)
    # Check again for duplicates after removing path
    if (any(duplicated(names_json))) {
      stop(paste0(
        "Duplicated names after removing path (all_names=FALSE): ",
        paste(unique(names_json[duplicated(names_json)]), collapse = ", ")
      ))
    }
  }

  # Create a data frame with proper types
  tryCatch(
    {
      values <- json_unlisted
      names(values) <- names_json

      # Create initial data frame
      df <- as.data.frame(t(as.matrix(values)), stringsAsFactors = FALSE)
      row.names(df) <- NULL

      # Auto-detect and convert column types
      for (col in names(df)) {
        # Try to convert to logical
        if (
          all(
            df[[col]] %in%
              c("TRUE", "FALSE", "true", "false", "True", "False", "T", "F")
          )
        ) {
          df[[col]] <- as.logical(tolower(df[[col]]))
        } else {
          # Try to convert to numeric (only if all values can be converted)
          is_numeric <- suppressWarnings(!any(is.na(as.numeric(df[[col]]))))
          if (is_numeric) {
            df[[col]] <- as.numeric(df[[col]])
          }
          # Otherwise keep as character
        }
      }
    },
    error = function(e) {
      stop("Failed to convert to data frame with proper types: ", e$message)
    }
  )

  #### Multiple choice question (not a panel or matrix) ####
  prev_names <- names(df)

  # Original survey names that finish with a digit in the original survey
  num_names <- names(json)[grepl(".*[0-9]+$", names(json))]

  # Unlisted json names finish with a digit (multiple choice)
  mult_names <- prev_names[grepl(".*[0-9]+$", prev_names)]

  # Unlisted json names finish with an underscore and digit (nested)
  nest_names <- prev_names[grepl(".*_[0-9]+$", prev_names)]

  # True mult_choice_names
  mult_choice_names <- mult_names[
    !(mult_names %in%
      num_names |
      mult_names %in% nest_names |
      mult_names %in% mult_input_names)
  ]

  if (length(mult_choice_names) > 0) {
    new_names <- paste0(
      gsub("[0-9]*$", "", mult_choice_names),
      "_",
      df[mult_choice_names]
    )

    names(df)[names(df) %in% mult_choice_names] <- new_names
    # All multiple choices as TRUE column names
    df[new_names] <- TRUE
  }

  return(df)
}

#' Extract Form Fields from js Survey JSON form
#'
#' Analyzes js survey JSON structure to extract all form fields and convert them to data frame headers.
#' Handles nested structures, panels, matrices, and various question types.
#'
#' @param path Character string with path to JSON file, or a pre-loaded JSON list object
#' @param dictionary Logical. If TRUE, returns both headers and a detailed dictionary (default: FALSE)
#' @param save_header Logical. If TRUE, saves headers as CSV in the same location as input (default: FALSE)
#' @param save_dictionary Logical. If TRUE, saves field dictionary as CSV (default: FALSE)
#'
#' @return If dictionary=FALSE, returns a character vector of column headers.
#'         If dictionary=TRUE, returns a list with two elements:
#'         \itemize{
#'           \item headers: Character vector of column headers
#'           \item dictionary: Data frame with detailed field information
#'         }
#'
#' @examples
#' # Basic usage - extract headers only
#' headers <- form_json_df_header(path="forms/survey.json")
#'
#' # Get both headers and dictionary
#' result <- form_json_df_header(path="forms/survey.json", dictionary=TRUE)
#'
#' # Save dictionary to CSV file
#' form_json_df_header(path="forms/survey.json", save_dictionary=TRUE)

form_json_df_header <- function(
  path,
  dictionary = FALSE,
  save_header = FALSE,
  save_dictionary = FALSE
) {
  # Import data - can also accept a pre-loaded form_json object
  if (is.character(path)) {
    form_json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else if (is.list(path)) {
    form_json <- path # Use the provided list directly
    path <- "form_data.json" # Default filename for saving
  } else {
    stop("Path must be either a file path or a pre-loaded JSON list")
  }

  # Create dictionary
  multiple_choice_types <- c("checkbox", "tagbox")
  form_choice_types <- c("checkbox", "multipletext", "tagbox")

  df <- data.frame(
    form_question = character(),
    form_choice = character(),
    csv_column = character(),
    multiple_choice = logical(),
    n_panel = integer(),
    type = character(),
    stringsAsFactors = FALSE
  )

  # Recursive function to process any element/panel at any nesting level
  process_element <- function(current_element, parent_name = NULL) {
    local_df <- data.frame(
      form_question = character(),
      form_choice = character(),
      csv_column = character(),
      multiple_choice = logical(),
      n_panel = integer(),
      type = character(),
      stringsAsFactors = FALSE
    )

    # Skip if element is NULL or not a list
    if (is.null(current_element) || !is.list(current_element)) {
      return(local_df)
    }

    # Get element name with proper parent prefix if needed
    element_name <- current_element$name

    # Determine if this is a panel/matrix with template elements
    is_panel <- FALSE
    n_panel <- NA

    # Check if this is a panel dynamic
    if (
      !is.null(current_element$maxPanelCount) &&
        length(current_element$maxPanelCount) > 0
    ) {
      is_panel <- TRUE
      n_panel <- current_element$maxPanelCount
    } else if (
      !is.null(current_element$maxRowCount) &&
        length(current_element$maxRowCount) > 0
    ) {
      # Check if this is a matrix dynamic
      is_panel <- TRUE
      n_panel <- current_element$maxRowCount

      # Standardize: Matrix columns are equivalent to panel template elements
      if (
        !is.null(current_element$columns) && length(current_element$columns) > 0
      ) {
        current_element$templateElements <- current_element$columns
      }
    }

    # Process direct questions (not panels or with subelements)
    if (
      !is_panel &&
        (is.null(current_element$elements) ||
          length(current_element$elements) == 0) &&
        (is.null(current_element$templateElements) ||
          length(current_element$templateElements) == 0)
    ) {
      form_question <- element_name
      type <- ifelse(
        !is.null(current_element$type),
        current_element$type,
        current_element$cellType
      )
      multiple_choice <- type %in% multiple_choice_types

      # Process multiple choice questions
      if (type %in% form_choice_types) {
        # Handle choices from question reference
        if (
          !is.null(current_element$choicesFromQuestion) &&
            length(current_element$choicesFromQuestion) > 0
        ) {
          ref_choices <- df$form_choice[
            df$form_question == current_element$choicesFromQuestion
          ]
          if (length(ref_choices) > 0) {
            current_element$choices <- as.list(ref_choices)
          }
        }

        # Unify items and choices
        if (
          !is.null(current_element$items) && length(current_element$items) > 0
        ) {
          current_element$choices <- current_element$items
        }

        # Add Other choices if present
        if (
          (!is.null(current_element$otherText) &&
            length(current_element$otherText) > 0) ||
            (!is.null(current_element$showOtherItem) &&
              current_element$showOtherItem)
        ) {
          if (is.null(current_element$choices)) {
            current_element$choices <- list()
          }
          current_element$choices[[
            length(current_element$choices) + 1
          ]] <- "other"
        }

        # Process each choice
        if (
          !is.null(current_element$choices) &&
            length(current_element$choices) > 0
        ) {
          for (k in 1:length(current_element$choices)) {
            choice <- current_element$choices[[k]]

            # Extract the choice value properly
            if (is.list(choice)) {
              form_choice <- ifelse(
                !is.null(choice$value) && length(choice$value) > 0,
                choice$value,
                ifelse(
                  !is.null(choice$name) && length(choice$name) > 0,
                  choice$name,
                  paste0("choice_", k)
                )
              )
            } else {
              form_choice <- choice
            }

            csv_column <- paste0(form_question, "_", form_choice)

            k_row <- data.frame(
              form_question = form_question,
              form_choice = form_choice,
              csv_column = csv_column,
              multiple_choice = multiple_choice,
              n_panel = n_panel,
              type = type,
              stringsAsFactors = FALSE
            )
            local_df <- rbind(local_df, k_row)
          }
        } else {
          # Handle case where choices might be dynamically generated
          csv_column <- form_question
          row <- data.frame(
            form_question = form_question,
            form_choice = NA,
            csv_column = csv_column,
            multiple_choice = multiple_choice,
            n_panel = n_panel,
            type = type,
            stringsAsFactors = FALSE
          )
          local_df <- rbind(local_df, row)
        }
      } else {
        # Handle simple questions without choices
        csv_column <- form_question
        row <- data.frame(
          form_question = form_question,
          form_choice = NA,
          csv_column = csv_column,
          multiple_choice = multiple_choice,
          n_panel = n_panel,
          type = type,
          stringsAsFactors = FALSE
        )
        local_df <- rbind(local_df, row)
      }
    }

    # Process template elements (for panel dynamic or matrix dynamic)
    if (
      !is.null(current_element$templateElements) &&
        length(current_element$templateElements) > 0
    ) {
      for (i in 1:length(current_element$templateElements)) {
        template_element <- current_element$templateElements[[i]]

        # Process each template element recursively
        template_df <- process_element(template_element, element_name)

        # Ensure the panel count is properly set for all template elements
        if (nrow(template_df) > 0) {
          template_df$n_panel <- n_panel
          local_df <- rbind(local_df, template_df)
        }
      }
    }

    # Process regular subelements (nested panels or questions)
    if (
      !is.null(current_element$elements) && length(current_element$elements) > 0
    ) {
      for (i in 1:length(current_element$elements)) {
        sub_element <- current_element$elements[[i]]

        # Process each subelement recursively
        sub_df <- process_element(sub_element, element_name)

        # Keep the panel count context for nested elements
        if (is_panel && nrow(sub_df) > 0) {
          sub_df$n_panel <- n_panel
        }

        local_df <- rbind(local_df, sub_df)
      }
    }

    return(local_df)
  }

  # Process all pages
  if (!is.null(form_json$pages)) {
    for (i in 1:length(form_json$pages)) {
      page <- form_json$pages[[i]]

      # Process elements on this page
      if (!is.null(page$elements)) {
        for (j in 1:length(page$elements)) {
          element_to_process <- page$elements[[j]]
          page_df <- process_element(element_to_process)
          if (nrow(page_df) > 0) {
            df <- rbind(df, page_df)
          }
        }
      }
    }
  }

  # If no pages found, try direct processing
  if (nrow(df) == 0 && is.list(form_json)) {
    # Try to process the whole form_json as a single element
    df <- process_element(form_json)
  }

  # Remove NA multiple choices from df
  if (nrow(df) > 0) {
    df <- df[!(df$multiple_choice & is.na(df$form_choice)), ]
  }

  # Convert to CSV headers

  # Initialize an empty output vector
  csv_headers <- c()

  # Loop over each row of the input table
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      csv_column <- df$csv_column[i]
      n_panel <- df$n_panel[i]
      if (is.na(n_panel)) {
        csv_headers <- c(csv_headers, csv_column)
      } else {
        csv_headers <- c(csv_headers, paste0(csv_column, "_", seq_len(n_panel)))
      }
    }
  }

  if (save_dictionary && nrow(df) > 0) {
    dictionary_path <- gsub("\\.json", "_dictionary.csv", path)
    write.table(df, dictionary_path, sep = ";", row.names = FALSE)
  }

  if (save_header && length(csv_headers) > 0) {
    headers_path <- gsub("\\.json", "_header.csv", path)
    write.table(
      matrix(csv_headers, nrow = 1),
      headers_path,
      sep = ";",
      row.names = FALSE,
      col.names = FALSE
    )
  }

  if (dictionary) {
    return(list(headers = csv_headers, dictionary = df))
  } else {
    return(csv_headers)
  }
}

#' Convert json js survey answer to data frame
#'
#' @param path json js answer file or files directory
#' @param all_header if true, form_json_df_header() is used to find all form fields, not only answered ones
#' @param save_csv If true, a csv is generated in the same location
#' @param mult_answer If true, multiple json file answers are converted to a data frame, one json per row
#'
#' @return data frame with no duplicated columns
#'
#' @examples
#'
#' answer_json_to_df(path="input_files/user/bl1_v3_mod/bsg.json")

answer_json_to_df <- function(
  path,
  all_header = TRUE,
  save_csv = TRUE,
  save_json = FALSE,
  add_rows = TRUE
) {
  if (grepl("\\.json$", path)) {
    # If input if path to input document
    json_str <- readLines(path, warn = FALSE, encoding = "UTF-8")
    json_str <- paste(json_str, collapse = "")
  } else {
    json_str <- path
  }

  # Remove escape characters except UTF-8 special characters
  if (grepl("\\\\", json_str)) {
    clean_json_str <- gsub("\\\\", "TEMP_MARKER", json_str)
    clean_json_str <- gsub(
      "TEMP_MARKER(?=u[0-9a-fA-F]{4})",
      "\\\\",
      clean_json_str,
      perl = TRUE
    )
    clean_json_str <- gsub("TEMP_MARKER", "", clean_json_str)
    answer_json <- jsonlite::fromJSON(
      txt = clean_json_str,
      simplifyVector = FALSE
    )
  } else {
    answer_json <- jsonlite::fromJSON(txt = json_str, simplifyVector = FALSE)
  }

  #path to output file

  if (length(answer_json$mov_id) > 0) {
    survey_id <- "mov"
    path <- paste0("input_files/user/", answer_json$farm_id, "/mov.json")
  } else if (length(answer_json$farm_id) > 0) {
    survey_id <- "bsg"
    path <- paste0("input_files/user/", answer_json$farm_id, "/bsg.json")
  } else {
    path <- paste0("input_files/user/", answer_json$farm_id, "/unk.json")
  }

  #json to dataframe
  answer_df <- json_to_df(answer_json, all_names = FALSE)

  #find all headers
  if (all_header) {
    # normalize version string to underscores to match filename conventions
    ver_norm <- gsub("\\.", "_", answer_json$version)
    form_path <- paste0(
      "forms/",
      answer_json$survey_id,
      "_form_",
      ver_norm,
      ".json"
    )

    header_info <- form_json_df_header(path = form_path, dictionary = TRUE)
    header <- header_info$headers # was header_info$header (fix)
    dictionary <- header_info$dictionary

    #Add all headers
    answer_df[header[!header %in% names(answer_df)]] <- NA

    #### Fix multiple-choice questions ####

    #Multiple-choice columns list
    mult_choice <- names(answer_df)[
      gsub("_[0-9]*$", "", names(answer_df)) %in%
        dictionary$csv_column[dictionary$multiple_choice]
    ]

    for (i in 1:length(mult_choice)) {
      #name of the column with the format "question_choice" (logic)
      ok_col <- mult_choice[i]

      name_col <- gsub("_[0-9]*$", "", ok_col)
      n_panel <- gsub(paste0(name_col), "", ok_col)

      #name of the column  with the format "question" (character)
      #it can look like a false single input column when only one input is given to a mult. choice question
      raw_col <- dictionary$form_question[dictionary$csv_column == name_col]

      #value to look for in the "question" column
      ok_value <- dictionary$form_choice[dictionary$csv_column == name_col]

      #look for ok_value in:
      # - panel single input multiple_choice columns (psm)

      rm_cols_psm <- names(answer_df)[grepl(raw_col, names(answer_df))]

      if (length(rm_cols_psm) > 0) {
        #If name_col_1 is null or NA, check if name_col_1 is present in any rm_cols or raw_col, else NA
        name_col_1 <- paste0(name_col, "_1")
        answer_df[[name_col_1]] <- ifelse(
          is.null(answer_df[[name_col_1]]) || is.na(answer_df[[name_col_1]]),
          ifelse(ok_value %in% answer_df[rm_cols_psm], TRUE, NA),
          answer_df[[name_col_1]]
        )
      }

      # - nested multiple_choice columns (nm)
      # - single input multiple_choice columns (sm)

      rm_cols <- names(answer_df)[grepl(
        unique(paste0(raw_col, "[0-9]*", n_panel)),
        names(answer_df)
      )]

      if (length(rm_cols) > 0) {
        #If ok_col is NA, check if ok_value is present in any rm_cols or raw_col, else NA
        answer_df[[ok_col]] <- ifelse(
          is.na(answer_df[[ok_col]]),
          ifelse(ok_value %in% answer_df[rm_cols], TRUE, NA),
          answer_df[[ok_col]]
        )
      }
    }

    #### Fix single row panel single-choice questions ####

    panel_single <- names(answer_df)[
      names(answer_df) %in% dictionary$csv_column[!is.na(dictionary$n_panel)]
    ]

    if (length(panel_single) > 0) {
      for (i in 1:length(panel_single)) {
        #name of the panel column (no rows)
        raw_col <- panel_single[i]

        #name of the first panel row column
        name_col_1 <- paste0(raw_col, "_1")

        #If name_col_1 is NA, return raw_col
        answer_df[[name_col_1]] <- ifelse(
          is.null(answer_df[[name_col_1]]) || is.na(answer_df[[name_col_1]]),
          answer_df[[raw_col]],
          answer_df[[name_col_1]]
        )
      }
    }

    #Remove columns that are not part of the definitive header
    answer_df[!names(answer_df) %in% header] <- NULL

    #Detect data type
    answer_df <- type.convert(answer_df, as.is = TRUE)
  }

  #save output: check if farm directory exists
  if (save_csv | save_json) {
    #check if farm directory exists
    if (!file.exists(paste0("input_files/user/", answer_json$farm_id))) {
      dir.create(paste0("input_files/user/", answer_json$farm_id))
    }
  }

  #write csv
  if (save_csv) {
    if (add_rows) {
      #check if csv exists
      prev_csv <- paste0(
        "input_files/user/",
        answer_json$farm_id,
        "/",
        survey_id,
        ".csv"
      )
      if (file.exists(prev_csv)) {
        prev_df <- read.csv(
          prev_csv,
          sep = ";",
          stringsAsFactors = TRUE,
          na.strings = c(NA, "NA", "")
        )
        answer_df <- dplyr::bind_rows(prev_df, answer_df)
        answer_df <- dplyr::distinct(answer_df)
      }
    }

    if (save_json) {
      answer_json_file <- jsonlite::toJSON(answer_json, auto_unbox = TRUE)
      answer_json_path <- paste0(
        "input_files/user/",
        answer_json$farm_id,
        "/",
        survey_id,
        ifelse(survey_id == "mov", paste0("_", answer_json$mov_id), ""),
        ".json"
      )
      write(answer_json_file, answer_json_path)
    }

    write.table(
      answer_df,
      file = gsub("\\.json", ".csv", path),
      sep = ";",
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
  }

  return(answer_df)
}


#' Convert farmrisk json js survey answer to data frame
#'
#' @param json json js answer string
#' @return data frame with no duplicated columns
parse_jsurvey <- function(json) {
  answer_json <- jsonlite::fromJSON(txt = json, simplifyVector = FALSE)
  answer_df <- json_to_df(answer_json, all_names = FALSE)

  if (is.null(answer_df$survey_id)) {
    answer_df$survey_id <- "bsg_cattle"
    answer_df$version <- "v4_4"
  }

  ver_norm <- gsub("\\.", "_", answer_df$version)
  form_path <- paste0(
    "forms/",
    answer_df$survey_id,
    "_form_",
    ver_norm,
    ".json"
  )

  header_info <- form_json_df_header(path = form_path, dictionary = TRUE)
  header <- header_info$headers # was header_info$header
  dictionary <- header_info$dictionary

  #Add all headers
  answer_df[header[!header %in% names(answer_df)]] <- NA

  #### Fix multiple-choice questions ####

  #Multiple-choice columns list
  mult_choice <- names(answer_df)[
    gsub("_[0-9]*$", "", names(answer_df)) %in%
      dictionary$csv_column[dictionary$multiple_choice]
  ]

  for (i in 1:length(mult_choice)) {
    #name of the column with the format "question_choice" (logic)
    ok_col <- mult_choice[i]

    name_col <- gsub("_[0-9]*$", "", ok_col)
    n_panel <- gsub(paste0(name_col), "", ok_col)

    #name of the column  with the format "question" (character)
    #it can look like a false single input column when only one input is given to a mult. choice question
    raw_col <- dictionary$form_question[dictionary$csv_column == name_col]

    #value to look for in the "question" column
    ok_value <- dictionary$form_choice[dictionary$csv_column == name_col]

    #look for ok_value in:
    # - panel single input multiple_choice columns (psm)

    rm_cols_psm <- names(answer_df)[grepl(raw_col, names(answer_df))]

    if (length(rm_cols_psm) > 0) {
      #If name_col_1 is null or NA, check if name_col_1 is present in any rm_cols or raw_col, else NA
      name_col_1 <- paste0(name_col, "_1")
      answer_df[[name_col_1]] <- ifelse(
        is.null(answer_df[[name_col_1]]) || is.na(answer_df[[name_col_1]]),
        ifelse(ok_value %in% answer_df[rm_cols_psm], TRUE, NA),
        answer_df[[name_col_1]]
      )
    }

    # - nested multiple_choice columns (nm)
    # - single input multiple_choice columns (sm)

    rm_cols <- names(answer_df)[grepl(
      unique(paste0(raw_col, "[0-9]*", n_panel)),
      names(answer_df)
    )]

    if (length(rm_cols) > 0) {
      #If ok_col is NA, check if ok_value is present in any rm_cols or raw_col, else NA
      answer_df[[ok_col]] <- ifelse(
        is.na(answer_df[[ok_col]]),
        ifelse(ok_value %in% answer_df[rm_cols], TRUE, NA),
        answer_df[[ok_col]]
      )
    }
  }

  #### Fix single row panel single-choice questions ####

  panel_single <- names(answer_df)[
    names(answer_df) %in% dictionary$csv_column[!is.na(dictionary$n_panel)]
  ]

  if (length(panel_single) > 0) {
    for (i in 1:length(panel_single)) {
      #name of the panel column (no rows)
      raw_col <- panel_single[i]

      #name of the first panel row column
      name_col_1 <- paste0(raw_col, "_1")

      #If name_col_1 is NA, return raw_col
      answer_df[[name_col_1]] <- ifelse(
        is.null(answer_df[[name_col_1]]) || is.na(answer_df[[name_col_1]]),
        answer_df[[raw_col]],
        answer_df[[name_col_1]]
      )
    }
  }

  #Remove columns that are not part of the definitive header
  answer_df[!names(answer_df) %in% header] <- NULL

  #Detect data type
  answer_df <- type.convert(answer_df, as.is = TRUE)

  return(answer_df)
}
