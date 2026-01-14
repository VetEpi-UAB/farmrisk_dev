unify_scripts <- function(script_name, files, logic = NULL) {
  # Obtain file information and add columns for name, script, and version
  files <- file.info(files)
  files$name <- row.names(files)
  files$script <- gsub("_v[0-9].*", "", as.character(files$name))
  files$version <- gsub(
    paste(files$script, collapse = "|"),
    "",
    as.character(files$name)
  )
  files$version <- gsub("^_", "", as.character(files$version))

  # Create an empty vector to store the last version of each script
  last_v_files <- vector()

  # Specify the target file for appending
  appended_file <- "unified_script/unified_bsg_risk.R"
  fun_appended_file <- "unified_script/farmrisk_cattle_header.R"

  # Get the current date
  current_date <- gsub("-", "_", Sys.Date())

  # Create a new file name based on the current date
  if (is.null(script_name)) {
    script_name <- paste(
      "unified_script/unified_bsg_risk_v3_",
      current_date,
      ".R",
      sep = ""
    )
    #Function file
    script_func_name <- paste(
      "unified_script/unified_bsg_risk_v3_farmrisk_",
      current_date,
      ".R",
      sep = ""
    )
  } else {
    script_func_name <- paste0("R/farmrisk_", script_name)
  }

  # If the new file already exists, remove it
  if (file.exists(script_name)) {
    file.remove(script_name)
  }

  if (file.exists(script_func_name)) {
    file.remove(script_func_name)
  }

  # Copy an empty R file to the new file name
  file.copy(appended_file, script_name)

  #Create automatic function to call automatically the scrip (no source)
  file.copy(fun_appended_file, script_func_name)

  # Iterate through each script, extract the last version, create a corresponding R file,
  # and append it to the new file
  for (i in files$script) {
    last_v <- max(files$version[files$script == i])
    last_v_i <- paste(i, last_v, sep = "_")
    last_v_R <- paste("unified_script/", gsub(".qmd", ".R", last_v_i), sep = "")
    knitr::purl(last_v_i, documentation = 2, output = last_v_R)

    #Wrap script to run it conditionally
    if (i == files$script[1]) {
      write(
        paste0("script_date <- '", Sys.time(), "'"),
        file = script_name,
        append = TRUE
      )
      write(
        paste0(
          "repo <- '",
          system("git config --get remote.origin.url", intern = TRUE),
          "'"
        ),
        file = script_name,
        append = TRUE
      )
      write(
        paste0(
          "last_commit <- ",
          "'",
          system("git rev-parse HEAD", intern = TRUE),
          "'"
        ),
        file = script_name,
        append = TRUE
      )
      write(
        paste0(
          "commit_date <- ",
          as.numeric(system("git log -1 --format=%ct", intern = TRUE))
        ),
        file = script_name,
        append = TRUE
      )
      write(
        paste0(
          "unify_info <- ",
          "'",
          script_name,
          " unified from: ",
          paste0(row.names(files), collapse = ", "),
          "'"
        ),
        file = script_name,
        append = TRUE
      )
    }

    if (i %in% names(logic)) {
      write(paste0("if(", logic[i], "){"), file = script_name, append = TRUE)
      write(
        paste0("if(", logic[i], "){"),
        file = script_func_name,
        append = TRUE
      )

      file.append(script_name, last_v_R)
      file.append(script_func_name, last_v_R)

      write("}", file = script_name, append = TRUE)
      write("}", file = script_func_name, append = TRUE)
    } else {
      file.append(script_name, last_v_R)
      file.append(script_func_name, last_v_R)
    }
  }

  write(
    paste0("assign(\"intro\",intro,envir=parent.frame())\n}"),
    file = script_func_name,
    append = TRUE
  )

  #Replace #' in function body with # to avoid document() issues, but keep #' in roxygen esqueleton
  file_contents <- readLines(script_func_name)
  header_length <- length(readLines(fun_appended_file))
  file_header <- file_contents[1:header_length]
  file_body <- file_contents[(header_length + 1):length(file_contents)]
  updated_body <- gsub(x = file_body, pattern = "#'", replacement = "#")
  updated_contents <- c(file_header, updated_body)
  cat(updated_contents, file = script_func_name, sep = "\n")
}

# Cattle file names
cattle_files <- c(
  "mov_v4.qmd",
  "farm_visits_v2.qmd",
  "farm_neighbours_v2.qmd",
  "farm_wildlife_v2.qmd",
  "output_v4.qmd"
)

# Set condition to run scripts (logic)
cattle_logic <- c(
  mov = "exists('mov')",
  farm_visits = "sum(!is.na(bsg$visit_type_1),!is.na(bsg$visit_veh_type_1))>0",
  farm_wildlife = "bsg$outdoors_access&&any(!bsg$outdoors_fencing_perimeter,bsg$outdoors_fencing_wildlife)"
)

#unify_scripts("cattle.R", cattle_files, cattle_logic)
#WRAPPER FUNCION FOR CATTLE
unify_cattle <- function() {
  unify_scripts("cattle.R", cattle_files, cattle_logic)
}
#WRAPPER FUNCION FOR SET UP
unify_setup <- function() {
  unify_scripts("run_farmrisk.R", "set_up_v3.qmd")
}
#unify_scripts("run_farmrisk_local.R", "set_up_v2.qmd")
