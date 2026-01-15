#' ---
#' title: "Set-up"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 
#' ## Default settings
#' 
## ----------------------------------------------------------------------------------------------------------------
defaults_set<-list(
    user_id=NULL,
    farm_id=NULL,
    risk_days=360,
    calc_summary = FALSE,
    admin_wif = TRUE,
    model_analysis = FALSE,
    input_path="input_files/",
    output_path="output_files/",
    forms_path="forms/"
  )

# Assign sefault settings
for(i in 1:length(defaults_set)){
  if(!exists(names(defaults_set[i]))||is.null(get(names(defaults_set[i])))){
    assign(names(defaults_set[i]), defaults_set[[i]])
    assign(names(defaults_set[i]), defaults_set[[i]])
  }else{
    assign(names(defaults_set[i]), get(names(defaults_set[i])))
  }
}

# Validate paths
if (!file.exists(input_path)) stop("Input path does not exist: ",input_path)
if (!file.exists(output_path)) stop("Output path does not exist: ", output_path)
if (!file.exists(forms_path)) stop("Forms path does not exist: ", forms_path)

#' 
#' ## Load libraries
#' 
## ----------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  # Load packages
  library(mc2d)      # Monte-Carlo simulations
  library(mcmodule)  # Modular Monte-Carlo
  library(dplyr)     # Data manipulation
  library(tidyr)     # Data cleaning
  library(ggplot2)   # Plots
  library(jsonlite)     # JSON manipulation
})

#' 
#' ## Load custom functions
#' 
## ----------------------------------------------------------------------------------------------------------------
if(!exists("loaded_functions_time")){

  # Get list of R script files in input directory and subdirectories
  custom_functions <- list.files(
    path = "R/",
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Source each R script file
  invisible(
    lapply(custom_functions, function(script) {
      tryCatch({
        source(script)
      }, error = function(e) {
        warning(sprintf("Error sourcing %s: %s\n", script, e$message))
      })
    })
  )

  loaded_functions_time<-Sys.time()

  message("custom_functions.R loaded in environment (",loaded_functions_time,")\n")
  
  
}else{
  
  message("custom_functions.R were already in the environment (last load: ",loaded_functions_time,")")
  
}


#' 
#' ## Get farm_id
#' 
## ----------------------------------------------------------------------------------------------------------------
# Get arguments from the command line
args = commandArgs(trailingOnly = TRUE)


# Check if there is at least one argument; if not, use a manual user_id
if (!length(args) == 0) {
  # if first argument if farm_id
  if (args[1]%in%list.files(path = paste0(input_path, "user/"))){
    farm_id<-args[1]
    if(length(args)>1){
      risk_days<-as.numeric(args[2])*30
    }
  # if fisrt argument is user_id
  } else {
    # Read user configuration
    user_config <- read.csv(
      paste0(input_path,"user/id/", args[1], ".csv"),
      sep = ";",
      stringsAsFactors = FALSE,
      header = FALSE
    )
    
    # Store user information in envir environment
    assign("farm_id", user_config[1,1])
    assign("risk_days", user_config[1,2] * 30)
  }
  
}

#Stop if farm data was requested and not found
if(!farm_id%in%list.files(path = paste0(input_path, "user/"))) stop(farm_id," not found in ",paste0(input_path, "user/"))

#' 
#' ## Display settings
#' 
## ----------------------------------------------------------------------------------------------------------------
if(!exists("script_date")) script_date <- Sys.time()
if(!exists("repo")) repo <- system("git config --get remote.origin.url", intern=TRUE)
if(!exists("last_commit")) last_commit<-system("git rev-parse HEAD", intern=TRUE)
if(!exists("commit_date")) commit_date<-as.numeric(system("git log -1 --format=%ct", intern=TRUE))

# Display simulation settings
message("\nSIMULATION SETTINGS: \n",
        "\n - Simulation date: ",as.POSIXct(Sys.time()),
        "\n - Script:",
        "\n    Unification date: ", as.POSIXct(script_date),
        "\n    Repository: ", repo,
        "\n    Last commit: ", last_commit,
        "\n    Last commit date: ", as.POSIXct(commit_date),
        "\n - user_id: ", user_id,
        "\n - farm_id: ", farm_id,
        "\n - Risk timeframe: ", paste(risk_days, "days"),
        "\n - Calculate all summaries: ", calc_summary,
        "\n - Automatic what-if scenarios: ", admin_wif,
        "\n - Analyse model sensitivity: ", model_analysis,
        "\n - Inputs path: ", input_path,
        "\n - Outputs path: ", output_path,
        "\n - Forms path: ", forms_path
)

#' 
#' ## Input files
#' 
#' ### User
#' 
#' #### Load files
#' 
## ----------------------------------------------------------------------------------------------------------------
# Process admin files
user_path <- paste0(input_path, "user/",farm_id,"/")
user_names <- list.files(path = user_path, pattern = "*.csv", recursive = TRUE)
user_files <- paste(user_path, user_names, sep = "")

user_files_info <- file.info(paste0(user_path, list.files(user_path)))

# Check bsg file modifications
bsg_change <- TRUE
if (file.exists(paste0(user_path, "bsg.csv"))) {
  bsg_change <- user_files_info[paste0(user_path, "bsg.csv"), "mtime"] <
    max(user_files_info[grepl("bsg.json$", row.names(user_files_info)), "mtime"])
}

if (bsg_change){
    # Process bsg file
  bsg_path <- paste0(user_path, "bsg.json")
  if (file.exists(bsg_path)) {
    bsg <- answer_json_to_df(bsg_path)
    assign("bsg", bsg, envir = envir)

    message("bsg.csv updated")
  }
}

# Check mov file modifications
mov_change <- TRUE
if (!is.na(user_files_info[paste0(user_path, "mov.csv"), "mtime"])) {
  mov_change <- user_files_info[paste0(user_path, "mov.csv"), "mtime"] <
    max(user_files_info[grepl("mov_.*json$", row.names(user_files_info)), "mtime"])
}

if (mov_change){
  # Process mov files
  mov_files <- list.files(user_path, pattern = "mov_.*json$")
  if (length(mov_files) > 0) {
    mov <- do.call(rbind, lapply(mov_files, function(f) {
      answer_json_to_df(paste0(user_path, f))
    }))
    assign("mov", mov, envir = envir)

    message("mov.csv updated")
  }
}

# Load all user CSV files
for(i in user_files[grepl(".csv$", user_files)]){
  user_file_name<-gsub(".*/", "", gsub(".csv", "", i))
  assign(user_file_name, 
         read.csv(i,
                  sep = ";",
                  stringsAsFactors = TRUE,
                  na.strings = c(NA, "NA", ""))
         )
}

#' 
#' #### Process bsg
#' 
## ----------------------------------------------------------------------------------------------------------------
# Add current time if date is missing
if (length(bsg$date) == 0) {
  bsg$date <- Sys.time()
}

# Convert dates to Unix timestamp
if (!is.numeric(bsg$date)) {
  bsg$date <- as.numeric(as.POSIXct(bsg$date, 
                                    tryFormats = c("%d/%m/%Y","%Y-%m-%d")))
}

# Get latest survey
bsg_index <- bsg$date == max(as.numeric(bsg$date))
bsg_index <- cumsum(bsg_index) == max(cumsum(bsg_index))
bsg <- bsg[bsg_index,]

# Clean and update survey
bsg <- tryCatch(form_backward(bsg), 
                error = function(msg){
                    message(paste("Error in bsg form backwards compatibility:", msg))
                    return(bsg)})
model_id<-gsub("bsg_","",bsg$survey_id)
bsg$survey_id<-NULL
bsg$scenario_id <- "0"

message("bsg ", bsg$version, ": ", bsg$farm_id, " (", 
        as.POSIXct(bsg$date, origin = "1960-01-01"), ")")

#' 
#' #### Process mov
#' 
## ----------------------------------------------------------------------------------------------------------------
# Process movement survey data if it exists
if (exists("mov")) {
   # Handle dates
  if (length(mov$date) == 0) mov$date <- Sys.time()
  
  if (!is.numeric(mov$date)) {
    mov$date <- as.numeric(as.POSIXct(mov$date, 
                                      tryFormats = c("%d/%m/%Y","%Y-%m-%d")))
  }
  
  
  # Convert movement dates to days
  mov$mov_in_date <- as.numeric(as.POSIXct(mov$mov_in_date, 
                                                    tryFormats = c("%d/%m/%Y","%Y-%m-%d")))/86400
  mov$mov_out_date <- as.numeric(as.POSIXct(mov$mov_out_date,
                                               tryFormats = c("%d/%m/%Y","%Y-%m-%d")))/86400

  # Filter by risk days
  # Filter by risk days
  last_date <-ifelse(all(grepl("recurrent",mov$mov_recurrent)),mov$date,
                     max(c(mov$mov_in_date, mov$mov_out_date), na.rm = TRUE))
  
  
  first_date <- last_date - risk_days
  
  
  mov_filter <- (mov$mov_in_date > first_date) | grepl("recurrent",mov$mov_recurrent)
  
  mov$mov_out_date <- ifelse(mov$mov_out_date < first_date, 
                                  first_date, mov$mov_out_date)
  
  mov_filter <- ifelse(is.na(mov_filter), FALSE, mov_filter)
  
  # Display filtered movements
  if (any(!mov_filter)) {
    message("Some movements are prior to the risk days to calculate (", risk_days,")")
    message("The following movements are not included in the 
            simulation: ",
            paste(mov[!mov_filter,c("mov_id")], collapse = ", "))
  }
  
  # Update movement data
  mov <- mov[mov_filter,]
  mov <- mov[!duplicated(mov$mov_id, fromLast = TRUE),]
  mov <- tryCatch(form_backward(mov), 
                error = function(msg){
                    message(paste("Error in mov form backwards compatibility:", msg))
                    return(mov)})
  mov$survey_id <- NULL
  mov$scenario_id <- "0"
  
  # Simplify IDs
  mov <- mov %>%
    mutate(
      pasture_id = mov_pasture_id,
      first_veh_id = coalesce(oneway_veh_id, round_to_veh_id),
      second_veh_id = coalesce(round_from_veh_id)
    ) %>%
    select(-mov_pasture_id, -oneway_veh_id, -round_to_veh_id, -round_from_veh_id)
  

  message("mov ", paste(unique(mov$version), collapse=", "), " (",
          nrow(mov), "): ", paste(mov$mov_id, collapse=", "))
}

#' 
#' ### Admin
#' 
#' #### Load files
#' 
## ----------------------------------------------------------------------------------------------------------------
admin_path <- paste0(input_path, "admin/")

admin_files_global <- list.files(path = admin_path, pattern = "*.csv", recursive = FALSE, full.names = TRUE)

admin_model_path <- paste0(admin_path,model_id,"/")

# Find all admin input files for the current model
admin_files <- list.files(path = admin_model_path, pattern = "*.csv", recursive = TRUE, full.names = TRUE)

admin_files <- c(admin_files_global, admin_files)

# Load all admin CSV files
for(i in admin_files[grepl(".csv$", admin_files)]){
  user_file_name<-gsub(".*/", "", gsub(".csv", "", i))
  assign(user_file_name, 
         read.csv(i,
                  sep = ";",
                  stringsAsFactors = TRUE,
                  na.strings = c(NA, "NA", ""))
         )
}

#' 
#' #### Source admin R scripts
#' 
## ----------------------------------------------------------------------------------------------------------------
# Get list of R script files in the input admin model directory
admin_r_files <- list.files(
  path = admin_model_path,
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

# Source each R script file
invisible(
  lapply(admin_r_files, function(script) {
    tryCatch({
      source(script)
    }, error = function(e) {
      warning(sprintf("Error sourcing %s: %s", script, e$message))
    })
  })
)

#' 
#' ## Set mcmodule
#' 
## ----------------------------------------------------------------------------------------------------------------
suppressMessages({
  set_data_keys(get(paste0(model_id,"_data_keys")))
  set_mctable(get(paste0(model_id,"_mctable")))
})

message("RUNNING ", toupper(model_id), " FARMRISK")


if(exists("unify_info")){
  message(unify_info)
  source(paste0(model_id,".R"))
}

