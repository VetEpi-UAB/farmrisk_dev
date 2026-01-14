#' ---
#' title: "set_up_v3"
#' format: html
#' ---
#' 
#' ## Default settings
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
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
## ------------------------------------------------------------------------------------------------------------------------------------
packages <- c(
  "mc2d", "mcmodule", "dplyr", "tidyr", "ggplot2",
  "jsonlite", "RMySQL", "DBI", "lubridate"
)

# Install any missing packages
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, dependencies = TRUE)
}

# Load packages quietly
suppressPackageStartupMessages({
  invisible(lapply(packages, function(p) {
    library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }))
})

#' 
#' ## Load custom functions
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
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
## ------------------------------------------------------------------------------------------------------------------------------------
# Get arguments from the command line
args = commandArgs(trailingOnly = TRUE)

# Check if there is at least one argument; if not, use a manual farm_id
if (!length(args) == 0) {
    farm_id<-args[1]
}

#' 
#' ## Display settings
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
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
#' ## Connect to database
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
suppressWarnings({
  # Connect to database
  creds <- read_db_credentials()
  con <- connect_farmrisk_db(creds)
  data <- get_farm_data(con, farm_id)
  if(dim(data$farm)[1]==0){
    stop("No bsg data found with farm_id: ", farm_id)
  }
  message("Data retrieved from ", creds["hostname"], " '",creds["db_name"], "' database:")
  message("- farm (",paste(dim(data$farm), collapse=", "),")")
  message("- movements (",paste(dim(data$movements), collapse=", "),")")
  # Close connection
  dbDisconnect(con)
})

#' 
#' ## Input files
#' 
#' ### User
#' 
#' #### Load files
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
# Parse farm biosecurity survey to dataframe
bsg<-parse_jsurvey(data$farm$survey_result)

# Parse farm movements survey to dataframe
if (nrow(data$movements) > 0) {
    mov <- do.call(
      rbind,
      lapply(data$movements$survey_result, 
             function(x) {
               parse_jsurvey(x)
               }))
}

#' 
#' #### Process bsg
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
# Clean and update survey
bsg$farm_id<-farm_id
bsg <- form_backward(bsg)
model_id<-gsub("bsg_","",bsg$survey_id)
bsg$updated_at<-data$farm$survey_result_updated_at
bsg$survey_id<-NULL
bsg$scenario_id <- "0"

message("bsg_",model_id, "_", bsg$original_version, ": ", bsg$farm_id, " (", 
        bsg$updated_at,")")

#' 
#' #### Process mov
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
# Process movement survey data if it exists
if (exists("mov")) {
  mov$mov_id <- data$movements$mov_id
  mov <- form_backward(mov)
  mov$survey_id <- NULL
  mov$scenario_id <- "0"

  # Keep movements in the last year (by mov_in_date).
  # Also keep movements with NA mov_in_date if recurrent_ongoing is TRUE.
  one_year_ago <- as.Date(Sys.Date() - lubridate::years(1))

  mov <- mov %>%
    mutate(
      mov_in_date = as.Date(mov_in_date)
    ) %>%
    filter(
      ( !is.na(mov_in_date) & mov_in_date >= one_year_ago ) |
      ( is.na(mov_in_date) & coalesce(recurrent_ongoing, FALSE) )
    ) %>%
    mutate(
      farm_id = farm_id,
      pasture_id = mov_pasture_id,
      first_veh_id = coalesce(oneway_veh_id, round_to_veh_id),
      second_veh_id = coalesce(round_from_veh_id)
    ) %>%
    select(-mov_pasture_id, -oneway_veh_id, -round_to_veh_id, -round_from_veh_id)

  message("mov_", model_id, "_", paste(unique(mov$original_version), collapse = ", "), " (",
          nrow(mov), "): ", paste(mov$mov_id, collapse = ", "))
}

#' 
#' ### Admin
#' 
#' #### Load files
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
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
## ------------------------------------------------------------------------------------------------------------------------------------
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
## ------------------------------------------------------------------------------------------------------------------------------------
suppressMessages({
  set_data_keys(get(paste0(model_id,"_data_keys")))
  set_mctable(get(paste0(model_id,"_mctable")))
})

message("RUNNING ", toupper(model_id), " FARMRISK")


if(exists("unify_info")){
  message(unify_info)
  source(paste0(model_id,".R"))
}

