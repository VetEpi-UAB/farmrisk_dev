# Read credentials from file
read_db_credentials <- function(file_path = "db_keys.txt") {
  creds <- readLines(file_path)
  creds_list <- strsplit(creds, "=")
  setNames(
    sapply(creds_list, `[`, 2),
    sapply(creds_list, `[`, 1)
  )
}

# Connect to database
connect_farmrisk_db <- function(creds) {
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = creds["hostname"],
    port = as.integer(creds["port"]),
    dbname = creds["db_name"],
    user = creds["user"],
    password = creds["password"]
  )
}

# Get farm biosecurity survey result and movements
# Note: start_date parameter present for compatibility but not used in the current query
get_farm_data <- function(
  con,
  farm_id,
  start_date = Sys.Date() - lubridate::years(1)
) {
  # Query farm biosecurity survey_result
  farm_query <- sprintf(
    "SELECT farm_id, survey_result, survey_result_updated_at
     FROM farm
     WHERE farm_id = '%s' AND active = 1;",
    farm_id
  )

  farm_data <- DBI::dbGetQuery(con, farm_query)

  # Query movements joined to farm (no year filtering)
  movement_query <- sprintf(
    "SELECT mov.mov_id, mov.survey_result, mov.mov_type, mov.creation_time, mov.survey_result_updated_at
     FROM movement mov
     INNER JOIN farm ON farm.id = mov.farm_id AND farm.farm_id = '%s'
     ORDER BY mov.survey_result_updated_at DESC;",
    farm_id
  )

  movement_data <- DBI::dbGetQuery(con, movement_query)

  return(list(
    farm = farm_data,
    movements = movement_data
  ))
}
