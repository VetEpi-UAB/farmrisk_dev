migration_from <- "v4_4"
migration_to <- "v4.5.0"

migrate <- function(data) {
  # Normalise version string to the new form
  if (!"version" %in% names(data)) {
    data$version <- migration_to
  } else {
    old_vals <- c("v4_4", "v4.4", "v4.4.0")
    data$version <- ifelse(
      as.character(data$version) %in% old_vals,
      migration_to,
      as.character(data$version)
    )
  }

  # Finalise version
  data$version <- migration_to
  data
}
