#' Get form version as number
#' Accepted input examples:
#' - "v2_7"
#' - "v2.7.1"
#' - "bsg_form_v2_7.json"  (filename containing a version pattern)
#'
#' Numeric encoding: major*100 + minor + patch*0.001
#' (patch is optional)
#'
#' @param data Either a character vector of version strings / filenames or a data.frame with a 'version' column.
#' @return A numeric vector where each element is (major*100 + minor + patch*0.001) or NA_real_ if parsing fails.
#' @examples
#' form_version("v2_7")           # 207.000
#' form_version("v2.7.1")         # 207.001
#' form_version(data.frame(version = c("v1_0", "v2_3")))
form_version <- function(data) {
  # validate and extract text vector to parse
  if (is.data.frame(data)) {
    if (!"version" %in% names(data)) {
      stop("data frame must contain a 'version' column")
    }
    text <- as.character(data$version)
  } else if (is.character(data)) {
    text <- data
  } else {
    stop(
      "data must be a character vector or a data.frame with a 'version' column"
    )
  }

  # prepare output and record indices we couldn't parse
  result <- numeric(length(text))
  na_idx <- integer(0)

  for (i in seq_along(text)) {
    text_i <- text[i]

    # Regex accepts optional leading 'v', separators '.' or '_' and optional patch:
    # groups: 1 -> major, 2 -> minor, 3 -> patch (optional)
    matches <- regexec(
      "(?:v)?(\\d+)[_.](\\d+)(?:[_.](\\d+))?",
      text_i,
      perl = TRUE
    )
    extracted <- regmatches(text_i, matches)[[1]]

    if (length(extracted) >= 3) {
      major <- as.integer(extracted[2])
      minor <- as.integer(extracted[3])
      patch <- if (length(extracted) >= 4 && nzchar(extracted[4])) {
        as.integer(extracted[4])
      } else {
        0L
      }

      # guard against malformed numeric parse
      if (is.na(major) || is.na(minor) || is.na(patch)) {
        result[i] <- NA_real_
        na_idx <- c(na_idx, i)
        next
      }

      # enforce versioning limits to keep numeric encoding unambiguous
      if (minor >= 100L) {
        stop("Parsed minor version (", minor, ") >= 100. Minor must be 0..99.")
      }
      if (patch >= 1000L) {
        stop(
          "Parsed patch version (",
          patch,
          ") >= 1000. Patch must be 0..999."
        )
      }

      # numeric encoding: major*100 + minor + patch*0.001
      result[i] <- major * 100 + minor + patch * 0.001
    } else {
      # couldn't match expected pattern -> mark NA and continue
      result[i] <- NA_real_
      na_idx <- c(na_idx, i)
    }
  }

  if (length(na_idx) > 0) {
    warning(
      "Could not parse version for entries: ",
      paste0(na_idx, collapse = ", "),
      ". Expected forms like 'v2_7', 'v2.7.1' or filenames containing those patterns."
    )
  }

  result
}

# Migration runner: sources migration files from forms/migrations/ and
# applies sequential migrations from current_numeric_version -> target_numeric_version.
# Migration files must define:
#   migration_from <- "vX_Y"    # or vX.Y.Z
#   migration_to   <- "vA_B"
#   migrate <- function(data) { data$version <- migration_to; data }
run_form_migrations <- function(
  data,
  survey_id,
  current_numeric_version,
  target_numeric_version,
  migrations_path = "forms/migrations"
) {
  if (!dir.exists(migrations_path)) {
    stop("Migrations path does not exist: ", migrations_path)
  }
  if (is.na(current_numeric_version)) {
    stop("current_numeric_version is NA; cannot run migrations")
  }
  if (current_numeric_version >= target_numeric_version) {
    return(data)
  }

  files <- list.files(migrations_path, pattern = survey_id, full.names = TRUE)
  if (length(files) == 0) {
    stop("No migration files found for survey_id: ", survey_id)
  }

  # load metadata and migrate functions into isolated envs
  migrations <- lapply(files, function(fpath) {
    # isolate migration code but allow base primitives/functions to resolve
    env <- new.env(parent = baseenv())
    sys.source(fpath, envir = env)
    if (
      !exists("migration_from", envir = env, inherits = FALSE) ||
        !exists("migration_to", envir = env, inherits = FALSE) ||
        !exists("migrate", envir = env, inherits = FALSE)
    ) {
      stop(
        "Migration file ",
        basename(fpath),
        " must define migration_from, migration_to and migrate()"
      )
    }
    list(
      file = fpath,
      from = get("migration_from", envir = env),
      to = get("migration_to", envir = env),
      env = env,
      migrate_fn = get("migrate", envir = env)
    )
  })

  # helper to convert version string to numeric (single value expected)
  to_numeric <- function(x) {
    vn <- form_version(x)
    if (length(vn) != 1 || is.na(vn)) {
      stop("Could not parse version: ", as.character(x))
    }
    vn
  }

  # attach numeric fields
  for (i in seq_along(migrations)) {
    migrations[[i]]$from_num <- to_numeric(migrations[[i]]$from)
    migrations[[i]]$to_num <- to_numeric(migrations[[i]]$to)
  }

  tol <- 1e-9
  current <- current_numeric_version
  applied <- character(0)

  repeat {
    if (current >= target_numeric_version - tol) {
      break
    }

    # find migrations whose from matches current
    idx <- which(sapply(migrations, function(m) {
      abs(m$from_num - current) < tol
    }))
    if (length(idx) == 0) {
      stop(
        "Missing migration to advance from version ",
        format(current, digits = 12),
        " to reach target ",
        format(target_numeric_version, digits = 12)
      )
    }
    # choose candidate with smallest to_num > from_num
    cand <- migrations[idx]
    cand <- cand[order(sapply(cand, function(m) m$to_num))]
    sel <- cand[[1]]

    # apply migration function
    data <- sel$migrate_fn(data)

    # validate version was updated to expected migration_to
    new_version_num <- form_version(data$version)
    if (is.na(new_version_num) || abs(new_version_num - sel$to_num) > 1e-6) {
      stop(
        "Migration ",
        basename(sel$file),
        " did not set data$version to expected ",
        sel$to
      )
    }

    applied <- c(applied, basename(sel$file))
    current <- new_version_num
  }

  message("Applied migrations: ", paste0(applied, collapse = " -> "))
  data
}

# Main backward-compatibility function: iterates rows, checks versions,
# and uses run_form_migrations when a row is older than the latest form.
form_backward <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  if (!all(c("survey_id", "version") %in% names(data))) {
    stop("data must contain 'survey_id' and 'version' columns")
  }

  # Preserve original submitted version for each row so it can be printed later.
  # If the caller already provided original_version, keep it (coerce to char).
  if (!"original_version" %in% names(data)) {
    data$original_version <- as.character(data$version)
  } else {
    data$original_version <- as.character(data$original_version)
  }

  data_new <- data.frame()
  n <- nrow(data)
  tol <- 1e-9

  for (i in seq_len(n)) {
    data_i <- data[i, , drop = FALSE]

    # Ensure the single-row copy also has original_version set (safety)
    if (
      !"original_version" %in% names(data_i) || is.na(data_i$original_version)
    ) {
      data_i$original_version <- as.character(data_i$version)
    }

    survey_id <- as.character(data_i$survey_id)
    if (is.na(survey_id) || nzchar(survey_id) == FALSE) {
      stop("Missing survey_id in row ", i)
    }

    # find form files that match this survey_id (filenames expected in forms_path)
    version_list <- list.files(
      forms_path,
      pattern = paste0("^", survey_id, ".*\\.json$")
    )
    if (length(version_list) == 0) {
      stop("No form files found for survey ID: ", survey_id)
    }

    version_vals <- form_version(version_list)
    if (all(is.na(version_vals))) {
      stop("Could not parse any form versions for survey ID: ", survey_id)
    }

    idx_latest <- which.max(version_vals)
    last_version <- version_vals[idx_latest]
    if (is.na(last_version)) {
      stop(
        "Latest form version could not be determined for survey ID: ",
        survey_id
      )
    }
    last_version_name <- gsub(
      paste0(survey_id, "_form_|\\.json"),
      "",
      version_list[idx_latest]
    )

    # numeric version for this particular data row (may be NA if unparsable)
    data_i_version <- form_version(data_i)

    # if exact match to latest => accept
    if (!is.na(data_i_version) && abs(data_i_version - last_version) < tol) {
      data_new <- bind_rows(data_new, data_i)
      next
    }

    # if data older than latest but not too old, attempt migrations
    if (!is.na(data_i_version) && data_i_version < last_version) {
      data_i <- run_form_migrations(
        data = data_i,
        survey_id = survey_id,
        current_numeric_version = data_i_version,
        target_numeric_version = last_version,
        migrations_path = file.path("forms", "migrations")
      )
      # preserve original_version in case migration altered it accidentally
      if (is.null(data_i$original_version) || is.na(data_i$original_version)) {
        data_i$original_version <- as.character(data$version) # fallback: set to pre-migration value
      }
      data_i_version <- form_version(data_i)
      if (is.na(data_i_version) || abs(data_i_version - last_version) >= tol) {
        stop(
          "After migrations, row ",
          i,
          " did not reach latest version (",
          last_version_name,
          ")"
        )
      } else {
        message(survey_id, " (row ", i, ") migrated to ", last_version_name)
      }
    }

    data_new <- bind_rows(data_new, data_i)
  }

  return(data_new)
}
