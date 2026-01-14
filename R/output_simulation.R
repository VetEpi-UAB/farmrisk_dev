#' Save Monte Carlo Module Summary
#'
#' Saves summary tables from Monte Carlo module nodes to CSV files
#'
#' @param mcmodule List containing Monte Carlo module data
#' @param mcnodes Character vector of node names to process
#' @return None (saves files to disk)
save_mc_summary <- function(mcmodule, mcnodes) {
  # Get nodes with summaries
  summary_yes <- !sapply(sapply(mcmodule$node_list, "[[", "summary"), is.null)
  outputs <- mcnodes[mcnodes %in% names(summary_yes[summary_yes])]
  no_outputs <- mcnodes[!mcnodes %in% names(summary_yes[summary_yes])]

  # Warning for nodes without summaries
  if (length(no_outputs) > 0) {
    warning("No summary found in ", paste(no_outputs, collapse = ", "))
  }

  # Create output directory if needed
  output_folder_id <- file.path(output_path, farm_id)
  dir.create(output_folder_id, showWarnings = FALSE, recursive = TRUE)

  # Save summaries to CSV
  for (output in outputs) {
    file_path <- file.path(output_folder_id, paste0(output, ".csv"))
    write.table(
      mcmodule$node_list[[output]][["summary"]],
      file = file_path,
      row.names = FALSE,
      sep = ";"
    )
  }
}


#' Save Table Summary
#'
#' Saves data tables to CSV files
#'
#' @param tables Named list of data frames to save
#' @return None (saves files to disk)
save_table_summary <- function(tables) {
  # Create output directory if needed
  output_folder_id <- file.path(output_path, farm_id)
  dir.create(output_folder_id, showWarnings = FALSE, recursive = TRUE)

  # Save tables to CSV using names from the list
  table_names <- names(tables)
  if (is.null(table_names)) {
    stop("Tables must be provided as a named list")
  }

  for (name in table_names) {
    file_path <- file.path(output_folder_id, paste0(name, ".csv"))
    write.table(tables[[name]], file = file_path, row.names = FALSE, sep = ";")
  }
}

#' Export results as JSON
#'
#' Build a compact JSON summary of the simulation (simulation_info + results)
#' and write it to disk. Optionally the JSON can also be printed to stdout so it
#' can be captured when the script is run from the command line.
#'
#' @param output_dir Character. Directory to write the JSON file to. Defaults to the project `output_path` if available.
#' @param farm_id Character. Farm identifier used in the output filename. Defaults to the project `farm_id` if available.
#' @param intro List or NULL. The combined mcmodule object (intro) created by the model run.
#' @param outputs Character or NULL. Named vector of output node identifiers (as used in `intro$node_list`).
#' @param wif_median Data frame or NULL. Table with median values per pathogen and scenario (preferred source for summaries).
#' @param summary_median Data frame or NULL. Optional summary table produced by the outputs pipeline.
#' @param bsg List or NULL. Cleaned biosecurity survey object (used to fill minimal farm metadata).
#' @param model_id Character. Model identifier (e.g. "cattle").
#' @param script_date POSIXct/character. Date/time of script unification or generation.
#' @param repo Character. Repository URL.
#' @param last_commit Character. Git commit SHA of the code used to run the model.
#' @param commit_date POSIXct/character. Commit date (epoch or POSIXct) for the last commit.
#' @param risk_days Integer. Risk timeframe in days.
#' @param write_stdout Logical. If TRUE, print the JSON to stdout as well as saving to disk.
#'
#' @return Invisibly returns the prepared R list that was serialized to JSON.
#' @examples
#' \dontrun{
#' export_results_json(output_dir = "output_files", farm_id = "farm_123", write_stdout = TRUE)
#' }
#' @export
export_results_json <- function(
  output_dir = if (exists("output_path")) output_path else "output_files",
  farm_id = if (exists("farm_id")) farm_id else "unknown",
  intro = if (exists("intro")) intro else NULL,
  outputs = if (exists("outputs")) outputs else NULL,
  wif_median = if (exists("wif_median")) wif_median else NULL,
  summary_median = if (exists("summary_median")) summary_median else NULL,
  bsg = if (exists("bsg")) bsg else NULL,
  model_id = if (exists("model_id")) model_id else NA_character_,
  script_date = if (exists("script_date")) script_date else Sys.time(),
  repo = if (exists("repo")) repo else NA_character_,
  last_commit = if (exists("last_commit")) last_commit else NA_character_,
  commit_date = if (exists("commit_date")) commit_date else NA_character_,
  risk_days = if (exists("risk_days")) risk_days else NA_integer_,
  write_stdout = FALSE
) {
  requireNamespace("jsonlite")
  requireNamespace("dplyr")

  # helper to find a column among common candidate names
  pick_col <- function(df, candidates) {
    if (is.null(df)) {
      return(NULL)
    }
    cols <- intersect(names(df), candidates)
    if (length(cols) == 0) {
      return(NULL)
    }
    cols[[1]]
  }

  # get numeric stats for a given metric (median, lower_cl, upper_cl) using intro$node_list if possible,
  # otherwise fall back to wif_median (median only).
  get_node_stats <- function(metric_name, pathogen) {
    # try intro + outputs mapping first
    if (
      !is.null(intro) && !is.null(outputs) && metric_name %in% names(outputs)
    ) {
      node_name <- outputs[[metric_name]]
      if (
        !is.null(intro$node_list[[node_name]]) &&
          !is.null(intro$node_list[[node_name]][["summary"]])
      ) {
        s <- intro$node_list[[node_name]][["summary"]]
        s_row <- s %>%
          dplyr::filter(
            pathogen == pathogen & scenario_id %in% c("0", "Current")
          )
        if (nrow(s_row) == 0) {
          s_row <- s %>% dplyr::filter(pathogen == pathogen)
        }
        if (nrow(s_row) >= 1) {
          # median candidates
          med_col <- pick_col(s_row, c("median", "50%", "50"))
          low_col <- pick_col(
            s_row,
            c("2.5%", "2.5", "lower", "lower_cl", "quantile_2.5")
          )
          up_col <- pick_col(
            s_row,
            c("97.5%", "97.5", "upper", "upper_cl", "quantile_97.5")
          )
          median <- if (!is.null(med_col)) {
            as.numeric(s_row[[med_col]][1])
          } else {
            NA_real_
          }
          lower_cl <- if (!is.null(low_col)) {
            as.numeric(s_row[[low_col]][1])
          } else {
            NA_real_
          }
          upper_cl <- if (!is.null(up_col)) {
            as.numeric(s_row[[up_col]][1])
          } else {
            NA_real_
          }
          return(list(
            median = median,
            lower_cl = lower_cl,
            upper_cl = upper_cl
          ))
        }
      }
    }

    # fallback to wif_median (only median available)
    if (!is.null(wif_median) && metric_name %in% names(wif_median)) {
      row <- wif_median %>%
        dplyr::filter(pathogen == pathogen & scenario_id %in% c("0", "Current"))
      if (nrow(row) == 0) {
        row <- wif_median %>% dplyr::filter(pathogen == pathogen)
      }
      if (nrow(row) >= 1) {
        median <- as.numeric(row[[metric_name]][1])
        return(list(median = median, lower_cl = NA_real_, upper_cl = NA_real_))
      }
    }

    # no data
    list(median = NA_real_, lower_cl = NA_real_, upper_cl = NA_real_)
  }

  # Helper to list available pathogens
  pathogens <- character(0)
  if (!is.null(wif_median) && "pathogen" %in% names(wif_median)) {
    pathogens <- unique(wif_median$pathogen)
  } else if (
    !is.null(intro) && !is.null(outputs) && !is.null(outputs["total"])
  ) {
    s <- intro$node_list[[outputs["total"]]][["summary"]]
    if (!is.null(s) && "pathogen" %in% names(s)) pathogens <- unique(s$pathogen)
  }
  if (
    length(pathogens) == 0 &&
      !is.null(summary_median) &&
      "Pathogen" %in% names(summary_median)
  ) {
    pathogens <- unique(summary_median$Pathogen)
  }

  # Build pathogen entry using node stats
  build_pathogen_entry <- function(p) {
    # metrics we care about (should match outputs keys used in model)
    metrics <- c(
      mov_livestock = "mov_livestock",
      mov_transport = "mov_transport",
      visit_total = "visit_total",
      neighbour_total = "neighbour_total",
      total_wildlife = "total_wildlife",
      total = "total"
    )

    stats <- list()
    for (m in names(metrics)) {
      stats[[m]] <- get_node_stats(metrics[[m]], p)
    }

    # compute pathway shares using medians if possible (fractions, not percent)
    total_median <- stats[["total"]]$median
    share <- function(mname) {
      v <- stats[[mname]]$median
      if (!is.na(v) && !is.na(total_median) && total_median > 0) {
        return(v / total_median)
      }
      if (!is.na(v)) {
        return(v)
      }
      NA_real_
    }

    shares <- list(
      visit_people = share("visit_total"),
      visit_veh = share("mov_transport"),
      neighbour_total = share("neighbour_total")
    )

    # risk_reduction_plot: try to assemble per-scenario relative numbers (numeric)
    rr <- list()
    # prefer wif_median if it contains multiple scenarios
    if (!is.null(wif_median) && "scenario_id" %in% names(wif_median)) {
      sc <- wif_median %>% dplyr::filter(pathogen == p)
      if (nrow(sc) > 0) {
        # if relative column exists use it
        if ("relative" %in% names(sc)) {
          for (i in seq_len(nrow(sc))) {
            rr[[as.character(sc$scenario_id[i])]] <- as.numeric(sc$relative[i])
          }
        } else if ("total" %in% names(sc) || "total_wif" %in% names(sc)) {
          cols <- intersect(names(sc), c("total", "total_wif"))
          cur <- sc %>% dplyr::filter(scenario_id %in% c("0", "Current"))
          curval <- if (nrow(cur) >= 1 && length(cols) >= 1) {
            as.numeric(cur[[cols[1]]][1])
          } else {
            NA_real_
          }
          for (i in seq_len(nrow(sc))) {
            sv <- if (length(cols) >= 1) {
              as.numeric(sc[[cols[1]]][i])
            } else {
              NA_real_
            }
            rel <- if (!is.na(curval) && !is.na(sv) && curval > 0) {
              sv / curval
            } else {
              NA_real_
            }
            rr[[as.character(sc$scenario_id[i])]] <- rel
          }
        }
      }
    } else if (
      !is.null(intro) && !is.null(outputs) && "total" %in% names(outputs)
    ) {
      # try intro node_list for multiple scenarios
      node_summary <- intro$node_list[[outputs[["total"]]]][["summary"]]
      if (!is.null(node_summary) && "scenario_id" %in% names(node_summary)) {
        sc <- node_summary %>% dplyr::filter(pathogen == p)
        if (nrow(sc) > 0) {
          # attempt to use median/total_wif as numeric
          med_col <- pick_col(
            sc,
            c("median", "50%", "50", "total", "total_wif")
          )
          cur <- sc %>% dplyr::filter(scenario_id %in% c("0", "Current"))
          curval <- if (nrow(cur) >= 1 && !is.null(med_col)) {
            as.numeric(cur[[med_col]][1])
          } else {
            NA_real_
          }
          for (i in seq_len(nrow(sc))) {
            sv <- if (!is.null(med_col)) {
              as.numeric(sc[[med_col]][i])
            } else {
              NA_real_
            }
            rel <- if (!is.na(curval) && !is.na(sv) && curval > 0) {
              sv / curval
            } else {
              NA_real_
            }
            rr[[as.character(sc$scenario_id[i])]] <- rel
          }
        }
      }
    }

    # pathway_text now contains numeric stats (median, lower_cl, upper_cl) for total and each pathway
    pathway_text <- list(
      total = stats[["total"]],
      by_pathway = list(
        mov_livestock = stats[["mov_livestock"]],
        mov_transport = stats[["mov_transport"]],
        visit_total = stats[["visit_total"]],
        neighbour_total = stats[["neighbour_total"]],
        total_wildlife = stats[["total_wildlife"]]
      )
    )

    list(
      risk_reduction_plot = rr,
      pathway_plot = shares,
      pathway_text = pathway_text
    )
  }

  general <- list()
  if (length(pathogens) > 0) {
    for (i in seq_along(pathogens)) {
      general[[pathogens[i]]] <- build_pathogen_entry(pathogens[i])
    }
  }

  farm_info <- list(
    location = if (!is.null(bsg) && !is.null(bsg$region)) {
      bsg$region
    } else if (!is.null(bsg) && !is.null(bsg$farm_gid0)) {
      bsg$farm_gid0
    } else {
      NA_character_
    },
    farm_type = if (!is.null(bsg) && !is.null(bsg$farm_type)) {
      bsg$farm_type
    } else {
      NA_character_
    }
  )

  sim_info <- list(
    simulation_date = as.character(Sys.time()),
    model_id = model_id,
    script_unification_date = as.character(script_date),
    repository = repo,
    commit = last_commit,
    commit_date = if (!is.na(commit_date)) {
      as.character(as.POSIXct(commit_date, origin = "1970-01-01"))
    } else {
      NA_character_
    },
    bsg_version = if (!is.null(bsg) && !is.null(bsg$version)) {
      bsg$version
    } else {
      NA_character_
    },
    mov_version = NA_character_,
    user_id = if (exists("user_id")) user_id else NA_character_,
    farm_id = farm_id,
    risk_timeframe = risk_days
  )

  output_list <- list(
    simulation_info = sim_info,
    results = list(
      general = general,
      farm = farm_info
    )
  )

  # ensure output dir exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  out_file <- file.path(output_dir, paste0("output_", farm_id, ".json"))
  json <- jsonlite::toJSON(
    output_list,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
  writeLines(json, con = out_file)

  if (isTRUE(write_stdout)) {
    cat(json, "\n")
  }

  invisible(output_list)
}
