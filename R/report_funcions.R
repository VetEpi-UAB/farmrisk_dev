#' Create a ggplot Risk Value Visualization
#'
#' @param value Numeric. The risk value to visualize (between 0 and 1).
#' @param max_box Numeric. Optional. The maximum number of boxes to use.
#'
#' @return A ggplot object representing the risk value visualization.
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual theme_void theme
#'
#' @export
#'
#' @examples
#' # Basic usage
#' ggplot_risk_value(0.7)
#'
#' # With custom max_box
#' ggplot_risk_value(0.5, max_box = 16)
ggplot_risk_value <- function(value, max_box = NULL) {
  # Calculate number of icons and boxes
  exp10 <- ifelse(value < 0.1, -floor(log10(value)), 2)
  n_icons <- round(sqrt(10^exp10))^2
  n_boxes <- n_icons / 100

  if (!is.null(max_box)) {
    n_boxes <- max_box
    n_icons <- max_box * 100
  }

  # Adjust to pretty box numbers
  n <- 1:100
  pretty_boxes <- sort(unique(c(n * (n + 1), n^2)))
  n_boxes <- min(pretty_boxes[pretty_boxes - n_boxes >= 0])
  n_icons <- n_boxes * 100

  # Calculate icon coordinates
  n_row_box <- floor(sqrt(n_boxes))
  n_col_box <- ceiling(sqrt(n_boxes))

  row_box <- rep(0:(n_row_box - 1), each = (n_boxes / n_row_box) * 100)
  col_box <- rep(
    rep(0:(n_col_box - 1), each = 100),
    times = n_boxes / n_col_box
  )

  box_y_coord <- row_box * 11
  box_x_coord <- col_box * 11

  in_x_coord <- rep(0:9, each = 10, times = n_boxes)
  in_y_coord <- rep(0:9, times = n_icons / 10)

  y_coord <- in_y_coord + box_y_coord
  x_coord <- in_x_coord + box_x_coord

  # Create icon data
  icon <- c(
    rep("pos", times = ceiling(n_icons * value)),
    rep("neg", times = n_icons - ceiling(n_icons * value))
  )
  data <- data.frame(icon = icon, x = x_coord, y = y_coord)

  # Create ggplot
  icon_plot <- ggplot(data, aes(x = x, y = y, colour = icon)) +
    geom_point(size = 1000 / n_icons) +
    scale_color_manual(values = c(neg = "#BAD3D3", pos = "#ff1c5d")) +
    theme_void() +
    theme(legend.position = "none")

  return(icon_plot)
}

#' Format Monte Carlo Simulation Summary Results
#'
#' @description
#' Formats the results of Monte Carlo simulations, providing the median (50th percentile)
#' with a 95% confidence interval (2.5% - 97.5%).
#'
#' @param data A data frame containing Monte Carlo simulation summary results with columns
#'             named "50%", "2.5%", and "97.5%".
#'
#' @return A character string representing the formatted Monte Carlo results.
#'
#' @export
#'
#' @examples
#' mc_results <- data.frame("50%" = 0.75, "2.5%" = 0.70, "97.5%" = 0.80)
#' format_mc(mc_results)
#'
format_mc <- function(data) {
  if (is.data.frame(data) && all(c("50%", "2.5%", "97.5%") %in% names(data))) {
    paste0(
      "**",
      signif(data$`50%` * 100, 3),
      "%** (",
      signif(data$`2.5%` * 100, 3),
      "-",
      signif(data$`97.5%` * 100, 3),
      "%)"
    )
  } else if (is.numeric(data) && length(data) == 1) {
    paste0(signif(data * 100, 3), "% (-, -)")
  } else {
    NA
  }
}

#' Create a Donut Plot
#'
#' @param data A data frame containing columns 'name' and 'value'
#'
#' @return A ggplot object representing the donut plot
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   name = c("A", "B", "C"),
#'   value = c(10, 60, 30)
#' )
#' create_donut_plot(data)
ggplot_donut <- function(data) {
  # Compute percentages
  data$fraction <- data$value / sum(data$value)

  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)

  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n = -1))

  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2

  # Compute a good label
  data$label <- ifelse(data$fraction > 0.01, key_to_lit(data$name), "")
  #paste0(key_to_lit(data$name), "\n", Signif(data$value*100),"%"), "")

  # Make the plot
  ggplot(data, aes(fill = name, colour = name)) +
    geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
    geom_text_repel(
      aes(x = 4, y = labelPosition, label = label),
      size = 10,
      hjust = .5,
      nudge_x = 1,
      direction = "x",
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20,
      seed = 123,
      label.size = NA
    ) +
    coord_polar(theta = "y", clip = "off") +
    xlim(c(2, 5)) +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2") +
    theme(legend.position = "none") +
    theme_void() +
    guides(
      fill = "none",
      colour = "none"
    )
}

#' Safely Extract Data from Node List
#'
#' @param node_list List containing node data
#' @param mcname String, name of the node to extract
#' @param pathogen String, pathogen to filter by
#' @param scenario_id String, scenario ID to filter by
#'
#' @return A data frame with extracted data or default values if not found
#'
#' @importFrom dplyr filter
safe_extract <- function(node_list, mcname, pathogen, scenario_id) {
  if (mcname %in% names(node_list)) {
    data <- node_list[[mcname]]$summary %>%
      filter(pathogen == !!pathogen, scenario_id == !!scenario_id)
    if (nrow(data) > 0) return(data)
  } else {
    data <- data.frame(`50%` = 0, `2.5%` = 0, `97.5%` = 0)
    names(data) <- c("50%", "2.5%", "97.5%")
    return(data)
  }
}

#' Process Pathogen Data
#'
#' @param mcmodule List containing Monte Carlo simulation results
#' @param pathogen String, pathogen to process
#' @param scenario_id String, scenario ID to filter by (default "0")
#' @param outputs Vector, output names to process (default outputs_flex)
#'
#' @return List containing formatted results and pathways data
#'
#' @importFrom dplyr filter
agg_list <- function(
  mcmodule,
  pathogen,
  scenarios = NULL,
  results_names = outputs
) {
  if (is.null(scenarios)) {
    #All scenarios
    scenarios <- unique(unlist(lapply(mcmodule$data, "[[", "scenario_id")))
  }

  data_all <- list()

  for (i in 1:length(scenarios)) {
    scenario_id <- scenarios[[i]]
    # Extract and process data for each pathway
    results <- lapply(results_names, function(mcname) {
      safe_extract(mcmodule$node_list, mcname, pathogen, scenario_id)
    })

    #If is all null, skip to the next iteration
    if (all(sapply(results, is.null))) {
      next
    }

    if (
      !is.null(mcmodule$node_list$visit_indir_contact_all_veh) &&
        scenario_id %in%
          intro$node_list$visit_indir_contact_all_veh$summary$scenario_id
    ) {
      # Visit data by type
      results$visit_veh <- safe_extract(
        mcmodule$node_list,
        "visit_indir_contact_all_veh",
        pathogen,
        scenario_id
      )
      results$visit_people <- results$visit_veh[!results$visit_veh$visit_veh, ]
      results$visit_veh <- results$visit_veh[results$visit_veh$visit_veh, ]
    }

    # Remove empty results
    results <- results[!sapply(results, is.null)]
    results <- results[
      !unlist(sapply(results, function(x) all(is.na(x[["50%"]]))))
    ]
    results <- results[unlist(sapply(results, nrow)) > 0]

    # Remove if risk 0
    results <- results[unlist(sapply(results, function(x) min(x[["50%"]]))) > 0]

    # Sort by median
    results <- results[names(sort(
      unlist(sapply(results, function(x) max(x[["50%"]]))),
      decreasing = TRUE
    ))]

    # Format results
    formatted_results <- lapply(results, format_mc)

    formatted_median <- lapply(results, function(x) {
      paste0(signif(x[["50%"]] * 100, 3), "%")
    })

    # Define pathways
    pathway <- c(
      neighbour_total = "farm_neigbour",
      visit_people = "visit_people",
      visit_veh = "visit_veh",
      mov_livestock = "mov_livestock",
      mov_transport = "mov_transport",
      total_wildlife = "total_wildlife"
    )

    # Filter results and pathway
    results_pathways <- results[names(results) %in% names(pathway)]
    pathway <- pathway[names(pathway) %in% names(results_pathways)]

    # Prepare data for donut plot
    pathways_data <- data.frame(
      name = pathway[names(results_pathways)],
      value = sapply(results_pathways, function(x) x[["50%"]]),
      central = sapply(results_pathways, function(x) x[["50%"]]),
      lower = sapply(results_pathways, function(x) x[["2.5%"]]),
      upper = sapply(results_pathways, function(x) x[["97.5%"]]),
      sd = sapply(results_pathways, function(x) x[["sd"]])
    )

    data_i <- list(
      results = results,
      formatted_results = formatted_results,
      formatted_median = formatted_median,
      formatted_pathways = formatted_results[names(results_pathways)],
      pathways_data = pathways_data
    )

    data_all[[scenario_id]] <- data_i
  }
  return(data_all)
}

agg_by_list <- function(
  mcmodule,
  pathogen,
  scenarios = NULL,
  results_names = outputs_agg
) {
  if (is.null(scenarios)) {
    #All scenarios
    scenarios <- unique(unlist(lapply(mcmodule$data, "[[", "scenario_id")))
  }

  data_all <- list()

  for (i in 1:length(scenarios)) {
    scenario_id <- scenarios[[i]]

    # Extract and process data for each pathway
    results <- lapply(results_names, function(mcname) {
      safe_extract(mcmodule$node_list, mcname, pathogen, scenario_id)
    })

    #If is all null, skip to the next iteration
    if (all(sapply(results, is.null))) {
      next
    }

    if (!is.null(results$visit_veh)) {
      results$visit_veh$visit_veh <- ifelse(
        results$visit_veh$visit_veh,
        "visit_veh",
        "visit_people"
      )
    }

    # Remove empty results
    results <- results[!sapply(results, is.null)]
    results <- results[
      !unlist(sapply(results, function(x) all(is.na(x[["50%"]]))))
    ]
    results <- results[unlist(sapply(results, nrow)) > 0]

    # Order by median
    results <- results[order(
      sapply(results, function(x) max(x[["50%"]])),
      decreasing = TRUE
    )]

    formatted_results <- c()
    formatted_names <- c()
    formatted_median <- c()

    for (j in 1:length(results)) {
      results_j <- results[[j]]

      results_j <- results_j %>%
        arrange(desc(`50%`)) %>%
        #filter(!`50%`==0)%>%
        mutate(
          mc_name = NULL,
          farm_id = NULL,
          pathogen = NULL,
          scenario_id = NULL
        )

      keys_j <- names(results_j[!sapply(results_j, is.numeric)])

      formatted_results_j <- format_mc(results_j)

      formatted_median_j <- paste0(signif(results_j$`50%` * 100, 3), "%")

      formatted_names_j <- rep(names(results[j]), length(formatted_results_j))

      if (length(keys_j) > 1) {
        results_j <- unite(results_j, "key", all_of(keys_j), sep = "-")
        keys_j <- "key"
      }

      names(formatted_results_j) <- paste(as.character(results_j[[keys_j]]))

      names(formatted_median_j) <- paste(as.character(results_j[[keys_j]]))

      formatted_names <- c(formatted_names, formatted_names_j)

      formatted_results <- c(formatted_results, formatted_results_j)

      formatted_median <- c(formatted_median, formatted_median_j)
    }

    data_i <- list(
      results = results,
      formatted_results = formatted_results,
      formatted_median = formatted_median,
      formatted_pathways = formatted_results
    )

    data_all[[scenario_id]] <- data_i
  }
  return(data_all)
}


ggplot_biosecurity_bars <- function(pathogen_name) {
  wif_median_plot %>%
    filter(pathogen == pathogen_name, !scenario_id == "0", yesno == "no") %>%
    ggplot(aes(x = reorder(scenario_id, value), y = -value, fill = value)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    scale_fill_gradient(high = "#00CD00", low = "gray") +
    coord_flip() +
    scale_x_discrete(name = "", position = "top") +
    scale_y_continuous(
      name = "Risk reduction",
      breaks = seq(0, -1, by = -0.1),
      labels = paste0(seq(0, 1, by = 0.1) * 100, "%")
    ) +
    theme_minimal() +
    guides(fill = "none")
}

biosecurity_table <- function(pathogen_name) {
  biosecurity_reduction <- wif_median_plot %>%
    filter(pathogen == pathogen_name, yesno == "no")

  biosecurity_table <- wif_median %>%
    filter(pathogen == pathogen_name) %>%
    left_join(biosecurity_reduction, by = join_by(pathogen, scenario_id)) %>%
    arrange(desc(total_wif)) %>%
    transmute(
      `Scenario` = ifelse(scenario_id == 0, "Baseline", scenario_id),
      `Relative risk reduction` = ifelse(scenario_id == 0, NA, value),
      `Entry risk` = total_wif,
      mov_livestock,
      mov_transport,
      visit_total,
      neighbour_total,
      total_wildlife
    ) %>%
    mutate(across(where(is.numeric), ~ signif(., digits = 3))) %>%
    mutate(Scenario = ifelse(Scenario == 0, "Baseline", Scenario))

  names(biosecurity_table) <- key_to_lit(names(biosecurity_table))

  biosecurity_table[biosecurity_table == 0] <- NA

  return(biosecurity_table)
}
il_medians <- function(result, agg_by = FALSE) {
  if (!agg_by) {
    IBR <- ifelse(
      result %in% names(ibr_agg_list[["0"]]$formatted_median),
      ibr_agg_list[["0"]]$formatted_median[[result]],
      "-"
    )
    BVD <- ifelse(
      result %in% names(bvd_agg_list[["0"]]$formatted_median),
      bvd_agg_list[["0"]]$formatted_median[[result]],
      "-"
    )
    TB <- ifelse(
      result %in% names(tb_agg_list[["0"]]$formatted_median),
      tb_agg_list[["0"]]$formatted_median[[result]],
      "-"
    )
  } else {
    IBR <- ifelse(
      result %in% names(ibr_agg_by_list[["0"]]$formatted_median),
      ibr_agg_by_list[["0"]]$formatted_median[[result]],
      "-"
    )
    BVD <- ifelse(
      result %in% names(bvd_agg_by_list[["0"]]$formatted_median),
      bvd_agg_by_list[["0"]]$formatted_median[[result]],
      "-"
    )
    TB <- ifelse(
      result %in% names(tb_agg_by_list[["0"]]$formatted_median),
      tb_agg_by_list[["0"]]$formatted_median[[result]],
      "-"
    )
  }

  all_pathogens <- c(IBR = IBR, BVD = BVD, TB = TB)

  all_pathogens <- ifelse(
    length(all_pathogens) > 0 & !all(all_pathogens %in% "-"),
    paste0(names(all_pathogens), ": ", all_pathogens, collapse = ", "),
    "negligible risk"
  )

  return(all_pathogens)
}


get_numeric_values <- function(data, position = 1) {
  # Function to process a single string NOTE: ONLY ABSOLUTE NUMBERS
  process_string <- function(x) {
    if (is.na(x)) {
      return(as.numeric(NA))
    }
    # Extract all values from the formatted string
    values <- gsub("(?<=\\d)-(?=\\d)", "_", x, perl = TRUE)
    values <- strsplit(gsub("[^0-9.e-]", " ", values), "\\s+")[[1]]
    values <- values[values != ""]

    # Return the numeric value based on the position argument
    if (length(values) >= position) {
      return(as.numeric(values[position]) / 100) # Convert percentage to proportion
    } else {
      return(as.numeric(NA))
    }
  }

  # Check data type and process accordingly
  if (is.matrix(data) || is.data.frame(data)) {
    result <- apply(data, c(1, 2), process_string)
    if (is.data.frame(data)) {
      result <- as.data.frame(result)
    }
  } else if (is.vector(data)) {
    result <- sapply(data, process_string)
  } else if (is.character(data)) {
    result <- process_string(data)
  } else {
    stop("Input must be a matrix, data frame, vector, or single string")
  }

  return(result)
}


#From: https://stackoverflow.com/a/45858044

add_emtpy_cols <- function(data, cols) {
  add <- cols[!cols %in% names(data)]

  if (length(add) != 0) {
    data[add] <- NA
  }
  data
}


#' Adapted Version of Function 'signif'
#'
#' This function adapts base-function \code{\link{signif}}
#' by always returning integer values in case the number of
#' requested significant digits is less than the the number of
#' digits in front of the decimal separator.
#'
#' @param x			(numeric) value to be rounded to the desired number
#' 					of significant digits
#' @param digits	(integer) number of significant digits
#' @param force		(logical) TRUE = force the return value to have at least 4 significant
#' 					digits, i.e. to integers with less digits zeros will be appended after
#' 					the decimal separator, otherwise the return value will be casted from
#' 					character to numeric
#' @param ...		additional parameters
#'
#' @return 	number with 'digits' significant digits, if 'force=TRUE' "character" objects will be
#' 			returned otherwise objects of mode "numeric"
#'
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

Signif <- function(x, digits = 4, force = TRUE, ...) {
  call <- match.call()
  manyX <- call$manyX
  if (is.null(manyX)) {
    manyX <- FALSE
  }
  stopifnot(is.numeric(x))
  if (length(x) > 1) {
    return(sapply(x, Signif, digits = digits, manyX = TRUE))
  }

  if (!manyX && "coef.gnm" %in% class(x)) {
    # assign name to single gnm-coefficient
    x <- as.numeric(x)
    names(x) <- "beta1"
  }
  Ndbc <- nchar(substr(as.character(x), 1, regexpr("\\.", as.character(x)) - 1))
  x <- signif(x, ifelse(Ndbc > digits, Ndbc, digits))
  NcX <- nchar(x)
  comma <- grepl("\\.", x)
  if (comma) {
    NcX <- NcX - 1
  }
  if (NcX < digits) {
    x <- paste0(
      x,
      ifelse(comma, "", "."),
      paste(rep(0, digits - NcX), collapse = "")
    )
  }
  if (!force) {
    x <- as.numeric(x)
  }
  x
}

#' Print Pathway Risk Summary
#'
#' Prints a formatted message showing the current pathway risk summary for a specified
#' Monte Carlo module node, including pathogen names and their median (50th percentile) risk values
#' for the baseline scenario (scenario_id == "0").
#'
#' @param mcmodule A Monte Carlo module object containing node_list with summary data
#' @param mc_name Character string specifying the name of the node in the module's node_list
#'   to extract and display risk information from
#'
#' @return NULL (invisibly). The function is called for its side effect of printing
#'   a message to the console.
#'
#' @details
#' The function extracts risk summary data from the specified node and formats it as:
#' "Current [mc_name] -> pathogen1: value1, pathogen2: value2, ..."
#' where values are the 50th percentile (median) estimates rounded using Signif().
#'
#' @examples
#' # Print wildlife infection aggregation risk
#' print_pathway_risk(wildlife, "wildlife_inf_agg")
#'
#' @export

print_pathway_risk <- function(mcmodule, mc_name) {
  message("\nCurrent ", mc_name, " -> ",
          paste(
            mcmodule$node_list[[mc_name]]$summary$pathogen[mcmodule$node_list[[mc_name]]$summary$scenario_id=="0"],
            Signif(mcmodule$node_list[[mc_name]]$summary$`50%`[mcmodule$node_list[[mc_name]]$summary$scenario_id=="0"]),
            sep=": ", collapse =", "))
}
