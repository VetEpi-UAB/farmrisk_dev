farmrisk <- function(
  farms,
  mcnode_plot = NULL,
  hg_csv = FALSE,
  replace_csv = FALSE,
  append_csv = TRUE,
  user_id = "dev",
  risk_months = 12,
  save_model = FALSE,
  calc_summary = FALSE,
  admin_wif = TRUE,
  model_analysis = FALSE,
  alternative_params = NULL,
  alternative_formula = NULL
) {
  # Set options for the simulation
  farmrisk_set <- TRUE
  message(
    "\nfarmrisk_set: \n - calc_summary ",
    calc_summary,
    "\n - admin_wif: ",
    admin_wif,
    "\n - model_analysis: ",
    model_analysis,
    "\n - alternative_params: ",
    alternative_params,
    "\n - alternative_formula: ",
    deparse(alternative_formula)
  )

  # Default settings
  defaults_set <- list(
    user_id = NULL,
    farm_id = NULL,
    risk_days = 360,
    calc_summary = FALSE,
    admin_wif = TRUE,
    model_analysis = FALSE,
    alternative_params = NULL,
    alternative_formula = NULL
  )
  envir <- parent.frame()
  # Assign sefault settings
  for (i in 1:length(defaults_set)) {
    if (
      !exists(names(defaults_set[i])) || is.null(get(names(defaults_set[i])))
    ) {
      assign(names(defaults_set[i]), defaults_set[[i]], envir = envir)
      assign(names(defaults_set[i]), defaults_set[[i]])
    } else {
      assign(names(defaults_set[i]), get(names(defaults_set[i])), envir = envir)
    }
  }

  #Load custom functions
  source("custom_functions.R")

  #Unify last script version
  unify_cattle()

  if (replace_csv & file.exists("output_files/hg_df.csv")) {
    file.remove("output_files/hg_df.csv")
  }

  for (i in 1:length(farms)) {
    farm_id <- farms[i]
    write.table(
      data.frame(farm_id, risk_months),
      file = "input_files/user/id/dev.csv",
      col.names = FALSE,
      row.names = FALSE,
      sep = ";"
    )

    #Source in local environment
    source("cattle.R")

    #Print mcnode boxplots
    if (!is.null(mcnode_plot)) {
      for (j in 1:length(mcnode_plot)) {
        try(wif_boxplot(intro, mcnode_plot[j]))
      }
    }

    if (length(farms) == 1) {
      if (save_model) {
        save(intro, file = "output_files/intro")
        message("\nFull model saved in 'output_files/intro'\n")
      }
    } else {
      #remove all objects except functions and farmrisk arguments
      func_args <- deparse(args(farmrisk))
      func_args <- unlist(strsplit(func_args, ", "))
      func_args <- func_args[grepl(" .*=.*", func_args)]
      func_args <- gsub(" =.*", "", func_args)
      func_args <- gsub(" ", "", func_args)

      keep_envir <- c("farms", lsf.str(), func_args)

      rm(list = setdiff(ls(), keep_envir))
    }

    loaded_functions = TRUE
  }
}

#farms_BC<-c("2024_BC_1_mod","2024_BC_2_mod","2024_BC_3_mod","2024_BC_4_mod","2024_BC_5_mod")
#farms_BL<-c("bl_1", "BL_4","BL_3","BL_2")

#RUN THIS TO GET ALL VISUALIZATIONS TO EXPLORE IN POWER BI
#farmrisk(farms_BC, mcnode_plot=c("total_agg", "no_pasture_agg"), hg_csv=TRUE, replace_csv = TRUE)
#farmrisk(farms_BL, mcnode_plot=c("total_agg", "no_pasture_agg"), hg_csv=TRUE, replace_csv = TRUE)

#farmrisk("2024_BC_4_mod", mcnode_plot=c("total_agg"), hg_csv=FALSE, save_model=TRUE)

farmrisk_report <- function(farms) {
  errors <- list()
  successes <- character(0)

  for (i in seq_along(farms)) {
    farm_id <- farms[i]
    init_time <- Sys.time()

    tryCatch(
      {
        # export html report
        quarto::quarto_render(
          input = "cattle_report.qmd",
          execute_params = list(farm_id = farm_id),
          output_file = paste0(farm_id, "_report.html")
        )

        # move to reports folder
        file.rename(
          from = paste0(farm_id, "_report.html"),
          to = paste0("reports/", farm_id, "_report_v2.html")
        )

        finish_time <- Sys.time()
        run_time <- finish_time - init_time
        cat("\nRun time ", farm_id, ": ", run_time, "\n", sep = "")
        successes <- c(successes, farm_id)
        invisible(TRUE)
      },
      error = function(e) {
        errors[[farm_id]] <<- conditionMessage(e)
        cat(
          "\nError running farm ",
          farm_id,
          ": ",
          conditionMessage(e),
          "\n",
          sep = ""
        )
        invisible(NULL)
      }
    )
  }

  if (length(errors) > 0) {
    farm_list <- paste(names(errors), collapse = ", ")
    cat(
      "\nAn error occurred running the following farms: ",
      farm_list,
      "\n",
      sep = ""
    )
    for (nm in names(errors)) {
      cat("- Farm ", nm, ": ", errors[[nm]], "\n", sep = "")
    }
  }

  invisible(list(successful = successes, errors = errors))
}
#farmrisk_report(c("eco_sat_mod","eco_montells","eco_petras"))
#farmrisk_report(c("bc6_v2", "bc9_v2", "bc11_v2", "bc10_v2"))

#farmrisk_report(c("bl1_v3_mod"))
#farmrisk_report(c("bl2_v2_mod","bl3_v3","bl4_v3","bl5_v2_mod"))
#farmrisk_report(c("bl1_v3_mod","bl2_v2_mod","bl3_v3","bl4_v3","bl5_v2_mod"))
#farmrisk_report(c("2024_BC_1_mod","2024_BC_2_mod","2024_BC_3_mod","2024_BC_4_mod","2024_BC_5_mod","bl_1","BL_2","BL_3", "BL_4","BL5"))
