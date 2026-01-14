#' Calculate risk of disease entry in cattle farms
#' 
#' @description
#' This function calculates the probability of disease entry through different pathways in cattle farms
#' 
#' @param farm_id Character. Unique identifier for the farm
#' @param risk_days Numeric. Time period (in days) for risk assessment calculation. Default is 360
#' @param calc_summary Logical. Whether to calculate summary for all nodes. Default: TRUE
#' @param admin_wif Logical. Whether to run all admin what-if scenarios. Default: TRUE
#' @param model_analysis Logical. Whether to run model analysis. Default: FALSE
#' @param alternative_params List. Optional alternative parameters for simulation
#' @param alternative_formula Formula. Optional formula for parameter modifications
#' @param inputs_path Character. Path to the directory containing input files. Default is "input_files"
#' @param outputs_path Character. Path to the directory where output files will be saved. Default is "output_files"
#' 
#' @details
#' The function evaluates multiple disease entry pathways including:
#' * Purchase of animals
#' * Pasture movements
#' * Visits
#' * Neighboring farms
#' * Wildlife contact
#' 
#' For each pathway, the function calculates:
#' * Probability of disease entry
#' * Relative risk compared to baseline
#' * Pathway-specific risks
#' 
#' @return
#' Returns a mcmodule containing:
#' * Risk probability summaries
#' * Pathway-specific risk calculations
#' * Comparative risk assessments
#' * Detailed statistics for each disease pathway
#' 
#' @examples
#' \dontrun{
#' farm_risk <- farmrisk_cattle("FARM001", risk_days = 360)
#' }
#' 
#' @export

farmrisk_cattle<-function(farm_id=NULL,
                   risk_days=360,
                   calc_summary = TRUE,
                   admin_wif = TRUE,
                   model_analysis = FALSE,
                   alternative_params = NULL,
                   alternative_formula = NULL,
                   input_path=paste0(system.file("input_files/",package="farmrisk"),"/"),
                   output_path=paste0(system.file("output_files/",package="farmrisk"),"/"),
                   forms_path=paste0(system.file("forms/",package="farmrisk"),"/")){
  
  #Remove previous farm bsg or mov tables
  rm(list = c("bsg","mov"),envir=parent.frame())
