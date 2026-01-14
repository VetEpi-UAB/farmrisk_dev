#' ---
#' title: "Set-up"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 
#' ### Set-up for unified script
#' 
## ----------------------------------------------------------------------------------------
# Get arguments from the command line
args = commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument; if not, use a manual user_id
if (length(args) == 0) {
  user_id<-"dev"
} else {
  user_id <- args[1]
}

suppressPackageStartupMessages({
  # Load packages
  library(mc2d)      # Monte-Carlo simulations
  library(mcmodule)  # Modular Monte-Carlo
  library(dplyr)     # Data manipulation
  library(tidyr)     # Data cleaning
  
  # Load custom functions
  source("custom_functions.R") 
})

#Set up simulation
set_up_simulation(user_id)

