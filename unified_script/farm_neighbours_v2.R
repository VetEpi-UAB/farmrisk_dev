#' ---
#' title: "Farm surroundings introduction pathway"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 

#' 
## ------------------------------------------------------------------------------------------------------------------------------------
message("\nNEIGHBOURS PATHWAY: ")

#' 
#' ## Neighbour farms module
#' 
#' ### Prepare data
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
neighbour_data<-get_region_code(bsg, pathway = "farm")
neighbour_data<-neighbour_data%>%
  left_join(tidy_prefix(bsg, "neighbour", rm_prefix = FALSE))%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  left_join(filter(pathogen_status, health_status == "unk"))

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Do not allow direct contact with neighbour farms
  neighbour_data<-wif_no_dc(neighbour_data, scenario="Do not allow direct contact with neighbour farms")
  
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
neighbour_exp<-c(neighbour_herd_select = herd_select_exp,
                 neighbour_link = neighbour_link_exp,
                 neighbour_dir_contact = dir_contact_exp,
                 neighbour_area = area_exp)

neighbour<-mcmodule::eval_module(
  exp = neighbour_exp,
  data = neighbour_data)

neighbour<-mcmodule::at_least_one(
  mcmodule = neighbour,
  mc_names = c("dir_contact","dir_contact_pi"))

neighbour<-mcmodule::at_least_one(
  mcmodule = neighbour,
  mc_names = c("dir_contact_all","area_inf"),
  name = "neighbour_inf")

neighbour<-mcmodule::agg_totals(
  mcmodule=neighbour,
  mc_name=c("dir_contact_all"),
  agg_keys = c("pathogen", "scenario_id", "farm_id"))

neighbour<-mcmodule::agg_totals(
  mcmodule=neighbour,
  mc_name=c("neighbour_inf"),
  agg_keys = c("pathogen", "scenario_id", "farm_id"))

neighbour<-mcmodule::add_prefix(neighbour)

print_pathway_risk(neighbour, "neighbour_inf_agg")
message("\nNEIGHBOURS PATHWAY OK!")

