#' ---
#' title: "Outdoors introduction pathway"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 

#' 
## -------------------------------------------------------------------------------------------------------------------
message("\nNEIGHBOURS PATHWAY: ")

#' 
#' ## Neighbour farms module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------
neighbour_data<-get_region_code(bsg, pathway = "farm")
neighbour_data<-neighbour_data%>%
  left_join(tidy_prefix(bsg, "neighbour", rm_prefix = FALSE))%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)

#' 
#' ### Admin what-if
#' 
## -------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Do not allow direct contact with neighbour farms
  neighbour_data<-wif_no_dc(neighbour_data, scenario="Do not allow direct contact with neighbour farms")
  
}

#' 
#' ### Evaluate
#' 
## -------------------------------------------------------------------------------------------------------------------
neighbour_expression<-c(neighbour_link = neighbour_link_expression,
                        neighbour_dir_contact=dir_contact_expression,
                        neighbour_area_inf=area_inf_expression)

neighbour<-eval_model_expression(model_expression = neighbour_expression,
                             data=neighbour_data)

neighbour<-at_least_one(mcmodule=neighbour, mcnodes=c("a_dir_contact","area_inf"), name="neighbour_inf")

neighbour<-get_agg_totals(mcmodule=neighbour, mcnode=c("neighbour_inf"))

neighbour<-add_prefix(neighbour)

message("\nNEIGHBOURS PATHWAY OK!")

#' 
#' ### Save results
#' 
