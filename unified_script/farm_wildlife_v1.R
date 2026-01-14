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
message("\nFARM WILDLIFE PATHWAY: ")

#' 
#' ## Wildlife area effect module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------
wildlife_area_data<-get_region_code(bsg, pathway = "farm")%>%
  left_join(tidy_prefix(bsg, "outdoors"))

wildlife_area_data<-wildlife_area_data%>%
  mutate(surface="soil")%>%
  left_join(pathogen_surface)%>%
  hjoin_region(wildlife_region)%>%
  hjoin_region(wildlife_pathogen_region)%>%
  left_join(wildlife_point_density)%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  #Filter those that have wildlife prevalence
  filter(pathogen%in%
           unique(wildlife_pathogen_region$pathogen[wildlife_pathogen_region$wl_prev_mode>0]))

message("\nwildlife_area_data (",paste(dim(wildlife_area_data), collapse=", "),") created")

#' 
#' ### Prepare what-if
#' 
## -------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #There are no admin what-ifs in this module, but we set up wif for compatibility
  wildlife_area_data<-set_up_wif(wildlife_area_data)
}

#' 
#' ### Evaluate
#' 
## -------------------------------------------------------------------------------------------------------------------
wildlife_area_expression<-c(wildlife_area_link = wildlife_area_link_expression,
                            wildlife_area_inf=area_inf_expression)  

wildlife<-eval_model_expression(model_expression = wildlife_area_expression,                              data=wildlife_area_data)  

wildlife<-get_agg_totals(mcmodule=wildlife, mcnode=c("area_inf"))

wildlife<-add_prefix(wildlife)

#' 
#' ## Wildlife contact points module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------
farm_census_data<-tidy_prefix(bsg, "farm_census")%>%
  pivot_longer(
      cols = starts_with("cattle"),
      names_to = "animal_category",
      values_to = "census",
      values_drop_na = TRUE
    )%>%
  left_join(animal)%>%
  mutate(livestock_units=census*livestock_units)

wildlife_water_data<-wildlife_area_data%>%
  pivot_longer(
    cols = ends_with("_p")&!mud_p&!sum_p,
    names_to = "waterpoint_type",
    values_to = "point_p",
    values_drop_na = TRUE
  )%>%
  mutate(
    animals_n=sum(farm_census_data$census),
    livestock_units=sum(farm_census_data$livestock_units),
    contact_point_type=sub("_p", "", waterpoint_type),
    mud_p=ifelse(contact_point_type%in%c("low", "high"),ifelse(mud,as.numeric(gsub("%", "", mud_p)),0),100)/100,
    contact_point_p=as.numeric(gsub("%", "", point_p))/100
  )

wildlife_water_data<-wildlife_water_data%>%
  left_join(wildlife_point)%>%
  left_join(animal_point)

message("\nwildlife_water_data (",paste(dim(wildlife_water_data), collapse=", "),") created")

#' 
#' ### Admin what-if
#' 
## -------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Avoid mud on wateres
  wildlife_water_data<-wif_no_mud(wildlife_water_data, scenario="Avoid mud on wateres")
  wildlife_water_data<-wif_high_waterer(wildlife_water_data, scenario="Use high wateres")
}

#' 
#' ### Evaluate
#' 
## -------------------------------------------------------------------------------------------------------------------
wildlife_water_expression<-c(
  wildlife_link = wildlife_water_link_expression,
  wildlife_time = time_expression,
  wildlife_indir_contact=indir_contact_expression)

#debugonce(get_node_list)
wildlife_water<-eval_model_expression(
  model_expression = wildlife_water_expression,
  data=wildlife_water_data)

wildlife_water<-get_totals(mcmodule=wildlife_water, mcnode=c("a_indir_contact"))

wildlife_water<-get_agg_totals(mcmodule=wildlife_water, mcnode=c("b_indir_contact"))

wildlife_water<-add_prefix(wildlife_water)

#' 
#' ## Combine modules
#' 
## -------------------------------------------------------------------------------------------------------------------
wildlife<-combine_modules(wildlife, wildlife_water)

wildlife<-at_least_one(mcmodule=wildlife, mcnodes=c("wildlife_area_inf_agg","wildlife_water_b_indir_contact_agg"), name="wildlife_inf_agg")

message("\nFARM WILDLIFE PATHWAY OK!")

#' 
#' ## Save results
#' 
