#' ---
#' title: "Farm visits pathogen introduction pathway"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 

#' 
#' ## Description
#' 
## -------------------------------------------------------------------------------------------------------------------
message("\nVISITS PATHWAY: ")

#' 
#' ## Visits module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------
visit_data<-get_region_code(bsg, pathway = "farm")

visit_veh_data<-visit_data%>%
  left_join(tidy_prefix(tidy_panel(panel="visit_veh"),"visit_veh", rm_prefix = "veh"))%>%
  filter(!is.na(visit_type))%>%
  mutate(visit_type=as.factor(visit_type),
         visit_frequency=as.numeric(visit_frequency),
         visit_boots_enter=as.factor(visit_boots_enter),
         visit_boots_cleaning=as.factor(visit_boots_cleaning))


visit_people_data<-visit_data%>%
  left_join(tidy_prefix(tidy_panel(select(bsg,!contains("_veh")), panel="visit"),"visit", rm_prefix = FALSE))%>%
  filter(!is.na(visit_type))%>%
  mutate(visit_type=as.factor(visit_type),
         visit_frequency=as.numeric(visit_frequency),
         visit_boots_enter=as.factor(visit_boots_enter),
         visit_boots_cleaning=as.factor(visit_boots_cleaning))



visit_data<-visit_people_data%>%
  bind_rows(visit_veh_data)%>%
  mutate(visit_id=ifelse(is.na(visit_id),visit_veh_id,visit_id),
         #If is people, direct contact is TRUE (implicit in question)
         visit_direct=ifelse(is.na(visit_veh_id),TRUE,visit_direct),
         visit_veh_id=NULL,
         #provisional step to TEST visit link
         prev_b_inf_TEST=0.6)


visit_data<-visit_data%>%
  hjoin_region(visit_region)%>%
  mutate(visit_livestock=ifelse(is.na(visit_livestock_cattle_p),
                                visit_livestock_type_cattle,
                                visit_livestock_cattle_p))

visit_data<-visit_data%>%
  pivot_longer(cols=starts_with("visit_boots")|starts_with("visit_equipment")|starts_with("visit_wheels"),
               names_to = c("fomite_type", ".value"),
               names_pattern = "^visit_(boots|equipment|wheels)_(.*)$",
               names_repair = ~ gsub("^(cleaning|enter)", "visit_\\1", .x))%>%
  filter(!is.na(fomite_type))%>%
  #Remove equipment pathway in vehicles and wheels pathway in people
  filter(!((visit_veh&fomite_type=="equipment")|(!visit_veh&fomite_type=="wheels")))%>%
  #Filter if the fomite never enters the farm (in vehicle wheels always enter)
  filter(!(visit_enter=="never"&visit_direct)|(visit_veh))%>%
  #Filter "boots" if driver never has contact (direct or close)
  filter(!(fomite_type=="boots"&!visit_direct)|!(fomite_type=="boots"&!visit_close))%>%
  #If visit_enter is NA, it is assumed worst case scenario: "always"
  mutate(visit_enter=ifelse(is.na(visit_enter),
                            ifelse(fomite_type=="wheels", "always",
                                   as.character(visit_enter)),
                            as.character(visit_enter)),
         #Add fomite_id
         fomite_id=paste0(visit_id,"_",fomite_type),
         #Unknown pathogen plan status
         status="unk")

#PROVISIONAL for back-compatibility
if(form_version(bsg)<215){
  visit_data<-visit_data%>%
    mutate(visit_close = grepl("veh_driver_help",bsg$complete_comments)
         &(visit_type=="slaughter_veh"|
             visit_type=="calf_veh"|
             visit_type=="adult_veh"))
}

visit_own_exists<-"visit_own"%in%names(visit_data) #BACKWARDS COMPATIBILITY for #141

visit_data<-visit_data%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  left_join(pathogen_animal)%>%
  left_join(animal)%>%
  mutate(surface="soil")%>%
  left_join(pathogen_surface)%>%
  #TO SOLVE PROVISIONALLY #141 visit_own = No risk
  mutate(visit_own=if(visit_own_exists) visit_own else NA,
         visit_enter=ifelse(!is.na(visit_own)&visit_own,"never", visit_enter))

#' 
#' ### Admin what-if
#' 
## -------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Do not allow vehicles to enter in the farm perimeter
  visit_data<-wif_no_fomites(visit_data, fomites="wheels", scenario="Do not allow vehicles to enter in the farm perimeter")
  
  #Provide boots to all visitors
  visit_data<-wif_no_fomites(visit_data, visit_veh==FALSE, fomites="boots", scenario="Provide boots to all visitors")
  
  #Provide boots to all drivers
  visit_data<-wif_no_fomites(visit_data, visit_veh==TRUE, fomites="boots", scenario="Provide boots to all drivers")
  
  #Do not share equipment with other farms
  visit_data<-wif_no_fomites(visit_data, fomites="equipment", scenario="Do not share equipment with other farms")
  
}

#' 
#' ### Evaluate I
#' 
## -------------------------------------------------------------------------------------------------------------------
visit_farm_expression<-c(visit_link = visit_link_expression,
                         visit_origin = origin_expression)

visit<-eval_model_expression(model_expression = visit_farm_expression,
                             data=visit_data)

visit<-get_totals(mcmodule=visit, 
                  mcnodes=c("a_inf_infectious","a_inf_pi"),
                  prefix="prev",
                  all_mcnodes=FALSE)

visit<-get_agg_totals(mcmodule=visit, 
                         mcnode=c("prev_b_inf_infectious"),
                         keys_names=c("pathogen", "farm_id", "fomite_id"),
                         keep_variates=TRUE)

visit<-get_agg_totals(mcmodule=visit, 
                         mcnode=c("prev_b_inf_pi"),
                         keys_names=c("pathogen", "farm_id", "fomite_id"),
                         keep_variates=TRUE)

#' 
#' ### Evaluate II
#' 
## -------------------------------------------------------------------------------------------------------------------
visit_indir_expression<-c(visit_indir_contact=indir_contact_expression)

visit_indir<-eval_model_expression(model_expression = visit_indir_expression,
                             prev_mcmodule = visit,
                             data=visit_data)

visit<-combine_modules(visit, visit_indir)

visit<-get_totals(mcmodule=visit,
                  mcnodes=c("a_indir_contact","a_indir_contact_pi"))

visit<-get_agg_totals(mcmodule=visit,mcnode=c("a_indir_contact"), keys_names=c("pathogen", "farm_id","scenario_id", "visit_type"), suffix="type")

visit<-get_agg_totals(mcmodule=visit,mcnode=c("a_indir_contact"), keys_names=c("pathogen", "farm_id","scenario_id", "visit_veh"), suffix="veh")

visit<-get_agg_totals(mcmodule=visit, mcnode=c("a_indir_contact_all"))

visit<-get_agg_totals(mcmodule=visit,mcnode=c("a_indir_contact_all"), keys_names=c("pathogen", "farm_id","scenario_id", "visit_type"), suffix="type")

visit<-get_agg_totals(mcmodule=visit,mcnode=c("a_indir_contact_all"), keys_names=c("pathogen", "farm_id","scenario_id", "visit_name"), suffix="name")

visit<-get_agg_totals(mcmodule=visit,mcnode=c("a_indir_contact_all"), keys_names=c("pathogen", "farm_id","scenario_id", "visit_veh"), suffix="veh")

visit<-add_prefix(visit)

message("\nVISITS PATHWAY OK!")

#' 
#' ## Save results
#' 

#' 
#' ## Output values
#' 
#' We are only going to save the probability of disease introduction at aggregated level (one year movements)
