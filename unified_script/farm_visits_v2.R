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
## ------------------------------------------------------------------------------------------------------------------------------------
message("\nVISITS PATHWAY: ")

#' 
#' ## Visits module
#' 
#' ### Prepare data: previous farm
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
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
         #Unknown pathogen plan health_status
         health_status="unk")

#PROVISIONAL for back-compatibility
if(form_version(bsg)<215){
  visit_data<-visit_data%>%
    mutate(visit_close = grepl("veh_driver_help",bsg$complete_comments)
         &(visit_type=="slaughter_veh"|
             visit_type=="calf_veh"|
             visit_type=="adult_veh"))
}

visit_own_exists<-"visit_own"%in%names(visit_data) #BACKWARDS COMPATIBILITY for #141

visit_data_prev<-visit_data%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  left_join(pathogen_animal)%>%
  left_join(animal)

#' 
## ------------------------------------------------------------------------------------------------------------------------------------
# Keys to aggregate by to account for same probability of herd being infected by all animal categories from the same herd (Multiple group multilevel trials)
agg_fomite_keys<- c("pathogen","scenario_id", "farm_id", "visit_id", "fomite_id")
agg_visit_keys<- c("pathogen","scenario_id", "farm_id", "visit_id")
agg_totals_keys<- c("pathogen","scenario_id", "farm_id")
levels_herd<-c(trial="oneanimal",subset="oneherd",set="allherds")

#' 
#' ### Evaluate I
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
visit_prev_exp<-c(
  visit_prev_link = visit_prev_link_exp,
  visit_prev_herd_select = herd_select_exp,
  visit_prev_animal_select = animal_select_exp)

visit<-mcmodule::eval_module(
  exp = visit_prev_exp,
  data = visit_data_prev)

# At least one animal in at least one of the previously visited herd is either infectious or pi, aggregated by "pathogen","scenario_id", "fomite_id" and "visit_id" (hag by animal_category within the herd)
visit<-mcmodule::trial_totals(
  mcmodule=visit,
  mc_names = c("infectious","infected_pi"),
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "herd_inf",
  agg_keys = agg_fomite_keys,
  prefix = "prev",
  combine_prob = FALSE,
  level_suffix = levels_herd)

#' 
#' ### Prepare data: current farm
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
visit_data_current<-visit_data%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  mutate(surface="soil")%>%
  left_join(pathogen_surface, relationship = "many-to-many")%>%
  #TO SOLVE PROVISIONALLY #141 visit_own = No risk
  mutate(visit_own=if(visit_own_exists) visit_own else NA,
         visit_enter=ifelse(!is.na(visit_own)&visit_own,"never", visit_enter))

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Do not allow vehicles to enter in the farm perimeter
  visit_data_current<-wif_no_fomites(visit_data_current, fomites="wheels", scenario="Do not allow vehicles to enter in the farm perimeter")
  
  #Provide boots to all visitors
  visit_data_current<-wif_no_fomites(visit_data_current, visit_veh==FALSE, fomites="boots", scenario="Provide boots to all visitors")
  
  #Provide boots to all drivers
  visit_data_current<-wif_no_fomites(visit_data_current, visit_veh==TRUE, fomites="boots", scenario="Provide boots to all drivers")
  
  #Do not share equipment with other farms
  visit_data_current<-wif_no_fomites(visit_data_current, fomites="equipment", scenario="Do not share equipment with other farms")
  
}

#' 
#' ### Evaluate II
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
visit_indir_exp<-c(
  visit_indir_link = visit_indir_link_exp, 
  visit_indir_contact = indir_contact_exp)

visit_indir<-mcmodule::eval_module(
  exp = visit_indir_exp,
  prev_mcmodule = visit,
  data = visit_data_current)

#TODO - trial totals with n_times instead of including it in link expression! to allow aggregated results by 1 visit #189
visit<-mcmodule::combine_modules(visit, visit_indir)

# One animal in the farm is infected  by fomites contaminated by previous herds either infectious or pi animals
visit<-mcmodule::at_least_one(
  mcmodule=visit,
  mc_names = c("indir_contact","indir_contact_pi"))

# At least one fomite is one visits infects one farm animal aggregated by "pathogen","scenario_id" and "visit_id"
visit<-mcmodule::agg_totals(
  mcmodule=visit,
  mc_name = c ("indir_contact_all"),
  agg_keys = agg_visit_keys,
  agg_suffix ="type")

# At least one fomite is one visits infects one farm animal aggregated by "pathogen","scenario_id" and "visit_veh" (people visits vs vehicle visits)
visit<-mcmodule::agg_totals(
  mcmodule=visit,
  mc_name=c("indir_contact_all"),
  agg_keys=c(agg_totals_keys,"visit_veh"),
  agg_suffix="veh")

# At least one fomite is one visits infects one farm animal aggregated by "pathogen","scenario_id" and "visit_name" (people visits vs vehicle visits)
visit<-mcmodule::agg_totals(
  mcmodule=visit,
  mc_name=c("indir_contact_all"),
  agg_keys=c(agg_totals_keys,"visit_name"),
  agg_suffix="name")

# At least one fomite is one visits infects one farm animal aggregated by "pathogen" and "scenario_id" 
visit<-mcmodule::agg_totals(
  mcmodule=visit,
  mc_name=c("indir_contact_all"),
  agg_keys=agg_totals_keys)

visit<-add_prefix(visit)

print_pathway_risk(visit, "visit_indir_contact_all_agg")
message("\nVISITS PATHWAY OK!")

