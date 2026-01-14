#' ---
#' title: "Animal Purchase pathogen introduction pathway"
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
#' This pathway analyses the probability of pathogen introduction when animals are purchased. The pathway consists of several modules such as farm, quarantine, transport and fattening. Each module has its own set of data preparation, evaluation and combination steps. The risk of introduction is also assessed under different what-if biosecurity scenarios.
#' 
## ------------------------------------------------------------------------------------------------------------------
message("\nPURCHASE PATHWAY: ")

#' 
#' ## Farm module
#' 
#' ### Prepare data
#' 
#' Tidy data from biosecurity and movements surveys
#' 
## ------------------------------------------------------------------------------------------------------------------
#Tidy number of animals by animal category
purchase_animal<-tidy_animal_table(pathway = "purchase")

#Tidy health status by disease and farm
purchase_status_origin<-tidy_status_table(pathway="purchase", module="origin")

#Tidy diagnostic tests before transport by disease, farm and type of test
purchase_test_origin<-tidy_test_table(pathway="purchase", module="origin")


#Find homogeneous grups
purchase_health_origin<-purchase_animal%>%
  left_join(purchase_status_origin, relationship = "many-to-many")%>%
  left_join(purchase_test_origin)%>%
  left_join(tidy_prefix(mov,"purchase_quarantine", rm_prefix="purchase"))%>%
  tidy_group()


#Get the smallest region code
purchase_region<-get_region_code(mov, pathway = "purchase")

veh<-tidy_prefix(tidy_panel(panel="veh"),"veh", rm_prefix=FALSE)%>%
  filter(!is.na(veh_id))

#Get mov vehicles table
purchase_veh<-filter(mov, mov_type=="purchase")%>%
  tidy_prefix("purchase_veh", rm_prefix="purchase")%>%
  left_join(veh)%>%
  distinct()

#' 
#' Merge tidy user data with admin inputs
#' 
## ------------------------------------------------------------------------------------------------------------------
#Origin
purchase_origin_data<-purchase_animal%>%
  left_join(purchase_region)%>%
  left_join(pathogen_animal,relationship = "many-to-many")%>%
  left_join(pathogen)%>%
  hjoin_region(pathogen_region, add_keys=c("animal_category"))%>%
  left_join(purchase_health_origin)%>%
  mutate(status=ifelse(status=="unk_plan", "unk",as.character(status)))%>%
  left_join(pathogen_status)

#' 
## ------------------------------------------------------------------------------------------------------------------
#Test
purchase_test_data<-purchase_test_origin%>%
  left_join(pathogen_test)

#Used m2 in each movement
purchase_m2_used<-purchase_animal%>%
  left_join(purchase_region)%>%
  hjoin_region(animal_region)%>%
  mutate(m2_used=veh_m2animal*animals_n*farms_total)%>%
  group_by(farm_id, mov_id)%>%
  summarize(m2_used=sum(m2_used),
            animals_total=sum(animals_n*farms_total))


#Estimate the number of animals from other farms that could fit in the vehicle
purchase_veh_animal<-purchase_veh%>%
  left_join(purchase_m2_used)%>%
  left_join(purchase_animal)%>%
  left_join(purchase_region)%>%
  hjoin_region(animal_region)%>%
  mutate(veh_size=ifelse(is.na(veh_size),veh_size_ext,veh_size),
         m2_free=veh_size-m2_used)%>%
  mutate(animals_p=animals_n/animals_total,
         other_all_animals_n=(animals_p*m2_free)/veh_m2animal,
         other_all_animals_n=ifelse(other_all_animals_n>0,other_all_animals_n,2),
         prev_all_animals_n=veh_size/veh_m2animal,
         surface=ifelse(veh_cleaning=="each","metal","soil"))

#' 
#' #### Merge all
#' 
## ------------------------------------------------------------------------------------------------------------------
#All data
purchase_data<-purchase_origin_data%>%
  left_join(purchase_test_data)

purchase_data<-purchase_data%>%
  select(setdiff(names(purchase_data), names(purchase_veh_animal))| matches("mov_id|farm_id|animal_category"))%>%
  left_join(purchase_veh_animal)%>%
  left_join(pathogen_surface)

message("\npurchase_data (",paste(dim(purchase_data), collapse=", "),") created")

#' 
#' #### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Test  before transport
  purchase_data<-wif_test(purchase_data, scenario="Test before transport")
  
  #Use, and not share, your own vehicle
  #purchase_data<-wif_own_veh(purchase_data, scenario="Use, and not share, your own vehicle (purchase)")
  
  #Use, and not share, your own vehicle
  purchase_data<-wif_no_share_veh(purchase_data, scenario="No shared purchase transport", prev_wif="wif_own_veh")
  
  #Clean the vehicle
  #purchase_data<-wif_clean(purchase_data, scenario="Clean and disinfect the vehicle between transports")
  
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
purchase_expression<-c(purchase_farm_link=farm_link_expression,
                       purchase_farm_origin = origin_expression,
                       purchase_farm_test = test_expression)

farm<-eval_model_expression(model_expression = purchase_expression,
                                     data=purchase_data)

farm<-get_totals(mcmodule=farm, mcnodes=c("a_inf","a_inf_pi","a_inf_tr"))

farm<-get_totals(mcmodule=farm, mcnodes=c("a_no_detect","a_no_detect_pi","a_no_detect_tr"))

farm<-get_agg_totals(mcmodule=farm, mcnode=c("a_inf_all"))

farm<-get_agg_totals(mcmodule=farm, mcnode=c("b_inf_all"))

farm<-get_agg_totals(mcmodule=farm, mcnode=c("a_no_detect_all"))

farm<-get_totals(mcmodule=farm, mcnode=c("a_no_detect_infectious"), all_mcnodes = FALSE)

farm<-add_prefix(farm)

#' 
#' ## Other farm module
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------
#Other origin keys
unk_origin_by<-c("mov_id", "farm_id","animal_category", "pathogen")
#Other origin
purchase_unk_origin_data<-purchase_animal%>%
  left_join(purchase_region)%>%
  left_join(pathogen_animal)%>%
  left_join(pathogen)%>%
  hjoin_region(pathogen_region, add_keys ="animal_category")%>%
  #left_join(purchase_health_origin)%>%
  mutate(status="unk")%>%
  left_join(pathogen_status)
  
purchase_unk_origin_data<-purchase_unk_origin_data%>%
  select(setdiff(names(purchase_unk_origin_data), names(purchase_veh_animal))| matches("mov_id|farm_id|animal_category"))%>%
  left_join(purchase_veh_animal)%>%
  add_group_id(by=unk_origin_by)

message("\npurchase_unk_origin_data (",paste(dim(purchase_unk_origin_data), collapse=", "),") created")

#' 
## ------------------------------------------------------------------------------------------------------------------
purchase_unk_origin_data<-group_match(purchase_unk_origin_data,purchase_data)

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
unk_origin_expression<-c(purchase_unk_link=unk_link_expression,
                         purchase_unk_origin=origin_expression)

unk_farm<-eval_model_expression(model_expression = unk_origin_expression,
                                     data=purchase_unk_origin_data)

unk_farm<-get_totals(mcmodule=unk_farm, mcnodes=c("a_inf_infectious","a_inf_pi"),
                     trials_n="other_animals_n", 
                     subsets_n="veh_farms_from_n",
                     prefix="other",
                     all_mcnodes=FALSE)

unk_farm<-get_totals(mcmodule=unk_farm, mcnodes=c("a_inf_infectious","a_inf_pi"),
                     trials_n="prev_animals_n",
                     subsets_n="veh_farms_from_n",
                     prefix="prev",
                     all_mcnodes=FALSE)

unk_farm<-get_agg_totals(mcmodule=unk_farm, 
                         mcnode=c("prev_b_inf_infectious"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

unk_farm<-get_agg_totals(mcmodule=unk_farm, 
                         mcnode=c("prev_b_inf_pi"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

unk_farm<-get_agg_totals(mcmodule=unk_farm, 
                         mcnode=c("other_b_inf_infectious"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

unk_farm<-get_agg_totals(mcmodule=unk_farm, 
                         mcnode=c("other_b_inf_pi"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

unk_farm<-add_prefix(unk_farm)

#' 
#' ### Combine modules farm and unknown farm
#' 
## ------------------------------------------------------------------------------------------------------------------
purchase<-combine_modules(farm, unk_farm)

#' 
#' ## Transport module
#' 
#' It is assumed that the probability of infection of an animal is the same as the **probability of infection of the animals currently in the vehicle** (per farm), but the probability of at least one animal being infected depends on the free space in the vehicle (`animals_other_n`) and the number of farms from which it is shared (or number of farms shared by default).
#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
transport_expression<-c(purchase_transport_link=transport_link_expression,
                        purchase_transport_dir_contact_transport= dir_contact_expression,
                        purchase_transport_indir_contact_transport=indir_contact_expression)

transport<-eval_model_expression(
  model_expression = transport_expression,
  prev_mcmodule = purchase,
  data = purchase_data)
  #create_nodes = FALSE)


transport<-get_totals(mcmodule=transport, mcnodes=c("a_dir_contact","a_dir_contact_pi"))

transport<-get_totals(mcmodule=transport, mcnodes=c("a_indir_contact","a_indir_contact_pi"))

transport<-get_totals(mcmodule=transport, mcnodes=c("a_dir_contact_all","a_indir_contact_all"), name = "a_contact_all")

transport<-get_agg_totals(mcmodule=transport, mcnode=c("a_contact_all"))

transport<-add_prefix(transport)

#' 
#' ### Combine modules purchase and transport
#' 
## ------------------------------------------------------------------------------------------------------------------
purchase<-combine_modules(purchase, transport)

purchase<-get_totals(mcmodule=purchase, mcnodes=c("transport_a_contact_all","farm_a_no_detect_all"), trials_n="farm_animals_n", name="purchase_a_inf_all")

purchase<-get_totals(mcmodule=purchase, mcnodes=c("transport_a_contact_all","farm_a_no_detect"), trials_n="farm_animals_n", name="purchase_a_inf")

#' 
#' ## Quarantine module
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------
#Tidy diagnostic tests before transport by disease, farm and type of test
purchase_test_quarantine<-tidy_test_table(pathway="purchase", module="quarantine")

#Tidy quarantine parameters
quarantine_bsg_data<-tidy_prefix(bsg, module="quarantine", rm_prefix = FALSE)

#Add movement and tests info
quarantine_data<-quarantine_bsg_data%>%
  left_join(purchase_origin_data[,!names(purchase_origin_data)%in%"test"])%>%
  left_join(purchase_test_quarantine)%>%
  left_join(pathogen_test)%>%
  mutate(quarantine_test_time="end",
         surface=ifelse(quarantine_cleaning=="each","metal","soil"))%>%
  left_join(pathogen_surface)

message("\nquarantine_data (",paste(dim(quarantine_data), collapse=", "),") created")

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Match index
  quarantine_by<-c("mov_id", "farm_id","animal_category", "pathogen","status")

  quarantine_data<-group_match(quarantine_data,purchase_data,quarantine_by)
  
  #Clean quarantine equipement
  quarantine_data<-wif_quarantine(quarantine_data, scenario="Quarantine new animals")
  
  #Test  during quarantine
  quarantine_data<-wif_test(quarantine_data, scenario="Test during quarantine", prev_wif = "wif_quarantine")
  
  #Clean quarantine equipement
  quarantine_data<-wif_clean(quarantine_data, scenario="Clean and disinfect quarantine equipment", prev_wif = "wif_quarantine")
  
  #Exclusive quarantine material
  quarantine_data<-wif_exclusive_material(quarantine_data, scenario="Exclusive quarantine material", prev_wif="wif_quarantine")
  
  #Test during quarantine with exclusive material
  quarantine_data<-combine_wif(data=quarantine_data, scenario="Test during quarantine with exclusive material", wif_list=c("quarantine","exclusive_material","test"))
  
}

#' 
#' ### Evaluate I
#' 
## ------------------------------------------------------------------------------------------------------------------
quarantine_I_expression<-c(purchase_quarantine_I_link=quarantine_I_link_expression,
                     purchase_quarantine_test=test_expression)

quarantine_I<-eval_model_expression(
  model_expression = quarantine_I_expression,
  prev_mcmodule = purchase,
  param_names=c(a_inf ="purchase_a_inf",
                a_inf_pi="farm_a_no_detect_pi",
                a_inf_tr="farm_a_no_detect_tr",
                b_contact="transport_b_contact_all"),
  data=quarantine_data,
  match_prev = TRUE)

quarantine_I<-get_totals(mcmodule=quarantine_I, mcnodes=c("a_no_detect","a_no_detect_pi","a_no_detect_tr"))

quarantine_I<-get_agg_totals(mcmodule=quarantine_I, mcnode=c("a_no_detect_all"))

quarantine_I<-get_totals(mcmodule=quarantine_I, mcnode=c("a_detect_infectious","a_detect_pi"), name="a_detect_infectious_all")

quarantine_I<-get_agg_totals(mcmodule=quarantine_I, mcnode=c("a_detect_infectious_all"))

quarantine_I<-add_prefix(quarantine_I)

#' 
#' ### Combine modules purchase and quarantine I
#' 
## ------------------------------------------------------------------------------------------------------------------
purchase<-combine_modules(purchase, quarantine_I)

#' 
#' ### Evaluate II
#' 
## ------------------------------------------------------------------------------------------------------------------
quarantine_II_expression<-c(purchase_quarantine_II_link=quarantine_II_link_expression,
                            purchase_quarantine_indir_contact=indir_contact_expression,
                     purchase_quarantine_weight=quarantine_weight_expression)

quarantine_II<-eval_model_expression(
  model_expression = quarantine_II_expression,
  prev_mcmodule = purchase,
  param_names=c(b_detect_infectious="quarantine_I_b_detect_infectious",
                b_detect_pi="quarantine_I_b_detect_pi",
                b_no_detect_infectious="quarantine_b_no_detect_infectious",
                b_no_detect_pi="quarantine_b_no_detect_pi",
                b_contact="transport_b_contact_all"),
  data=quarantine_data,
  match_prev = TRUE)


quarantine_II<-get_totals(mcmodule=quarantine_II, mcnodes=c("a_indir_contact", "a_indir_contact_pi"))

quarantine_II<-get_agg_totals(mcmodule=quarantine_II, mcnode=c("b_indir_contact_all"))

quarantine_II<-get_totals(mcmodule=quarantine_II, mcnodes=c("farm_a_no_detect_all_weight"))

quarantine_II<-get_totals(mcmodule=quarantine_II, mcnodes=c("transport_a_contact_all_weight"))

quarantine_II<-add_prefix(quarantine_II)

#' 
#' ### Combine modules quarantine I and quarantine II
#' 
## ------------------------------------------------------------------------------------------------------------------
quarantine<-combine_modules(quarantine_I, quarantine_II)


quarantine<-get_totals(mcmodule=quarantine, mcnodes=c("quarantine_I_a_no_detect_all","quarantine_II_a_indir_contact_all"), name = "a_entry_all")

quarantine<-get_totals(mcmodule=quarantine, mcnodes=c("quarantine_I_f_no_detect_all","quarantine_II_f_indir_contact_all"), name = "f_entry_all")


quarantine<-get_totals(mcmodule=quarantine, mcnode=c("a_entry_all"))

quarantine<-get_agg_totals(mcmodule=quarantine, mcnode=c("a_entry_all"))

quarantine<-get_agg_totals(mcmodule=quarantine, mcnode=c("b_entry_all"))

quarantine<-add_prefix(quarantine)

#' 
#' ### Combine modules purchase and quarantine
#' 
## ------------------------------------------------------------------------------------------------------------------
purchase<-combine_modules(purchase, quarantine)

#' 
#' ## Fattening module
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------
#Tidy quarantine parameters
fattening_bsg_data<-tidy_prefix(bsg, module="fattening", rm_prefix = FALSE)

#Add movement and tests info
fattening_data<-fattening_bsg_data%>%
  left_join(purchase_origin_data)%>%
  mutate(surface="soil")%>%
  left_join(pathogen_surface)%>%
  mutate(test=NA)

message("\nfattening_data (",paste(dim(fattening_data), collapse=", "),") created")

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Test  before transport
  fattening_data<-group_match(fattening_data, purchase_data, quarantine_by)
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
fattening_expression<-c(purchase_fattening_link=fattening_link_expression,
                     purchase_fattening_indir_contact=indir_contact_expression)

fattening<-eval_model_expression(
  model_expression = fattening_expression,
  prev_mcmodule = purchase,
  param_names=c(b_no_detect="farm_b_no_detect",
                b_no_detect_pi="farm_b_no_detect_pi",
                b_contact="farm_b_no_detect_all"),
  data=fattening_data)


fattening<-get_totals(mcmodule=fattening, mcnodes=c("a_indir_contact"))

fattening<-get_agg_totals(mcmodule=fattening, mcnode=c("a_indir_contact"))


fattening<-add_prefix(fattening)

#' 
#' ### Combine modules purchase and fattening
#' 
## ------------------------------------------------------------------------------------------------------------------
purchase<-combine_modules(purchase, fattening)

purchase<-get_totals(mcmodule=purchase, mcnodes=c("quarantine_a_entry_all_all","fattening_a_indir_contact_all"), name = "a_entry")

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("farm_b_no_detect_all"))

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("quarantine_II_farm_b_no_detect_all_weight"))

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("transport_b_contact_all"))

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("quarantine_II_transport_b_contact_all_weight"))

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("purchase_b_inf_all"))


purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("b_entry"))

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("b_entry"), keys_names= c("pathogen", "farm_id","scenario_id", "mov_id"), suffix="mov")

purchase<-get_agg_totals(mcmodule=purchase, mcnode=c("b_entry"), keys_names= c("pathogen", "farm_id","scenario_id", "mov_id" ,"animal_category"), suffix="mov_animal")

message("\nPURCHASE PATHWAY OK!")

#' 
#' ## Save results
#' 
