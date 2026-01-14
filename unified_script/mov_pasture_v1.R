#' ---
#' title: "Animal pasture pathogen introduction pathway"
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
#' This pathway analyses the probability of pathogen introduction when animals go to pastured. The pathway consists of several modules. Each module has its own set of data preparation, evaluation and combination steps. The risk of introduction is also assessed under different what-if biosecurity scenarios.
#' 
## ------------------------------------------------------------------------------------------------------------------
message("\nPASTURE PATHWAY: ")

#' 
#' ## Risk days
#' 
## ------------------------------------------------------------------------------------------------------------------
from_date<-mov$pasture_from_date[!is.na(mov$pasture_from_date)]
to_date<-mov$pasture_to_date[!is.na(mov$pasture_to_date)]
mov$pasture_risk_days<-mov$pasture_to_date-mov$pasture_from_date

from_date_s<-c(min(to_date)-1, sort(from_date))
to_date_s<-c(sort(to_date),max(from_date)+1)

diff_date<-to_date_s-from_date_s
cont_date<-diff_date>5 #Assume pasture continuity if gap is under 5 days

cont_to_date<-ifelse(cont_date,NA, to_date_s)
cont_from_date<-ifelse(cont_date,NA, from_date_s)

pasture_risk_days<-data.frame(cont_to_date,cont_from_date)%>%
  fill(cont_to_date, .direction ="up")%>%
  fill(cont_from_date, .direction ="down")%>%
  mutate(cont_diff_date=cont_to_date-cont_from_date)%>%
  na.omit()%>%
  unique()%>%
  summarise(sum=sum(cont_diff_date))

mov$pasture_risk_days<-mov$pasture_to_date-mov$pasture_from_date

risk_days<-risk_days-pasture_risk_days$sum

#' 
#' ## Pasture data preparation
#' 
#' ### Prepare data
#' 
#' Tidy data from biosecurity and movements surveys
#' 
## ------------------------------------------------------------------------------------------------------------------
#Tidy number of animals by animal category
pasture_animal<-mov%>%
  filter(mov_type=="pasture")%>%
  tidy_prefix("pasture_animals_p", rm_prefix="pasture")%>%
  left_join(get_columns(mov, c("cattle_n","cattlebull", "farms_n","share"), "pasture"))%>%
  left_join(tidy_census_table(),relationship = "many-to-many")%>%
  mutate(animals_p=as.numeric(sub("%", "", animals_p))/100, #ojo puede ser más de 1
         pasture_own_animals_n=animals_n*animals_p, #animals from user farm
         pasture_own_farm_n=1,
         pasture_other_animals_n=pasture_cattle_n/pasture_farms_n,#animals form other farms
         pasture_other_animals_n=ifelse(is.na(pasture_other_animals_n),0,pasture_other_animals_n),
         pasture_other_farm_n=ifelse(is.na(pasture_farms_n),0,pasture_farms_n))%>%
  left_join(animal)%>%
  mutate(livestock_units=animals_n*livestock_units) 



#Tidy health status by disease and farm
pasture_status_origin<-tidy_status_table(pathway="pasture", module="origin")

#Tidy diagnostic tests before pasture_transport by disease, farm and type of test
pasture_test_origin<-tidy_test_table(pathway="pasture", module="origin")


#Find homogeneous grups
pasture_health_origin<-pasture_animal%>%
  left_join(pasture_status_origin,relationship = "many-to-many")%>%
  left_join(pasture_test_origin)%>%
  tidy_group()


#Get the smallest region code
pasture_region<-tidy_panel(panel="pasture")%>%
  filter(!is.na(pasture_id))%>%
  mutate(pasture_gid0=bsg$farm_gid0)%>%
  get_region_code()

#Get mov vehicles table
pasture_to_veh<-filter(mov, mov_type=="pasture")%>%
  tidy_prefix("pasture_to_veh", rm_prefix="pasture_to")%>%
  mutate(veh_direction="pasture_to")

pasture_from_veh<-filter(mov, mov_type=="pasture")%>%
  tidy_prefix("pasture_from_veh", rm_prefix="pasture_from")%>%
  mutate(veh_direction="pasture_from")

pasture_veh<-bind_rows(pasture_to_veh, pasture_from_veh)%>%
  left_join(tidy_prefix(tidy_panel(panel="veh"),"veh", rm_prefix=FALSE), relationship = "many-to-many")%>%
  distinct()

#' 
#' Merge tidy user data with admin inputs
#' 
## ------------------------------------------------------------------------------------------------------------------
#Origin
pasture_origin_data<-pasture_animal%>%
  left_join(pasture_region)%>%
  left_join(pathogen_animal,relationship = "many-to-many")%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  left_join(pasture_health_origin)%>%
  left_join(pathogen_status)

#' 
## ------------------------------------------------------------------------------------------------------------------
#Test
pasture_test_data<-pasture_test_origin%>%
  left_join(pathogen_test)

#Used m2 in each movement
pasture_m2_used<-pasture_animal%>%
  left_join(pasture_region, relationship = "many-to-many")%>%
  hjoin_region(animal_region, add_keys ="animal_category")%>%
  mutate(m2_used=veh_m2animal*animals_n)%>%
  group_by(farm_id, mov_id)%>%
  summarize(m2_used=sum(m2_used),
            animals_total=sum(animals_n))


#Estimate the number of animals from other farms that could fit in the vehicle
pasture_veh_animal<-pasture_veh%>%
  left_join(pasture_m2_used)%>%
  left_join(pasture_animal, relationship = "many-to-many")%>%
  left_join(pasture_region)%>%
  hjoin_region(animal_region, add_keys ="animal_category")%>%
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
pasture_data<-pasture_origin_data%>%
  left_join(pasture_test_data)%>%
  mutate(animals_p=NULL) #desambiguate
  
pasture_data<-pasture_data%>%
  select(setdiff(names(pasture_data), names(pasture_veh_animal)) | matches("mov_id|farm_id|animal_category|veh_id"))%>% #desambiguate
  left_join(pasture_veh_animal, relationship = "many-to-many")%>%
  left_join(pathogen_surface)


#' 
#' #### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Use, and not share, your own vehicle
  pasture_data<-wif_no_share_veh(pasture_data, scenario="No shared pasture transport", prev_wif="wif_own_veh")
}

#' 
#' #### Separate data by vehicle (direction)
#' 
## ------------------------------------------------------------------------------------------------------------------
#Separate data by vehicle (direction)
pasture_to_data<-filter(pasture_data, veh_direction=="pasture_to")
message("\npasture_to_data (",paste(dim(pasture_to_data), collapse=", "),") created")

pasture_from_data<-filter(pasture_data, veh_direction=="pasture_from")
message("\npasture_from_data (",paste(dim(pasture_from_data), collapse=", "),") created")

#' 
#' ## Unknown farm module
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------
#Other origin keys
unk_origin_by<-c("mov_id", "farm_id","animal_category", "pathogen")
#Other origin
pasture_unk_origin_data<-pasture_animal%>%
  left_join(pasture_region)%>%
  left_join(pathogen_animal, relationship = "many-to-many")%>%
  left_join(pathogen)%>%
  hjoin_region(pathogen_region, add_keys ="animal_category")%>%
  #left_join(pasture_health_origin)%>%
  mutate(status="unk")%>%
  left_join(pathogen_status)%>%
  distinct()

#Add vehicle information
pasture_unk_origin_data<-pasture_unk_origin_data%>%
  select(setdiff(names(pasture_unk_origin_data), names(pasture_veh_animal)) | matches("mov_id|farm_id|animal_category"))%>% #desambiguate
  left_join(pasture_veh_animal, relationship = "many-to-many")%>%
  add_group_id(by=unk_origin_by)%>%
  mutate(pregnant_p=0.5) #Does not affect risk but needed to calc origin_expression

#Separate data by vehicle (direction)
#Vehicle to pasture
pasture_to_unk_origin_data<-filter(pasture_unk_origin_data, veh_direction=="pasture_to")
message("\npasture_to_unk_origin_data (",paste(dim(pasture_to_unk_origin_data), collapse=", "),") created")

#Vehicle from pasture
pasture_from_unk_origin_data<-filter(pasture_unk_origin_data, veh_direction=="pasture_from")
message("\npasture_from_unk_origin_data (",paste(dim(pasture_from_unk_origin_data), collapse=", "),") created")

#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_to_unk_origin_data<-group_match(pasture_to_unk_origin_data,pasture_to_data,unk_origin_by)

pasture_from_unk_origin_data<-group_match(pasture_from_unk_origin_data,pasture_from_data,unk_origin_by)

#' 
#' ### Evaluate (to)
#' 
## ------------------------------------------------------------------------------------------------------------------
unk_origin_expression<-c(pasture_unk_link=unk_link_expression,
                         pasture_unk_origin=origin_expression)

pasture_to_unk_farm<-eval_model_expression(model_expression = unk_origin_expression,
                                     data=pasture_to_unk_origin_data)

pasture_to_unk_farm<-get_totals(mcmodule=pasture_to_unk_farm, mcnodes=c("a_inf_infectious","a_inf_pi"),
                     trials_n="other_animals_n", 
                     subsets_n="veh_farms_from_n",
                     prefix="other",
                     all_mcnodes=FALSE)

pasture_to_unk_farm<-get_totals(mcmodule=pasture_to_unk_farm, mcnodes=c("a_inf_infectious","a_inf_pi"),
                     trials_n="prev_animals_n",
                     subsets_n="veh_farms_from_n",
                     prefix="prev",
                     all_mcnodes=FALSE)

pasture_to_unk_farm<-get_agg_totals(mcmodule=pasture_to_unk_farm, 
                         mcnode=c("prev_b_inf_infectious"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

pasture_to_unk_farm<-get_agg_totals(mcmodule=pasture_to_unk_farm, 
                         mcnode=c("prev_b_inf_pi"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

pasture_to_unk_farm<-get_agg_totals(mcmodule=pasture_to_unk_farm, 
                         mcnode=c("other_b_inf_infectious"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

pasture_to_unk_farm<-get_agg_totals(mcmodule=pasture_to_unk_farm, 
                         mcnode=c("other_b_inf_pi"),
                         keys_names=c("pathogen", "farm_id", "mov_id"),
                         agg_variates=TRUE)

pasture_to_unk_farm<-add_prefix(pasture_to_unk_farm)

#' 
#' ### Evaluate (from)
#' 
## ------------------------------------------------------------------------------------------------------------------
#Check if trasnport to and from are equal
from_equal_to<-all(
  pasture_to_data[, !names(pasture_to_data) %in% c("veh_direction")]==
    pasture_from_data[, !names(pasture_from_data) %in% c("veh_direction")],
  na.rm=TRUE)

#If it they are not not equal calcualte unk farm "from" module
if(!from_equal_to){
  
  pasture_from_unk_farm<-eval_model_expression(model_expression = unk_origin_expression,
                                       data=pasture_from_unk_origin_data)
  
  pasture_from_unk_farm<-get_totals(mcmodule=pasture_from_unk_farm, mcnodes=c("a_inf_infectious","a_inf_pi"),
                       trials_n="other_animals_n", 
                       subsets_n="veh_farms_from_n",
                       prefix="other",
                       all_mcnodes=FALSE)
  
  pasture_from_unk_farm<-get_totals(mcmodule=pasture_from_unk_farm, mcnodes=c("a_inf_infectious","a_inf_pi"),
                       trials_n="prev_animals_n",
                       subsets_n="veh_farms_from_n",
                       prefix="prev",
                       all_mcnodes=FALSE)
  
  pasture_from_unk_farm<-get_agg_totals(mcmodule=pasture_from_unk_farm, 
                           mcnode=c("prev_b_inf_infectious"),
                           keys_names=c("pathogen", "farm_id", "mov_id"),
                           agg_variates=TRUE)
  
  pasture_from_unk_farm<-get_agg_totals(mcmodule=pasture_from_unk_farm, 
                           mcnode=c("prev_b_inf_pi"),
                           keys_names=c("pathogen", "farm_id", "mov_id"),
                           agg_variates=TRUE)
  
  pasture_from_unk_farm<-get_agg_totals(mcmodule=pasture_from_unk_farm, 
                           mcnode=c("other_b_inf_infectious"),
                           keys_names=c("pathogen", "farm_id", "mov_id"),
                           agg_variates=TRUE)
  
  pasture_from_unk_farm<-get_agg_totals(mcmodule=pasture_from_unk_farm, 
                           mcnode=c("other_b_inf_pi"),
                           keys_names=c("pathogen", "farm_id", "mov_id"),
                           agg_variates=TRUE)
  
  pasture_from_unk_farm<-add_prefix(pasture_from_unk_farm)
}

#' 
#' ## Pasture transport module
#' 
#' It is assumed that the probability of infection of an animal is the same as the **probability of infection of the animals currently in the vehicle** (per farm), but the probability of at least one animal being infected depends on the free space in the vehicle (`animals_other_n`) and the number of farms from which it is shared (or number of farms shared by default).
#' 
#' ### Evaluate (to)
#' 
## ------------------------------------------------------------------------------------------------------------------
transport_expression<-c(pasture_transport_link=transport_link_expression,
                        pasture_transport_dir_contact= dir_contact_expression,
                        pasture_transport_indir_contact=indir_contact_expression)

pasture_to_transport<-eval_model_expression(
  model_expression = transport_expression,
  prev_mcmodule = pasture_to_unk_farm,
  data = pasture_to_data)
  #create_nodes = FALSE)

pasture_to_transport<-get_totals(mcmodule=pasture_to_transport, mcnodes=c("a_dir_contact","a_dir_contact_pi"),
                     trials_n="pasture_own_animals_n",
                     subsets_n="pasture_own_farm_n")

pasture_to_transport<-get_totals(mcmodule=pasture_to_transport, mcnodes=c("a_indir_contact","a_indir_contact_pi"),
                     trials_n="pasture_own_animals_n",
                     subsets_n="pasture_own_farm_n")

pasture_to_transport<-get_totals(mcmodule=pasture_to_transport, mcnodes=c("a_dir_contact_all","a_indir_contact_all"),
                     trials_n="pasture_own_animals_n",
                     subsets_n="pasture_own_farm_n",
                     name = "a_contact_all")

pasture_to_transport<-get_agg_totals(mcmodule=pasture_to_transport, mcnode=c("a_contact_all"))


pasture_to_transport<-add_prefix(pasture_to_transport)

#' 
#' ### Evaluate (from)
#' 
## ------------------------------------------------------------------------------------------------------------------
#If it is equal copy unk farm "to" module
if(from_equal_to){
  pasture_from_transport<-add_prefix(pasture_to_transport, 
                             prefix="pasture_from_transport",
                             rewrite_module="pasture_to_transport")
  message("\npasture_from_transport mcmodule copied from pasture_to_transport")
#Else calcualte unk farm "from" module
}else{
  pasture_from_transport<-eval_model_expression(
    model_expression = transport_expression,
    prev_mcmodule = pasture_from_unk_farm,
    data = pasture_from_data)
    #create_nodes = FALSE)

  pasture_from_transport<-get_totals(mcmodule=pasture_from_transport, mcnodes=c("a_dir_contact","a_dir_contact_pi"),
                       trials_n="pasture_own_animals_n",
                       subsets_n="pasture_own_farm_n")
  
  pasture_from_transport<-get_totals(mcmodule=pasture_from_transport, mcnodes=c("a_indir_contact","a_indir_contact_pi"),
                       trials_n="pasture_own_animals_n",
                       subsets_n="pasture_own_farm_n")
  
  pasture_from_transport<-get_totals(mcmodule=pasture_from_transport, mcnodes=c("a_dir_contact_all","a_indir_contact_all"),
                       trials_n="pasture_own_animals_n",
                       subsets_n="pasture_own_farm_n",
                       name = "a_contact_all")
  
  pasture_from_transport<-get_agg_totals(mcmodule=pasture_from_transport, mcnode=c("a_contact_all"))
  
  pasture_from_transport<-get_agg_totals(mcmodule=pasture_from_transport, mcnode=c("b_contact_all"))

  pasture_from_transport<-add_prefix(pasture_from_transport)
}

#' 
#' ### Combine modules transport from and to pasture
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_transport<-combine_modules(pasture_to_unk_farm, pasture_to_transport)
pasture_transport<-combine_modules(pasture_transport, pasture_from_transport)

pasture_transport<-get_totals(mcmodule=pasture_transport, mcnodes=c("pasture_to_transport_a_dir_contact_all","pasture_from_transport_a_dir_contact_all"),
                       trials_n="pasture_own_animals_n",
                       subsets_n="pasture_own_farm_n",
                       name = "a_dir_contact_all")

pasture_transport<-get_totals(mcmodule=pasture_transport, mcnodes=c("pasture_to_transport_a_indir_contact_all","pasture_from_transport_a_indir_contact_all"),
                       trials_n="pasture_own_animals_n",
                       subsets_n="pasture_own_farm_n",
                       name = "a_indir_contact_all")

pasture_transport<-get_totals(mcmodule=pasture_transport, mcnodes=c("pasture_to_transport_a_contact_all","pasture_from_transport_a_contact_all"),
                       trials_n="pasture_own_animals_n",
                       subsets_n="pasture_own_farm_n",
                       name = "a_contact_all")

pasture_transport<-get_agg_totals(mcmodule=pasture_transport, mcnode=c("a_contact_all"))

pasture_transport<-get_agg_totals(mcmodule=pasture_transport, mcnode=c("b_contact_all"))

pasture_transport<-add_prefix(pasture_transport)

#' 
#' ## Other farms module
#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_mix_data<-pasture_to_data%>%
  mutate(pregnant_p=0.5) #Does not affect risk but needed to calc origin_expression

if(admin_wif){

  #All farms sharing pastures test and remove positive animals before going to pasture
  pasture_mix_data<-wif_test(pasture_mix_data, scenario="Screening before pasture")
  
  pasture_mix_data<-wif_no_share_pasture(pasture_mix_data, scenario="No share pasture")


}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_expression<-c(pasture_farm_link=farm_link_expression,
                       pasture_farm_origin = origin_expression,
                       pasture_farm_test = test_expression)

pasture_herd<-eval_model_expression(model_expression = pasture_expression,
                                     data=pasture_mix_data)

pasture_herd<-get_totals(mcmodule=pasture_herd, mcnodes=c("a_inf","a_inf_pi","a_inf_tr"),
                 trials_n="pasture_other_animals_n",
                 subsets_n="pasture_other_farm_n")

pasture_herd<-get_totals(mcmodule=pasture_herd, mcnodes=c("a_no_detect","a_no_detect_pi","a_no_detect_tr"),
                 trials_n="pasture_other_animals_n",
                 subsets_n="pasture_other_farm_n")


pasture_herd<-get_agg_totals(mcmodule=pasture_herd, mcnode=c("a_inf_all"))

pasture_herd<-get_agg_totals(mcmodule=pasture_herd, mcnode=c("b_inf_all"))

pasture_herd<-get_agg_totals(mcmodule=pasture_herd, mcnode=c("a_no_detect_all"))

pasture_herd<-get_totals(mcmodule=pasture_herd, mcnode=c("a_no_detect_infectious"), all_mcnodes = FALSE,
                 trials_n="pasture_other_animals_n",
                 subsets_n="pasture_other_farm_n")

pasture_herd<-add_prefix(pasture_herd)

#' 
#' ## Animal mix module
#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
mix_expression<-c(pasture_mix_link=pasture_mix_link_expression,
                  pasture_mix_dir_contact = dir_contact_expression)

pasture_mix<-eval_model_expression(
  model_expression = mix_expression,
  prev_mcmodule = pasture_herd,
  data = pasture_mix_data)
  #create_nodes = FALSE)

pasture_mix<-get_totals(mcmodule=pasture_mix, mcnodes=c("a_dir_contact","a_dir_contact_pi"),
                trials_n="pasture_own_animals_n",
                subsets_n="pasture_own_farm_n")

pasture_mix<-get_agg_totals(mcmodule=pasture_mix, mcnode=c("a_dir_contact_all"))

pasture_mix<-get_agg_totals(mcmodule=pasture_mix, mcnode=c("b_dir_contact_all"))

pasture_mix<-add_prefix(pasture_mix)

#' 
#' ### Combine modules pasture herd, mix and transport
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture<-combine_modules(pasture_herd, pasture_mix)

pasture<-combine_modules(pasture_transport, pasture)


#By lot
pasture<-get_agg_totals(mcmodule=pasture,mcnode="pasture_transport_a_contact_all", keys_names=c("pathogen", "farm_id","scenario_id", "mov_id", "animal_category"), suffix="lot")

pasture<-get_agg_totals(mcmodule=pasture,mcnode="pasture_mix_a_dir_contact_all", keys_names=c("pathogen", "farm_id","scenario_id", "mov_id", "animal_category"), suffix="lot")

pasture<-at_least_one(mcmodule=pasture, mcnodes = c("pasture_transport_a_contact_all_lot","pasture_mix_a_dir_contact_all_lot"), name="pasture_a_livestocK_all_lot")

#By mov
pasture<-get_agg_totals(mcmodule=pasture,mcnode="pasture_transport_b_contact_all", keys_names=c("pathogen", "farm_id","scenario_id", "mov_id"), suffix="mov")

pasture<-get_agg_totals(mcmodule=pasture,mcnode="pasture_mix_b_dir_contact_all", keys_names=c("pathogen", "farm_id","scenario_id", "mov_id"), suffix="mov")

pasture<-at_least_one(mcmodule=pasture, mcnodes = c("pasture_transport_b_contact_all_mov","pasture_mix_b_dir_contact_all_mov"), name="pasture_b_livestocK_all_mov")

#By pathogen (all movements)
pasture<-at_least_one(mcmodule=pasture, mcnodes=c("pasture_transport_a_contact_all_agg","pasture_mix_a_dir_contact_all_agg"), name="pasture_a_livestocK_all_agg")

#By pathogen (all movements)
pasture<-at_least_one(mcmodule=pasture, mcnodes=c("pasture_transport_b_contact_all_agg","pasture_mix_b_dir_contact_all_agg"), name="pasture_b_livestock_all_agg")

#' 
#' ## Wildlife area effect module
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_wildlife_area_data<-pasture_region%>%
  left_join(tidy_panel(bsg, "pasture"))%>%
  left_join(tidy_prefix(mov, "pasture_risk_days",rm_prefix = FALSE))%>%
  mutate(surface="soil",
         access=TRUE,
         fencing_wildlife=TRUE,
         fencing_perimeter=FALSE)%>%
  left_join(pathogen_surface)%>%
  hjoin_region(wildlife_region)%>%
  hjoin_region(wildlife_pathogen_region)%>%
  left_join(wildlife_point_density)%>%
  hjoin_region(pathogen_region)%>%
  left_join(pathogen)%>%
  #Filter those that have wildlife prevalence
  filter(pathogen%in%
           unique(wildlife_pathogen_region$pathogen[wildlife_pathogen_region$wl_prev_mode>0]))

message("\npasture_wildlife_area_data (",paste(dim(pasture_wildlife_area_data), collapse=", "),") created")

#' 
#' ### Prepare what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #There are no admin what-ifs in this module, but we set up wif for compatibility
  pasture_wildlife_area_data<-set_up_wif(pasture_wildlife_area_data)
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
wildlife_area_expression<-c(
  pasture_wildlife_area_link = wildlife_area_link_expression,
  pasture_wildlife_area_inf=area_inf_expression)  

pasture_wildlife<-eval_model_expression(model_expression = wildlife_area_expression,                              data=pasture_wildlife_area_data,
                             param_names = c(risk_days="pasture_risk_days"))

pasture_wildlife<-get_agg_totals(mcmodule=pasture_wildlife, mcnode=c("area_inf"))

#By mov
pasture_wildlife<-get_agg_totals(mcmodule=pasture_wildlife,mcnode="area_inf", keys_names=c("pathogen", "farm_id","scenario_id", "mov_id"), suffix="mov")

pasture_wildlife<-add_prefix(pasture_wildlife)

#' 
#' ## Wildlife contact points module
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_wildlife_water_data<-pasture_wildlife_area_data%>%
  pivot_longer(
    cols = ends_with("_p")&!pasture_mud_p&!pasture_sum_p,
    names_to = "waterpoint_type",
    values_to = "point_p",
    values_drop_na = TRUE)%>%
  left_join(pasture_animal)%>%
  mutate(
    contact_point_type=sub("_p", "", waterpoint_type),
    mud_p=ifelse(contact_point_type%in%c("low", "high"),ifelse(pasture_mud,as.numeric(gsub("%", "", pasture_mud_p)),0),100)/100,
    contact_point_p=as.numeric(sub("%", "", point_p))/100,
    contact_point_type=gsub("pasture_","",contact_point_type)
  )

pasture_wildlife_water_data<-pasture_wildlife_water_data%>%
  left_join(animal_point)%>%
  left_join(wildlife_point)

message("\npasture_wildlife_water_data (",paste(dim(pasture_wildlife_water_data), collapse=", "),") created")

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------
if(admin_wif){
  #Avoid mud on wateres
  pasture_wildlife_water_data<-wif_no_mud(pasture_wildlife_water_data, scenario="Avoid mud on pasture wateres")
  pasture_wildlife_water_data<-wif_high_waterer(pasture_wildlife_water_data, scenario="Use high pasture wateres")
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------
wildlife_water_expression<-c(
  pasture_wildlife_link = wildlife_water_link_expression,
  pasture_wildlife_time = time_expression,
  pasture_wildlife_indir_contact=indir_contact_expression)

#debugonce(get_node_list)
pasture_wildlife_water<-eval_model_expression(
  model_expression = wildlife_water_expression,
  data=pasture_wildlife_water_data,
  param_names = c(risk_days="pasture_risk_days"))

pasture_wildlife_water<-get_totals(mcmodule=pasture_wildlife_water, mcnode=c("a_indir_contact"))

pasture_wildlife_water<-get_agg_totals(mcmodule=pasture_wildlife_water, mcnode=c("b_indir_contact"))

#By mov
pasture_wildlife_water<-get_agg_totals(mcmodule=pasture_wildlife_water,mcnode="b_indir_contact", keys_names=c("pathogen", "farm_id","scenario_id", "mov_id"), suffix="mov")

pasture_wildlife_water<-add_prefix(pasture_wildlife_water)

#' 
#' ## Combine modules wildlife area, waterpoints and pasture
#' 
## ------------------------------------------------------------------------------------------------------------------
pasture_wildlife<-combine_modules(pasture_wildlife, pasture_wildlife_water)

pasture_wildlife<-at_least_one(mcmodule=pasture_wildlife, mcnodes=c("pasture_wildlife_area_inf_agg","pasture_wildlife_water_b_indir_contact_agg"), name="pasture_wildlife_inf_agg")

pasture<-combine_modules(pasture, pasture_wildlife)

#All pasture modules
pasture<-at_least_one(mcmodule=pasture, mcnodes=c("pasture_b_livestock_all_agg","pasture_wildlife_inf_agg"), name="pasture_inf_agg")

#By movement
pasture<-at_least_one(mcmodule=pasture, mcnodes = c("pasture_wildlife_area_inf_mov","pasture_wildlife_water_b_indir_contact_mov"), name="pasture_wildlife_all_mov")

pasture<-at_least_one(mcmodule=pasture, mcnodes = c("pasture_b_livestocK_all_mov","pasture_wildlife_all_mov"), name="pasture_all_mov")

message("\nPASTURE PATHWAY OK!")

#' 
#' ## Save results
#' 
