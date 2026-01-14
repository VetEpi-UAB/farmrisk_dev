#' ---
#' title: "Movements pathway"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 

#' 
## ------------------------------------------------------------------------------------------------------------------------------------
message("\nMOVEMENTS PATHWAY: ")

#' 
#' ## Risk period
#' 
#' During **circular movements** (where animals leave and return to the farm), animals are exposed to external risks (at pasture, market, etc.) while away, but not to farm-specific risks. If a farm is performing circular movements, risk days are calculated as follows:
#' 
#' -   **One-off movements**: Risk days are calculated as the difference between departure date (`mov_out_date`) and arrival date (`mov_in_date`).
#' 
#' -   **Recurrent movements**: Risk days use the average number of days animals spend outside the farm (`recurrent_days`).
#' 
#' -   **Farm risk adjustment**: When most animals (\>80%) are away from the farm, farm risk is set to zero for that period. During other periods, the full census is used for risk calculations.
#' 
#' -   **Buffer days**: A tolerance of 5 days between consecutive movements to handle data entry issues or logistical details. Gaps or overlaps within this buffer are not treated as actual farm returns or simultaneous presence at multiple sites.
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
from_date <- mov$mov_out_date[!is.na(mov$mov_out_date)]
to_date <- mov$mov_in_date[!is.na(mov$mov_in_date)]
mov$recurrent_days <- ifelse(is.na(mov$recurrent_days), 1, mov$recurrent_days)
buffer_days <- 5

# Calculate risk days: recurrent movements use recurrent_days, others use date difference
mov$round_risk_days <- ifelse(grepl("recurrent", mov$mov_recurrent),
                              mov$recurrent_days,
                              as.numeric(difftime(
                                mov$mov_out_date, mov$mov_in_date, units = "days"
                              )))

#SKIPED AND UNDER REVISION
calc_risk_days=FALSE
if(calc_risk_days){
 # Determine if farm is empty (>80% animals outside)
mov$empty_farm <- {
  r_outside <- sum(tidy_animal_table()$animals_n) /
    sum(tidy_census_table()$animals_n)
  r_outside > 0.8
}

# Recalculate risk days considering empty farm periods
if (any(mov$empty_farm)) {
  if (sum(!is.na(mov$round_risk_days)) > 1) {
    # Handle multiple empty periods
    empty_periods <- data.frame(
      from_date = mov$mov_in_date[!is.na(mov$mov_in_date)&mov$empty_farm&(mov$mov_type%in%"round")],
      to_date = mov$mov_out_date[!is.na(mov$mov_out_date)&mov$empty_farm])
    
    # Calculate continuous risk periods
    risk_adjustment <- empty_periods %>%
      arrange(from_date) %>%
      mutate(
        gap = difftime(from_date, lag(to_date), units = "days"),
        is_continuous = gap <= buffer_days | is.na(gap)
      ) %>%
      filter(is_continuous) %>%
      summarise(total_days = sum(as.numeric(
        difftime(to_date, from_date, units = "days")
      )))
    
    risk_days <- risk_days - risk_adjustment$total_days
  } else {
    risk_days <- risk_days - mov$round_risk_days[!is.na(mov$round_risk_days)]
  }
} 
}


#' 
#' ## Movements data preparation
#' 
#' ### Check pathways
#' 
#' Create indicators of which type of movements are included in the current simulation.
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
exists_oneway<-any(mov$mov_type=="oneway")
exists_round<-any(mov$mov_type=="round")
exists_pasture<-any(mov$mov_origin=="pasture")

#' 
#' ### Animal Data Processing
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
# Process and aggregate animal movement data
mov_animal <- mov %>%
  tidy_animal_table() %>%
  left_join(get_columns(
    mov,
    c(
      "site_cattle_n",
      "mov_farms_n",
      "site_share",
      "site_bulls",
      "mov_type",
      "mov_origin",
      "mov_frequency"
    )
  )) %>%
  mutate(
    mov_round = mov_type == "round",
    site_share = ifelse(mov_round,!is.na(mov_farms_n)&mov_farms_n>0,site_share),
    mov_frequency = ifelse(is.na(mov_frequency), 1, mov_frequency),
    mov_own_animals_n = animals_n,
    mov_own_farm_n = ifelse(mov_round, 1, ifelse(is.na(mov_farms_n), 1, mov_farms_n)),
    mov_other_animals_n = site_cattle_n / mov_farms_n,
    mov_other_animals_n = ifelse(is.na(mov_other_animals_n), 0, mov_other_animals_n),
    mov_other_farm_n = ifelse(mov_round, ifelse(is.na(mov_farms_n), 0, mov_farms_n), 0)
  ) %>%
  left_join(animal) %>%
  mutate(livestock_units = animals_n * livestock_units)

# Process health and testing data
mov_status_origin <- tidy_status_table(pathway = "mov")
mov_test_origin <- tidy_test_table(pathway = "mov", module = "origin")
mov_test_quarantine <- tidy_test_table(pathway = "mov", module = "quarantine")

# Combine animal, health status, and test data
mov_health <- mov_animal %>%
  left_join(mov_status_origin, relationship = "many-to-many") %>%
  left_join(mutate(mov_test_origin, test_origin = test)) %>%
  left_join(mutate(mov_test_quarantine, test_quarantine = test)) %>%
  tidy_group() %>%
  mutate(
    health = paste(
      status,
      "status + ",
      ifelse(is.na(test_origin), "no", test_origin),
      "test origin + ",
      ifelse(is.na(test_quarantine), "no", test_quarantine),
      "test quarantine"
    ),
    health_status=status) %>%
  group_by(health,pathogen,mov_id)%>%
  mutate(health_id=paste0("health_",cur_group_id()))%>%
  ungroup()

mov_health_origin <- mov_health %>%
  mutate(test = test_origin,
         test_origin = NULL,
         test_quarantine = NULL)

mov_health_quarantine <- mov_health %>%
  mutate(test = test_quarantine,
         test_origin = NULL,
         test_quarantine = NULL)

#' 
#' ### Region Data Processing
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
mov_origin <- mov %>%
  tidy_prefix("mov_origin", rm_prefix = FALSE)

mov_region <- data.frame()

# Process site regional data
mov_region_site <- get_region_code(bsg, pathway = "farm") %>%
  mutate(mov_origin = "site")

# Combine site data if available
if ("site" %in% mov_origin$mov_origin) {
  mov_region_site <- mov_origin %>%
    inner_join(mov_region_site)
  
  mov_region <- bind_rows(mov_region, mov_region_site)
}

# Process pasture data
mov_pasture <- tidy_panel(panel = "pasture") %>%
  filter(!is.na(pasture_id)) %>%
  inner_join(mov_origin) %>%
  mutate(pasture_gid0 = bsg$farm_gid0, mov_origin = "pasture")

# Combine pasture data if available
if (nrow(mov_pasture) > 0) {
  mov_region_pasture <- get_region_code(mov_pasture) %>%
    inner_join(mov_pasture)
  
  if (nrow(mov_region) > 0) {
    mov_region <- bind_rows(mov_region, mov_region_pasture)
  } else{
    mov_region <- mov_region_pasture
  }
  
}

# Process remaining regional data
mov_region_other <- get_region_code(mov, pathway = "mov") %>%
  left_join(mov_origin)

if (nrow(mov_region_other) > 0) {
  mov_region_other <- mov_origin %>%
    inner_join(mov_region_other)
  
  if (nrow(mov_region) > 0) {
    mov_region <- bind_rows(mov_region, mov_region_other)
  } else{
    mov_region <- mov_region_other
  }
}

#' 
#' ### Vehicle Data Processing
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
# Process vehicle movement data for different directions
mov_to_veh <- mov %>%
  filter(mov_type == "round") %>%
  tidy_prefix("round_to_veh", rm_prefix = "round_to") %>%
  mutate(veh_direction = "round_to",
         veh_id = coalesce(as.factor(first_veh_id), as.factor(second_veh_id))) %>%
  select(-first_veh_id, -second_veh_id)

mov_from_veh <- mov %>%
  filter(mov_type == "round") %>%
  tidy_prefix("round_from_veh", rm_prefix = "round_from") %>%
  mutate(veh_direction = "round_from",
         veh_id = coalesce(as.factor(first_veh_id), as.factor(second_veh_id))) %>%
  select(-first_veh_id, -second_veh_id)

mov_oneway_veh <- mov %>%
  filter(mov_type == "oneway") %>%
  tidy_prefix("oneway_veh", rm_prefix = "oneway") %>%
  mutate(veh_direction = "oneway",
         veh_id = coalesce(as.factor(first_veh_id), as.factor(second_veh_id))) %>%
  select(-first_veh_id, -second_veh_id)

# Combine all vehicle data
if(nrow(mov_to_veh) > 0){
  mov_veh <- mov_to_veh %>%
  bind_rows(mov_from_veh)
  
  if (nrow(mov_oneway_veh) > 0) {
  mov_veh <- bind_rows(mov_veh, mov_oneway_veh)
  }
  
}else{
  mov_veh<-mov_oneway_veh
}

# Add vehicle panel data and remove duplicates
mov_veh <- mov_veh %>%
  left_join(tidy_prefix(tidy_panel(panel = "veh"), "veh", rm_prefix = FALSE),
            relationship = "many-to-many") %>%
  distinct()

#' 
#' ### Merge tidy user data with admin inputs
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
#Origin
mov_herd_health <- mov_animal %>%
  left_join(mov_region) %>%
  left_join(pathogen_animal, relationship = "many-to-many") %>%
  hjoin_region(pathogen_region, add_keys = "animal_category") %>%
  left_join(pathogen) %>%
  left_join(mov_health_origin) %>%
  left_join(pathogen_status)

#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
#Test
mov_test_origin <- mov_health_origin %>%
  left_join(pathogen_test)

#Used m2 in each movement
mov_m2_used <- mov_animal %>%
  left_join(mov_region, relationship = "many-to-many") %>%
  hjoin_region(animal_region, add_keys = "animal_category") %>%
  mutate(m2_used = veh_m2animal * animals_n) %>%
  group_by(farm_id, mov_id) %>%
  summarize(m2_used = sum(m2_used),
            animals_total = sum(animals_n))


#Estimate the number of animals from other farms that could fit in the vehicle
mov_veh_animal <- mov_veh %>%
  left_join(mov_m2_used) %>%
  left_join(mov_animal, relationship = "many-to-many") %>%
  left_join(mov_region) %>%
  hjoin_region(animal_region, add_keys = "animal_category") %>%
  mutate(veh_size = ifelse(is.na(veh_size), veh_size_ext, veh_size),
         m2_free = veh_size - m2_used) %>%
  mutate(
    animals_p = animals_n / animals_total,
    other_all_animals_n = (animals_p * m2_free) / veh_m2animal,
    other_all_animals_n = ifelse(other_all_animals_n > 0, other_all_animals_n, 2),
    prev_all_animals_n = veh_size / veh_m2animal,
    surface = ifelse(is.na(veh_cleaning),"soil",ifelse(veh_cleaning == "each", "metal", "soil"))
  )

#' 
#' #### Merge all
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
#All data
mov_data <- mov_herd_health %>%
  left_join(mov_test_origin) %>%
  mutate(animals_p = NULL) #desambiguate

mov_data <- mov_data %>%
  select(setdiff(names(mov_data), names(mov_veh_animal)) |
           matches("mov_id|farm_id|animal_category|veh_id")) %>% #desambiguate
  left_join(mov_veh_animal, relationship = "many-to-many") %>%
  left_join(pathogen_surface)%>%
  mutate(pasture_mud_p = NULL) #Removed because it generates harmless warnings

#' 
#' #### Separate data by transport (first transport: oneway or round to, second transport: round from)
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#Separate data by vehicle
mov_first_data <- filter(mov_data, veh_direction == "round_to" |
                           veh_direction == "oneway")

message("\nmov_first_data (", paste(dim(mov_first_data), collapse = ", "), ") created")

mov_second_data <- filter(mov_data,veh_direction == "round_from")

message("\nmov_first_data (", paste(dim(mov_second_data), collapse = ", "), ") created")

#' 
#' #### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif) {
  #Not share vehicle
  mov_first_data <- wif_no_share_veh(mov_first_data, scenario = "No shared transport")
  
  mov_second_data <- wif_no_share_veh(mov_second_data, scenario = "No shared transport")
  
  #Own vehicle
  mov_first_data <- wif_no_share_veh(mov_first_data, scenario = "Use, and not share, your own vehicle", prev_wif ="wif_own_veh")
  
  mov_second_data <- wif_no_share_veh(mov_second_data, scenario = "Use, and not share, your own vehicle", prev_wif ="wif_own_veh")
  
  #Clean the vehicle
  mov_first_data <- wif_clean(mov_first_data, veh_type=="own", scenario = "Clean and disinfect the vehicle between transports")
  
  mov_second_data <- wif_clean(mov_second_data, veh_type=="own", scenario = "Clean and disinfect the vehicle between transports")
  
}

#' 
#' ## Other herds
#' 
#' ### Prepare data
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
mov_herd_data <- mov_first_data %>%
  select(-starts_with("veh_"))

message("\nmov_herd_data (", paste(dim(mov_herd_data), collapse = ", "), ") created")

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif) {
  #Test farms (origin or other herds sharing sites)
  #Screening herds before sharing sites (round)
  mov_herd_data <- wif_test(mov_herd_data, mov_round == TRUE & site_share==TRUE, scenario =
                              "Screening herds sharing sites")
  
  #Test  before transport (one-way)
  mov_herd_data <- wif_test(mov_herd_data, mov_round == FALSE, scenario =
                              "Test before transport")
  
  #Not share sites with other herds
  mov_herd_data <- wif_no_share_site(mov_herd_data, mov_round == TRUE & site_share == TRUE, scenario =
                                       "No sharing sites")
  
}

#' 
#' ### Match
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
mov_first_keys<-c("farm_id","mov_id", "first_veh_id", "animal_category","pathogen","health_id")

mov_first_data <-mcmodule::wif_match(
  x = mov_first_data, 
  y = mov_herd_data, 
  by=mov_first_keys)[[1]]

mov_second_keys<-c("farm_id","mov_id", "second_veh_id", "animal_category","pathogen","health_id")

# Only those movements which have a second transport
mov_herd_return_data<-mov_herd_data[mov_herd_data$mov_id%in%mov_second_data$mov_id,]

mov_second_data <-mcmodule::wif_match(
  x = mov_second_data,
  y = mov_herd_return_data, 
  by=mov_second_keys)[[1]]

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
mov_herd_exp <- c(
  mov_herd_link = mov_herd_link_exp,
  mov_herd_herd_select = herd_select_exp,
  mov_herd_animal_select = animal_select_exp,
  mov_herd_test = test_exp,
  mov_mov_type_link = mov_type_link_exp
)

mov_herd <- mcmodule::eval_module(
  exp = mov_herd_exp,
  data = mov_herd_data,
  keys = "health_id") #extra keys not included in cattle_data_keys

# Keys to aggregate by to account for same probability of herd being infected by all animal categories from the same herd (Multiple group multilevel trials)
agg_herd_keys<- c("pathogen","scenario_id", "farm_id", "mov_id", "health_id")
agg_mov_keys<- c("pathogen","scenario_id", "farm_id", "mov_id")
agg_totals_keys<- c("pathogen","scenario_id", "farm_id")
levels_herd<-c(trial="oneanimal",subset="oneherd",set="allherds")

#' 
#' #### Totals for one-way movements: origin
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#At least one animal from at least one of the origin farms is infected (by health_id, and mov_id, scenario_id and pathogen, for 1 movement)
mov_herd <-mcmodule::trial_totals(
  mcmodule = mov_herd,
  mc_names = c(
    "own_no_detect",
    "own_no_detect_pi",
    "own_no_detect_tr"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "farms_n",
  agg_keys = agg_herd_keys,
  subsets_p = "herd_inf",
  level_suffix = levels_herd
)

#At least one animal from at least one of the origin farms is infected (by mov_id, scenario_id and pathogen, for 1 movement)
mov_herd <-mcmodule::agg_totals(
  mcmodule = mov_herd,
  mc_name = "own_no_detect_all_hag_allherds",
  agg_keys = agg_mov_keys,
  agg_suffix="mov")

# Per risk_days (year)
#At least one animal from at least one of the origin farms is infected (by health_id, and mov_id, scenario_id and pathogen, for all movements in risk_days)
mov_herd <- mcmodule::trial_totals(
  mcmodule = mov_herd,
  mc_names = "own_no_detect_all_hag_allherds",
  trials_n = "mov_frequency",
  agg_keys = agg_herd_keys,
  agg_suffix = "",
  level_suffix = c(set="year")
)

#At least one animal from at least one of the origin farms is infected (by scenario_id and pathogen, for all movements in risk_days)
mov_herd <- mcmodule::agg_totals(
  mcmodule = mov_herd,
  mc_name = "own_no_detect_all_hag_allherds_year",
  agg_keys = agg_totals_keys
)

#' 
#' #### Totals for round movements: animal mix
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
# Contact not detected infectious animals (infectious during animal mix)
mov_herd <-mcmodule::trial_totals(
  mcmodule = mov_herd,
  mc_names = "other_no_detect_infectious",
  trials_n = "contacts_n",
  #subsets_n = "farms_n",
  agg_keys = agg_herd_keys,
  #subsets_p = "herd_inf", #This is added in the last step
  keep_variates = TRUE,
  level_suffix = c(set="otherherd")
)

# Contact not detected BVD PI animals (infectious during animal mix)
mov_herd <-mcmodule::trial_totals(
  mcmodule = mov_herd,
  mc_names = "other_no_detect_pi",
  trials_n = "contacts_n",
  #subsets_n = "farms_n",
  agg_keys = agg_herd_keys,
  #subsets_p = "herd_inf", #This is added in the last step
  keep_variates = TRUE,
  level_suffix = c(set="otherherd")
)

mov_herd <- mcmodule::add_prefix(mov_herd)

#' 
#' ## Animal mix module
#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(exists_round) {
  mix_exp <- c(mov_mix_herd_select = herd_select_exp,
               mov_mix_link = mix_link_exp,
               mov_mix_dir_contact = dir_contact_exp)
  
  mov_mix <- mcmodule::eval_module(
    exp = mix_exp,
    prev_mcmodule = mov_herd,
    data =mov_herd_data,
    keys = "health_id") #extra keys not included in cattle_data_keys
  
  #At least one of your animals is infected by direct contact during animal mix (by health_id, and mov_id, scenario_id and pathogen, for 1 movement)
  mov_mix <-mcmodule::trial_totals(
    mcmodule = mov_mix,
    mc_names = c("dir_contact","dir_contact_pi"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_other_farm_n", #number of other farms
    subsets_p = "herd_inf",
    agg_keys = agg_herd_keys,
    level_suffix = levels_herd
  )

  # Per risk_days (year)
  #At least one of your animals is infected by direct contact during animal mix in at least one movement (by health_id, and mov_id, scenario_id and pathogen, for all movements in risk_days)
  mov_mix <-mcmodule::trial_totals(
    mcmodule = mov_mix,
    mc_names = "dir_contact_all_hag_allherds",
    trials_n = "mov_frequency",
    agg_keys = agg_herd_keys,
    agg_suffix = "",
    level_suffix = c(set="year")
  )
  
  #At least one of your animals is infected by direct contact during animal mix in at least one movement (total by pathogen and scenario_id, for all movements in risk_days)
  mov_mix <- mcmodule::agg_totals(
    mcmodule = mov_mix,
    mc_name = c("dir_contact_all_hag_allherds_year"),
    agg_keys = agg_totals_keys
  )

  mov_mix <- mcmodule::add_prefix(mov_mix)
  
}

#' 
#' ### Combine modules herd and mix
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(exists_round) {
  movement <- mcmodule::combine_modules(mov_herd, mov_mix)
  
  # Infected by livestock during movements (either animal entries or direct contact in shared site)
  movement <- mcmodule::at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_herd_own_no_detect_all_hag_allherds",
      "mov_mix_dir_contact_all_hag_allherds"
    ),
    name = "mov_livestock_all_hag"
  )
  
  #For further multi-level calculations
  movement <- mcmodule::at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_herd_own_no_detect",
      "mov_mix_dir_contact_all"
    ),
    name = "mov_livestock_no_detect",
    prefix = "mov"
  )
  

} else{
  movement <- mov_herd
  
  # Infected by livestock during movements (only animal entries)
  movement$node_list$mov_livestock_all_hag <-
    movement$node_list$mov_herd_own_no_detect_all_hag_allherds
  
  #For further multi-level calculations
  movement$node_list$mov_livestock_no_detect <-
    movement$node_list$mov_herd_own_no_detect

}

#By mov
#At least one animal is infected by livestock during movements by movement
movement <- mcmodule::agg_totals(
  mcmodule = movement,
  mc_name = c("mov_livestock_all_hag"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov"
)

#By risk_days (year)
#At least one animal is infected by livestock during movements by year by movement
movement <- mcmodule::trial_totals(
  mcmodule = movement,
  mc_names = c("mov_livestock_all_hag"),
  trials_n = "mov_frequency",
  agg_keys = agg_mov_keys,
  agg_suffix = "",
  level_suffix = c(set="year")
)

#At least one animal is infected by livestock during movements by year aggregated by scenario and pathogen
movement <- mcmodule::agg_totals(
  mcmodule = movement,
  mc_name = c("mov_livestock_all_hag_year"),
  agg_keys = agg_totals_keys
)

#' 
#' ## Unknown farm module
#' 
#' ### Prepare data
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
#Other origin
mov_unk_farm_data <- mov_animal %>%
  left_join(mov_region) %>%
  left_join(pathogen_animal, relationship = "many-to-many") %>%
  left_join(pathogen, relationship="many-to-many") %>%
  hjoin_region(pathogen_region, add_keys = "animal_category") %>%
  mutate(health_status = "unk") %>%
  left_join(pathogen_status) %>%
  distinct()

#Add vehicle information
mov_unk_farm_data <- mov_unk_farm_data %>%
  select(setdiff(names(mov_unk_farm_data), names(mov_veh_animal)) |
           matches("mov_id|farm_id|animal_category")) %>% #desambiguate
  left_join(mov_veh_animal, relationship = "many-to-many") %>%
  mutate(pregnant_p = 0.5) #Does not affect risk but needed to calc origin_exp

#Separate data by transport (direction)
#First vehicle one way
mov_first_unk_origin_data <- filter(mov_unk_farm_data,
                                    veh_direction == "round_to" | veh_direction == "oneway")

message("\nmov_first_unk_origin_data (",
        paste(dim(mov_first_unk_origin_data), collapse = ", "),
        ") created")

#Second vehicle
mov_second_unk_origin_data <- filter(mov_unk_farm_data,
                                     veh_direction == "round_from" | veh_direction == "oneway") %>%
  mutate(plan_sensi = ifelse(veh_direction == "oneway", 1, plan_sensi)) #we keep all rows for compatibility but only calculate for round movements


message("\nmov_to_unk_origin_data (",
        paste(dim(mov_second_unk_origin_data), collapse = ", "),
        ") created")

#' 
#' ### Match
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#By unknwon origin keys
unk_farm_keys<- c("pathogen","scenario_id", "farm_id", "mov_id", "animal_category")
agg_unk_farm_keys<-c("pathogen","scenario_id", "farm_id", "mov_id")

mov_first_unk_origin_data <-mcmodule::wif_match(
  x = mov_first_unk_origin_data, 
  y = mov_first_data, 
  by=unk_farm_keys)[[1]]


# Only those movements which have a second transport
mov_second_unk_origin_return_data<-
  mov_second_unk_origin_data[mov_second_unk_origin_data$mov_id%in%mov_second_data$mov_id,]

mov_second_unk_origin_data <-mcmodule::wif_match(
  x = mov_second_unk_origin_return_data, 
  y = mov_second_data, 
  by=unk_farm_keys)[[1]]

#' 
#' ### Evaluate first transport unknown origin
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
unk_origin_exp <- c(mov_unk_link = unk_link_exp,
                    mov_unk_herd_select = herd_select_exp,
                    mov_unk_animal_select = animal_select_exp)

mov_first_unk_farm <- mcmodule::eval_module(
  exp = unk_origin_exp,
  data = mov_first_unk_origin_data)
#At least one of animal from at least one of the other farms in the vehicle (same time) is...

# infectious
mov_first_unk_farm <- mcmodule::trial_totals(
  mcmodule = mov_first_unk_farm,
  mc_names = c("infectious"),
  trials_n = "other_animals_n",
  subsets_n = "veh_farms_from_n",
  subsets_p = "herd_inf",
  agg_keys = agg_unk_farm_keys,
  keep_variates = TRUE,
  level_suffix = levels_herd,
  name = "other_infectious")

# BVD PI
mov_first_unk_farm <- mcmodule::trial_totals(
  mcmodule = mov_first_unk_farm,
  mc_names = c("infected_pi"),
  trials_n = "other_animals_n",
  subsets_n = "veh_farms_from_n",
  subsets_p = "herd_inf",
  agg_keys = agg_unk_farm_keys,
  keep_variates = TRUE,
  level_suffix = levels_herd,
  name = "other_pi")

# At least one of animal from at least one of the previous farms im the vehicle (different time) is...

# infectious 
mov_first_unk_farm <- mcmodule::trial_totals(
  mcmodule = mov_first_unk_farm,
  mc_names = c("infectious"),
  trials_n = "prev_animals_n",
  subsets_n = "veh_farms_from_n",
  subsets_p = "herd_inf",
  agg_keys = agg_unk_farm_keys,
  keep_variates = TRUE,
  level_suffix = levels_herd,
  name = "prev_infectious")

# BVD PI
mov_first_unk_farm <- mcmodule::trial_totals(
  mcmodule = mov_first_unk_farm,
  mc_names = c("infected_pi"),
  trials_n = "prev_animals_n",
  subsets_n = "veh_farms_from_n",
  subsets_p = "herd_inf",
  agg_keys = agg_unk_farm_keys,
  keep_variates = TRUE,
  level_suffix = levels_herd,
  name = "prev_pi")

mov_first_unk_farm <- mcmodule::add_prefix(mov_first_unk_farm)

mov_herd <- mcmodule::combine_modules(mov_herd, mov_first_unk_farm)

#' 
#' ### Evaluate second transport unknown origin
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#Check if trasnport to and from are equal
from_equal_to <- all(
  mov_first_data[mov_first_data$mov_id%in%mov_second_data$mov_id, 
                 !names(mov_first_data) %in% c("veh_direction", "hg")] ==
  mov_second_data[, !names(mov_second_data) %in% c("veh_direction", "hg")], na.rm =
                       TRUE)

#If it they are not not equal calcualate unk farm "from" module
if (!from_equal_to) {
  mov_second_unk_farm <- mcmodule::eval_module(
    exp = unk_origin_exp, 
    data = mov_second_unk_origin_data)
  
  #At least one of animal from at least one of the other farms in the vehicle (same time) is...
  
  # infectious
  mov_second_unk_farm <- mcmodule::trial_totals(
    mcmodule = mov_second_unk_farm,
    mc_names = c("infectious"),
    trials_n = "other_animals_n",
    subsets_n = "veh_farms_from_n",
    subsets_p = "herd_inf",
    agg_keys = agg_unk_farm_keys,
    keep_variates = TRUE,
    level_suffix = levels_herd,
    name = "other_infectious")
  
  # BVD PI
  mov_second_unk_farm <- mcmodule::trial_totals(
    mcmodule = mov_second_unk_farm,
    mc_names = c("infected_pi"),
    trials_n = "other_animals_n",
    subsets_n = "veh_farms_from_n",
    subsets_p = "herd_inf",
    agg_keys = agg_unk_farm_keys,
    keep_variates = TRUE,
    level_suffix = levels_herd,
    name = "other_pi")
  
  # At least one of animal from at least one of the previous farms im the vehicle (different time) is...
  
  # infectious 
  mov_second_unk_farm <- mcmodule::trial_totals(
    mcmodule = mov_second_unk_farm,
    mc_names = c("infectious"),
    trials_n = "prev_animals_n",
    subsets_n = "veh_farms_from_n",
    subsets_p = "herd_inf",
    agg_keys = agg_unk_farm_keys,
    keep_variates = TRUE,
    level_suffix = levels_herd,
    name = "prev_infectious")
  
  # BVD PI
  mov_second_unk_farm <- mcmodule::trial_totals(
    mcmodule = mov_second_unk_farm,
    mc_names = c("infected_pi"),
    trials_n = "prev_animals_n",
    subsets_n = "veh_farms_from_n",
    subsets_p = "herd_inf",
    agg_keys = agg_unk_farm_keys,
    keep_variates = TRUE,
    level_suffix = levels_herd,
    name = "prev_pi")
  
  mov_second_unk_farm <- mcmodule::add_prefix(mov_second_unk_farm)
  
}

#' 
#' ## Transport module
#' 
#' ### Evaluate first transport
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
transport_exp <- c(
  mov_transport_link = transport_link_exp,
  mov_transport_dir_contact = dir_contact_exp,
  mov_transport_indir_contact = indir_contact_exp
)

mov_transport_first <- mcmodule::eval_module(
  exp = transport_exp,
  prev_mcmodule = mov_first_unk_farm,
  data = mov_first_data,
  keys = c("health_id"))

# The following calculations are not multilevel as infection probability at origin is already accounted for in both purchased animals and others in the vehicle

# Probability of at least one purchased animal being infected during first transport by DIRECT contact
mov_transport_first <- mcmodule::trial_totals(
  mcmodule = mov_transport_first,
  mc_names = c("dir_contact", "dir_contact_pi"),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  level_suffix = levels_herd
)

# Probability of at least one purchased animal being infected during first transport by INDIRECT contact
mov_transport_first <- mcmodule::trial_totals(
  mcmodule = mov_transport_first,
  mc_names = c("indir_contact", "indir_contact_pi"),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  level_suffix = levels_herd
)

# Probability of at least one purchased animal being infected during first transport
mov_transport_first <- mcmodule::trial_totals(
  mcmodule = mov_transport_first,
  mc_names = c("dir_contact_all", "indir_contact_all"),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "contact_all",
  level_suffix = levels_herd
)

# Aggregated probability of one purchased animal being infected during first transport, by pathogen and scenario_id
mov_transport_first <- mcmodule::agg_totals(
  mcmodule = mov_transport_first,
  mc_name =c("contact_all"),
  agg_keys = agg_totals_keys
)

# Aggregated probability of at least one purchased animal being infected during first transport, by pathogen and scenario_id
mov_transport_first <- mcmodule::agg_totals(
  mcmodule = mov_transport_first, 
  mc_name =c("contact_all_allherds"),
  agg_keys = agg_totals_keys
)

mov_transport_first <- mcmodule::add_prefix(mov_transport_first)

#' 
#' ### Evaluate second transport
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#If it is equal copy of mov_transport_first
if (from_equal_to) {
  mov_transport_second <- mcmodule::add_prefix(mov_transport_first,
                                     prefix = "mov_transport_second",
                                     rewrite_module = "mov_transport_first")
  message("\nmov_transport_second mcmodule copied from mov_transport_first")
  #Else calculate mov_transport_second
} else{
  mov_transport_second <- mcmodule::eval_module(
    exp = transport_exp,
    prev_mcmodule = mov_second_unk_farm,
    data = mov_second_data,
    keys = c("health_id"))
  
  # The following calculations are not multilevel as infection probability at origin is already accounted for in both purchased animals and others in the vehicle

  # Probability of at least one purchased animal being infected during second transport by DIRECT contact
  mov_transport_second <- mcmodule::trial_totals(
    mcmodule = mov_transport_second,
    mc_names = c("dir_contact", "dir_contact_pi"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n",
    level_suffix = levels_herd
  )
  
  # Probability of at least one purchased animal being infected during second transport by INDIRECT contact
  mov_transport_second <- mcmodule::trial_totals(
    mcmodule = mov_transport_second,
    mc_names = c("indir_contact", "indir_contact_pi"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n",
    level_suffix = levels_herd
  )
  
  # Probability of at least one purchased animal being infected during second transport
  mov_transport_second <- mcmodule::trial_totals(
    mcmodule = mov_transport_second,
    mc_names = c("dir_contact_all", "indir_contact_all"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n",
    name = "contact_all",
    level_suffix = levels_herd
  )
  
  # Aggregated probability of one purchased animal being infected during second transport, by pathogen and scenario_id
  mov_transport_second <- mcmodule::agg_totals(
    mcmodule = mov_transport_second,
    mc_name =c("contact_all"),
    agg_keys = agg_totals_keys
  )
  
  # Aggregated probability of at least one purchased animal being infected during second transport, by pathogen and scenario_id
  mov_transport_second <- mcmodule::agg_totals(
    mcmodule = mov_transport_second, 
    mc_name =c("contact_all_allherds"),
    agg_keys = agg_totals_keys
  )
  
  mov_transport_second <- mcmodule::add_prefix(mov_transport_second)
}

#' 
#' ### Combine modules first transport and second transport
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
mov_transport <- combine_modules(mov_transport_first, mov_transport_second)

# Probability of at least one purchased animal being infected during transport by DIRECT contact
mov_transport <- mcmodule::trial_totals(
  mcmodule = mov_transport,
  mc_names = c(
    "mov_transport_first_dir_contact_all",
    "mov_transport_second_dir_contact_all"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "dir_contact_all",
  level_suffix = levels_herd,
  data_name = "mov_first_data"
)

# Probability of at least one purchased animal being infected during transport by INDIRECT contact
mov_transport <- mcmodule::trial_totals(
  mcmodule = mov_transport,
  mc_names = c(
    "mov_transport_first_indir_contact_all",
    "mov_transport_second_indir_contact_all"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "indir_contact_all",
  level_suffix = levels_herd,
  data_name = "mov_first_data"
)

# Probability of at least one purchased animal being infected during transport
mov_transport <- mcmodule::trial_totals(
  mcmodule = mov_transport,
  mc_names = c(
    "mov_transport_first_contact_all",
    "mov_transport_second_contact_all"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "contact_all",
  level_suffix = levels_herd,
  data_name = "mov_first_data"
)

# Aggregated probability of at least one purchased animal being infected during transport, by mov_id, pathogen and scenario_id
mov_transport <- mcmodule::agg_totals(
  mcmodule = mov_transport,
  mc_name =c("contact_all_allherds"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov"
)

# Annual probability of at least one purchased animal being infected during transport, by health_id, mov_id, pathogen and scenario_id
mov_transport <- mcmodule::trial_totals(
  mcmodule = mov_transport, 
  mc_name =c("contact_all_allherds"),
  trials_n = "mov_frequency",
  subsets_n = NULL,
  agg_keys = agg_herd_keys,
  level_suffix = c(set="year"),
  data_name = "mov_first_data"
)

# Aggregated annual probability of at least one purchased animal being infected during transport, by mov_id, pathogen and scenario_id
mov_transport <- mcmodule::agg_totals(
  mcmodule = mov_transport,
  mc_name =c("contact_all_allherds_hag_year"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov"
)

# Aggregated annual probability of at least one purchased animal being infected during transport, by pathogen and scenario_id
mov_transport <- mcmodule::agg_totals(
  mcmodule = mov_transport,
  mc_name =c("contact_all_allherds_hag_year"),
  agg_keys = agg_totals_keys
)

mov_transport <- mcmodule::add_prefix(mov_transport)

#' 
#' ### Combine modules transport and livestock
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
movement <- mcmodule::combine_modules(movement, mov_transport)

# Aggregated probability of at least one purchased animal being infected upon farm arrival, by health_id, mov_id, pathogen and scenario_id
movement <- mcmodule::at_least_one(
  mcmodule = movement,
  mc_names = c("mov_transport_contact_all_allherds_hag", "mov_livestock_all_hag"),
  name = "mov_inf_all_hag",
  prefix = "mov"
)

# Aggregated probability of at least one purchased animal being infected upon farm arrival, by mov_id, pathogen and scenario_id
movement <- mcmodule::at_least_one(
  mcmodule = movement,
  mc_names = c("mov_transport_contact_all_allherds_mov", "mov_livestock_all_hag_mov"),
  name = "mov_inf_all_mov",
  prefix = "mov"
)

# Aggregated annual probability of at least one purchased animal being infected upon farm arrival, by pathogen and scenario_id
movement <- mcmodule::at_least_one(
  mcmodule = movement,
  mc_names = c("mov_transport_contact_all_allherds_hag_year_agg", "mov_livestock_all_hag_year_agg"),
  name = "mov_inf_all_hag_year_agg",
  prefix = "mov"
)

#For further single level calculations
#if (exists_round) {
#  movement <- mcmodule::at_least_one(
#    mcmodule = movement,
#    mc_names = c(
#      "mov_transport_contact_all",
#      "mov_mix_dir_contact_all"
#    ),
#    name = "mov_contact_all",
#    prefix = "mov"
#  )
#} else{
#  movement$node_list$mov_contact_all <- #movement$node_list$mov_transport_contact_all
#}

#' 
#' ## Quarantine module
#' 
#' ### Prepare test data
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
#Tidy diagnostic tests before transport by disease, farm and type of test
mov_test_quarantine <- mov_health_quarantine %>%
  left_join(pathogen_test)

#Tidy quarantine parameters
quarantine_bsg_data <- tidy_prefix(bsg, module = "quarantine", rm_prefix = FALSE)

#Add movement and tests info
mov_quarantine_test_data <- mov_herd_data[, !names(mov_herd_data) %in% "test"]%>%
  left_join(quarantine_bsg_data) %>%
  left_join(tidy_prefix(mov, "quarantine", "mov")) %>%
  left_join(mov_test_quarantine) %>%
  select(-contains("veh")) %>%
  left_join(pathogen_test) %>%
  mutate(
    quarantine_test_time = test_time,
    surface = ifelse(quarantine_cleaning == "each", "metal", "soil"),
    quarantine = mov_quarantine
  ) %>%
  left_join(pathogen_surface)

message("\nquarantine_data (", paste(dim(mov_quarantine_test_data), collapse =
                                       ", "), ") created")

#' 
## ------------------------------------------------------------------------------------------------------------------------------------
quarantine_keys<-c("farm_id","mov_id", "animal_category","pathogen","health_id")

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif) {
  #Match index
  mov_quarantine_test_data <-mcmodule::wif_match(x = mov_quarantine_test_data, y = mov_herd_data, by=quarantine_keys)[[1]]
  
  #Clean quarantine equipement
  mov_quarantine_test_data <- wif_quarantine(mov_quarantine_test_data, scenario = "Isolate new animals")
  
  #Test  during quarantine
  mov_quarantine_test_data <- wif_test(mov_quarantine_test_data,
                                  scenario = "Test during quarantine",
                                  prev_wif = "wif_quarantine")
  
  #Clean quarantine equipement
  mov_quarantine_test_data <- wif_clean(mov_quarantine_test_data,
                                   scenario = "Clean and disinfect quarantine equipment")
  
  #Exclusive quarantine material
  mov_quarantine_test_data <- wif_exclusive_material(mov_quarantine_test_data,
                                                scenario = "Exclusive quarantine material")
  
  #Test during quarantine with exclusive material
  mov_quarantine_test_data <- combine_wif(
    data = mov_quarantine_test_data,
    scenario = "Test during quarantine with exclusive material",
    wif_list = c("quarantine", "exclusive_material", "test")
  )
  
}

#' 
#' ### Evaluate test multilevel
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
quarantine_test_multilevel_exp <- c(
  mov_quarantine_herd_select = herd_select_exp,
  mov_quarantine_test_link = quarantine_test_link_exp,
  mov_quarantine_test = test_exp)

mov_quarantine_test_multilevel <- mcmodule::eval_module(
  exp = quarantine_test_multilevel_exp,
  prev_mcmodule = movement,
  param_names = c(
    infected = "mov_livestock_no_detect",
    infected_pi = "mov_herd_own_no_detect_pi",
    infected_tr = "mov_herd_own_no_detect_tr"
  ),
  data = mov_quarantine_test_data,
  match_keys = quarantine_keys
  )

#At least one animal infected in origin is not detected during quarantine (by health_id, mov_id, scenario_id and pathogen, for 1 movement)
mov_quarantine_test_multilevel <-mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_names = c(
    "no_detect",
    "no_detect_pi",
    "no_detect_tr"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "farms_n",
  agg_keys = agg_herd_keys,
  subsets_p = "herd_inf",
  level_suffix = levels_herd
)

#At least one animal infected in origin is not detected during quarantine (by mov_id, scenario_id and pathogen, for 1 movement)
mov_quarantine_test_multilevel <-mcmodule::agg_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = "no_detect_all_hag_allherds",
  agg_keys = agg_mov_keys,
  agg_suffix="mov")

# Per risk_days (year)
#At least one animal infected in origin is not detected during quarantine (by health_id, and mov_id, scenario_id and pathogen, for all movements in risk_days)
mov_quarantine_test_multilevel <- mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_names = "no_detect_all_hag_allherds",
  trials_n = "mov_frequency",
  agg_keys = agg_herd_keys,
  agg_suffix = "",
  level_suffix = c(set="year")
)

mov_quarantine_test_multilevel <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = "no_detect_all_hag_allherds_year",
  agg_keys = agg_totals_keys
)

# Detected infectious infected in origin animals (infectious during quarantine + removed)
mov_quarantine_test_multilevel <-mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_names = "detect_infectious",
  trials_n = "mov_own_animals_n",
  subsets_n = "farms_n",
  agg_keys = agg_herd_keys,
  subsets_p = "herd_inf",
  level_suffix = levels_herd
)

mov_quarantine_test_multilevel <-mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_names = "detect_pi",
  trials_n = "mov_own_animals_n",
  subsets_n = "farms_n",
  agg_keys = agg_herd_keys,
  subsets_p = "herd_inf",
  level_suffix = levels_herd
)

mov_quarantine_test_multilevel <- add_prefix(mov_quarantine_test_multilevel)

#' 
#' ### Evaluate test single level
#' 
#' Not that the multi-level prior "herd_inf" for "mov_transport_contact_all" is calculated at the unk_farm level, but it is single-level for own animals (assuming only one farm that is free of infection).
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
quarantine_test_single_level_exp <- c(
  mov_quarantine_test_link = quarantine_test_link_exp,
  mov_quarantine_test = test_exp)

mov_quarantine_test_single_level <- mcmodule::eval_module(
  exp = quarantine_test_single_level_exp,
  prev_mcmodule = movement,
  param_names = c(
    infected = "mov_transport_contact_all",
    infected_pi = "mov_herd_no_risk",
    infected_tr = "mov_herd_no_risk"
  ),
  data = mov_quarantine_test_data,
  match_keys = quarantine_keys
)

#At least one animal infected during movement is not detected during quarantine (by health_id, mov_id, scenario_id and pathogen, for 1 movement)
mov_quarantine_test_single_level <-mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_names = c(
    "no_detect",
    "no_detect_pi",
    "no_detect_tr"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  agg_keys = agg_herd_keys,
  level_suffix = levels_herd
)

#At least one animal infected during movement is not detected during quarantine (by mov_id, scenario_id and pathogen, for 1 movement)
mov_quarantine_test_single_level <-mcmodule::agg_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_name = "no_detect_all_hag_allherds",
  agg_keys = agg_mov_keys,
  agg_suffix="mov")

# Per risk_days (year)
#At least one animal infected during movement is not detected during quarantine (by health_id, and mov_id, scenario_id and pathogen, for all movements in risk_days)
mov_quarantine_test_single_level <- mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_names = "no_detect_all_hag_allherds",
  trials_n = "mov_frequency",
  agg_keys = agg_herd_keys,
  agg_suffix = "",
  level_suffix = c(set="year")
)

mov_quarantine_test_single_level <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_name = "no_detect_all_hag_allherds_year",
  agg_keys = agg_totals_keys
)

# Detected infectious infected during movement animals (infectious during quarantine + removed)
mov_quarantine_test_single_level <-mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_names = "detect_infectious",
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  agg_keys = agg_herd_keys,
  level_suffix = levels_herd
)

mov_quarantine_test_single_level <-mcmodule::trial_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_names = "detect_pi",
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  agg_keys = agg_herd_keys,
  level_suffix = levels_herd
)

mov_quarantine_test_single_level <- add_prefix(mov_quarantine_test_single_level)

#' 
#' ### Combine modules quarantine test single level and multilevel
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
mov_quarantine_test <- mcmodule::combine_modules(mov_quarantine_test_single_level,
                                       mov_quarantine_test_multilevel)

# Hag level NOT detected infectious animal
mov_quarantine_test <- mcmodule::at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_no_detect_all_hag",
    "mov_quarantine_test_single_level_no_detect_all_hag"
  ),
  name = "mov_quarantine_test_no_detect_all_hag",
)

# At least one NOT DETECTED animal
mov_quarantine_test <- mcmodule::at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_no_detect_all_hag_allherds",
    "mov_quarantine_test_single_level_no_detect_all_hag_allherds"
  ),
  name = "mov_quarantine_test_no_detect_all_hag_allherds",
)

# At least one DETECTED infectious animal - FOR FOMITES
mov_quarantine_test <- mcmodule::at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_detect_infectious_hag_allherds",
    "mov_quarantine_test_single_level_detect_infectious_hag_allherds"
  ),
  name = "mov_quarantine_test_detect_infectious_hag_allherds"
)

# At least one DETECTED PI animal - FOR FOMITES
mov_quarantine_test <- mcmodule::at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_detect_pi_hag_allherds",
    "mov_quarantine_test_single_level_detect_pi_hag_allherds"
  ),
  name = "mov_quarantine_test_detect_pi_hag_allherds"
)

movement <- combine_modules(movement, mov_quarantine_test)

#' 
#' ### Prepare fomites data
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
# Remove animal category - because in quarantine_fomite_link_exp, prev_infectious_total and prev_pi_total is provided at hag level
# Note that it would be feasible to provide it at animal level and calculate totals afterwards (totals should be the same, but you would have individual animals traceability)
remove_fomites_keys<-unique(c(names(animal),names(pathogen_animal),names(pathogen_test), "hg", "row_number", "pregnant_p", "health"))

#Don't remove "pathogen"
remove_fomites_keys<-remove_fomites_keys[remove_fomites_keys!="pathogen"]

#Keep quarantine data at fomites level (no distinction by animal_categories)
mov_quarantine_fomites_data<-mov_quarantine_test_data%>%
  select(-all_of(remove_fomites_keys))%>%
  select(-contains("animal"))%>%
  distinct()%>%
  relocate(all_of(agg_herd_keys))

#' 
#' ### Evaluate fomites
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
quarantine_fomite_exp <- c(
  mov_quarantine_fomite_link = quarantine_fomite_link_exp,
  mov_quarantine_fomite_contact = indir_contact_exp)

mov_quarantine_fomite <- mcmodule::eval_module(
  exp = quarantine_fomite_exp,
  prev_mcmodule = movement,
  data = mov_quarantine_fomites_data
)

# Probability of one animal in the farm being infected by quarantine fomites (it is assumed that one contaminated fomite contacts one farm animal, not the whole census). This module is already hag (health_id, mov_id, pathogen and scenario_id)
mov_quarantine_fomite <- mcmodule::at_least_one(
  mcmodule = mov_quarantine_fomite,
  mc_names = c("indir_contact", "indir_contact_pi"),
  name= c("indir_contact_all_hag")
)

# Aggregated probability of one animal in the farm being infected by quarantine fomites, by mov_id, pathogen and scenario_id
mov_quarantine_fomite <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_fomite,
  mc_name =c("indir_contact_all_hag"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov"
)

# Annual probability of one animal in the farm being infected by quarantine fomites, by health_id, mov_id, pathogen and scenario_id 
mov_quarantine_fomite <- mcmodule::trial_totals(
  mcmodule = mov_quarantine_fomite,
  mc_names = c("indir_contact_all_hag"),
  trials_n = "mov_frequency",
  subsets_n = NULL,
  agg_keys = agg_herd_keys,
  agg_suffix = "",
  level_suffix = c(set="year")
)

# Annual aggregated probability of one animal in the farm being infected by quarantine fomites, by mov_id, pathogen and scenario_id
mov_quarantine_fomite <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_fomite,
  mc_name =c("indir_contact_all_hag_year"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov"
)

# Annual aggregated probability of one animal in the farm being infected by quarantine fomites, by pathogen and scenario_id
mov_quarantine_fomite <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_fomite,
  mc_name = c("indir_contact_all_hag_year"),
  agg_keys = agg_totals_keys)

mov_quarantine_fomite <- add_prefix(mov_quarantine_fomite)

#' 
#' ### Combine modules test and fomites
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
movement <- mcmodule::combine_modules(movement, mov_quarantine_fomite)

# At least one animal is not detected during quarantine or a detected animal contaminates an animal in the farm via fomites, by hag (health_id, mov_id, pathogen and scenario_id)
movement <- mcmodule::at_least_one(
  mcmodule = movement,
  mc_names = c(
    "mov_quarantine_test_no_detect_all_hag_allherds",
    "mov_quarantine_fomite_indir_contact_all_hag"
  ),
  name = "mov_quarantine_entry_all_hag",
  prefix = "mov_quarantine"
)

# Aggregated at least one animal is not detected during quarantine or a detected animal contaminates an animal in the farm via fomites, by movement (mov_id, pathogen and scenario_id)
movement <- mcmodule::agg_totals(
  mcmodule = movement,
  mc_name = c("mov_quarantine_entry_all_hag"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov")


# Annual probability of at least one animal is not detected during quarantine or a detected animal contaminates an animal in the farm via fomites, by health_id, mov_id, pathogen and scenario_id 
movement <- mcmodule::trial_totals(
  mcmodule = movement,
  mc_names = c("mov_quarantine_entry_all_hag"),
  trials_n = "mov_quarantine_fomite_mov_frequency_hag",
  agg_keys = agg_herd_keys,
  agg_suffix = "",
  level_suffix = c(set="year")
)

# Annual probability of at least one animal is not detected during quarantine or a detected animal contaminates an animal in the farm via fomites, by mov_id, pathogen and scenario_id
movement <- mcmodule::agg_totals(
  mcmodule = movement,
  mc_name =c("mov_quarantine_entry_all_hag_year"),
  agg_keys = agg_mov_keys,
  agg_suffix = "mov"
)

# Annual probability of at least one animal is not detected during quarantine or a detected animal contaminates an animal in the farm via fomites, by pathogen and scenario_id
movement <- mcmodule::agg_totals(
  mcmodule = movement,
  mc_name = "mov_quarantine_entry_all_hag_year",
  agg_keys = agg_totals_keys
)

#' 
#' ### Evaluate weights pre-quarantine
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
quarantine_weight_exp <- c(mov_weight_exp = mov_weight_exp)

mov_quarantine_weight <- mcmodule::eval_module(
  exp = quarantine_weight_exp,
  prev_mcmodule = movement,
  data = mov_quarantine_fomites_data,
  summary = TRUE
)

mov_quarantine_weight <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_weight, 
  mc_name ="mov_livestock_all_hag_year_weight",
  agg_keys= agg_totals_keys)

mov_quarantine_weight <- mcmodule::agg_totals(
  mcmodule = mov_quarantine_weight, 
  mc_name ="mov_transport_contact_all_allherds_hag_year_weight",
  agg_keys= agg_totals_keys)

movement <-  mcmodule::combine_modules(movement, mov_quarantine_weight)

#' 
#' ## Wildlife area effect module
#' 
#' ### Prepare data
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  mov_wildlife_area_data <- mov_region %>%
    filter(mov_origin == "pasture") %>%
    left_join(tidy_panel(bsg, "pasture")) %>%
    left_join(tidy_prefix(mov, c("recurrent_days","mov_frequency"), rm_prefix = FALSE)) %>%
    mutate(
      surface = "soil",
      access = TRUE,
      fencing_wildlife = TRUE,
      fencing_perimeter = FALSE
    ) %>%
    left_join(pathogen_surface, relationship = "many-to-many") %>%
    hjoin_region(wildlife_region) %>%
    hjoin_region(wildlife_pathogen_region) %>%
    left_join(wildlife_point_density) %>%
    hjoin_region(pathogen_region) %>%
    left_join(pathogen) %>%
    #Filter those that have wildlife prevalence
    filter(wildlife_prev_mode >0)
  
  message("\nmov_wildlife_area_data (",
          paste(dim(mov_wildlife_area_data), collapse = ", "),
          ") created")
} else{
  message("\nNo recurrent movement to pasture")
}


#' 
#' ### Prepare what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif&
   exists_pasture) {
  #There are no admin what-ifs in this module, but we set up wif for compatibility
  mov_wildlife_area_data <- set_up_wif(mov_wildlife_area_data)
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  wildlife_area_exp <- c(mov_wildlife_area_link = wildlife_area_link_exp,
                                mov_wildlife_area_inf = area_exp)
  
  mov_wildlife <- mcmodule::eval_module(
    exp = wildlife_area_exp,
    data = mov_wildlife_area_data,
    param_names = c(risk_days = "recurrent_days")
  )
  
  # Annually aggreggated by mov_id, pathogen and scenario_id
  # It is already at this level of aggregation, but agg_keys are explicit is to include mov_id in summary keys
  mov_wildlife <- mcmodule::trial_totals(
    mcmodule = mov_wildlife,
    mc_names = "area_inf",
    trials_n = "mov_frequency",
    subsets_n = NULL,
    agg_keys = agg_mov_keys,
    agg_suffix = "mov",
    level_suffix = c(set="year")
  )
  
  # Annually aggreggated by pathogen and scenario_id
  mov_wildlife <- mcmodule::agg_totals(
    mcmodule = mov_wildlife,
    mc_name = c("area_inf_mov_year"),
    agg_keys = agg_totals_keys,
    name = "area_inf_year_agg") 
  
  mov_wildlife <-  mcmodule::add_prefix(mov_wildlife)
}

#' 
#' ## Wildlife contact points module
#' 
#' ### Prepare data
#' 
## ----message=FALSE-------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  mov_wildlife_water_data <- mov_wildlife_area_data %>%
    pivot_longer(
      cols = ends_with("_p") & !pasture_mud_p & !pasture_sum_p,
      names_to = "waterpoint_type",
      values_to = "point_p",
      values_drop_na = TRUE
    ) %>%
    left_join(mov_animal) %>%
    mutate(
      contact_point_type = sub("_p", "", waterpoint_type),
      mud_p = ifelse(
        contact_point_type %in% c("low", "high"),
        ifelse(pasture_mud, as.numeric(gsub(
          "%", "", pasture_mud_p
        )), 0),
        100
      ) / 100,
      contact_point_p = as.numeric(sub("%", "", point_p)) / 100,
      contact_point_type = gsub("pasture_", "", contact_point_type)
    )
  
  mov_wildlife_water_data <- mov_wildlife_water_data %>%
    left_join(animal_point) %>%
    left_join(wildlife_point)
  
  message("\nmov_wildlife_water_data (",
          paste(dim(mov_wildlife_water_data), collapse = ", "),
          ") created")
}

#' 
#' ### Admin what-if
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(admin_wif&
   exists_pasture) {
  #Avoid mud on wateres
  mov_wildlife_water_data <- wif_no_mud(mov_wildlife_water_data, scenario =
                                          "Avoid mud on wateres")
  mov_wildlife_water_data <- wif_high_waterer(mov_wildlife_water_data, scenario =
                                                "Use high wateres")
}

#' 
#' ### Evaluate
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  wildlife_water_exp <- c(
    mov_wildlife_link = wildlife_contact_link_exp,
    mov_wildlife_time = time_exp,
    mov_wildlife_indir_contact = indir_contact_exp
  )
  
  mov_wildlife_water <- mcmodule::eval_module(
    exp = wildlife_water_exp,
    data = mov_wildlife_water_data,
    param_names = c(risk_days = "recurrent_days")
  )
  
    #At least one of your animals is infected by indirect contact with wildlife (by animal_category, mov_id, scenario_id and pathogen, for 1 movement)
  mov_wildlife_water <- mcmodule::trial_totals(
    mcmodule = mov_wildlife_water, 
    mc_name = c("indir_contact"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n",
    level_suffix = levels_herd
  )
  
    #At least one of your animals is infected by indirect contact with wildlife in one movement (by mov_id, scenario_id and pathogen, for 1 movement)
  mov_wildlife_water <- mcmodule::agg_totals(
    mcmodule = mov_wildlife_water,
    mc_name = "indir_contact_allherds",
    agg_keys = agg_mov_keys,
    agg_suffix = "mov"
  )
  
  # Per risk_days (year)
  #At least one of your animals is infected by indirect contact with wildlife in at least one movement (by animal_category, mov_id, scenario_id and pathogen, for all movements in risk_days)
  mov_wildlife_water <- mcmodule::trial_totals(
    mcmodule = mov_wildlife_water,
    mc_names = "indir_contact_allherds",
    trials_n = "mov_frequency",
    subsets_n = NULL,
    level_suffix = c(set="year")
  )
  
  #At least one of your animals is infected by indirect contact with wildlife in at least one movement (by mov_id, scenario_id and pathogen, for all movements in risk_days)
  mov_wildlife_water <- mcmodule::agg_totals(
    mcmodule = mov_wildlife_water,
    mc_name = "indir_contact_allherds_year",
    agg_keys = agg_mov_keys,
    agg_suffix = "mov"
  )
  
  #At least one of your animals is infected by indirect contact with wildlife in at least one movement (by scenario_id and pathogen, for all movements in risk_days)
  mov_wildlife_water <- mcmodule::agg_totals(
    mcmodule = mov_wildlife_water,
    mc_name = "indir_contact_allherds_year",
    agg_keys = agg_totals_keys
  )
  
  mov_wildlife_water <-  mcmodule::add_prefix(mov_wildlife_water)
}

#' 
#' ## Combine modules wildlife area, waterpoints and movement
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  mov_wildlife <-  mcmodule::combine_modules(mov_wildlife, mov_wildlife_water)
  
    # Per movement
    # At least one animal is infected by wildlife area effect or indir contact in risk points in one movement (by mov_id, pathogen and scenario_id, 1 movement)
  mov_wildlife <- at_least_one(
    mcmodule = mov_wildlife,
    mc_names = c(
      "mov_wildlife_area_inf_mov", #Is by mov by default
      "mov_wildlife_water_indir_contact_allherds_mov"
    ),
    name = "mov_wildlife_inf_mov"
  )
  
  # Per risk_days (year)
  # At least one animal is infected by wildlife area effect or indir contact in risk points in one movement (by mov_id, pathogen and scenario_id, for all movements in risk_days)
  mov_wildlife <- at_least_one(
    mcmodule = mov_wildlife,
    mc_names = c(
      "mov_wildlife_area_inf_mov_year",
      "mov_wildlife_water_indir_contact_allherds_year_mov"
    ),
    name = "mov_wildlife_inf_year_mov"
  )
    
  # At least one animal is infected by wildlife area effect or indir contact in risk points in at least one movement (by pathogen and scenario_id, for all movements in risk_days)
  mov_wildlife <-  mcmodule::at_least_one(
    mcmodule = mov_wildlife,
    mc_names = c(
      "mov_wildlife_area_inf_year_agg",
      "mov_wildlife_water_indir_contact_allherds_year_agg"
    ),
    name = "mov_wildlife_inf_year_agg"
  )
  
  movement <- mcmodule::combine_modules(movement, mov_wildlife)
  
  # Per movement
  #Pathogen is introduced into the farm in one movement (by mov_id, pathogen and scenario_id, 1 movement)
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_quarantine_entry_all_hag_mov", #Is by mov by default
      "mov_wildlife_inf_mov"
    ),
    name = "mov_entry_mov"
  )
  
  
  # Per risk_days (year)
  # Pathogen is introduced into the farm in one movement (by mov_id, pathogen and scenario_id, for all movements in risk_days)
  movement <- mcmodule::at_least_one(
    mcmodule = movement,
    mc_names = c("mov_quarantine_entry_all_hag_year_mov", 
                 "mov_wildlife_inf_year_mov"),
    name = "mov_entry_year_mov"
  )
  
  # Pathogen is introduced into the farm in at least one movement (by pathogen and scenario_id, for all movements in risk_days)
  movement <- mcmodule::at_least_one(
    mcmodule = movement,
    mc_names = c("mov_quarantine_entry_all_hag_year_agg", 
                 "mov_wildlife_inf_year_agg"),
    name = "mov_entry_year_agg"
  )
  
} else{
  movement$node_list$mov_entry_mov <- movement$node_list$mov_quarantine_entry_all_hag_mov
  
  movement$node_list$mov_entry_year_mov <- movement$node_list$mov_quarantine_entry_all_hag_year_mov
  
  movement$node_list$mov_entry_year_agg <- movement$node_list$mov_quarantine_entry_all_hag_year_agg

}

#' 
## ------------------------------------------------------------------------------------------------------------------------------------
print_pathway_risk(movement, "mov_entry_year_agg")
message("\nMOVEMENT PATHWAY OK!")

