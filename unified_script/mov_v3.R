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
## -------------------------------------------------------------------------------------------------------------------------
message("\nMOVEMENTS PATHWAY: ")

#' 
#' ## Risk days
#' 
## -------------------------------------------------------------------------------------------------------------------------
from_date <- mov$mov_out_date[!is.na(mov$mov_out_date)]
to_date <- mov$mov_in_date[!is.na(mov$mov_in_date)]
mov$recurrent_days <- ifelse(is.na(mov$recurrent_days), 1, mov$recurrent_days)
CONTINUITY_THRESHOLD <- 5

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
        is_continuous = gap <= CONTINUITY_THRESHOLD | is.na(gap)
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
## -------------------------------------------------------------------------------------------------------------------------
exists_oneway<-any(mov$mov_type=="oneway")
exists_round<-any(mov$mov_type=="round")
exists_pasture<-any(mov$mov_origin=="pasture")

#' 
#' ### Animal Data Processing
#' 
## -------------------------------------------------------------------------------------------------------------------------
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
      "mov_frequency"
    )
  )) %>%
  mutate(
    mov_round = mov_type == "round",
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
mov_status_origin <- tidy_status_table(pathway = "mov", module = "origin")
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
    )) %>%
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
## -------------------------------------------------------------------------------------------------------------------------
# Initialize data structures
mov_herd <- mov %>%
  tidy_prefix("mov_origin", rm_prefix = FALSE)

mov_region <- data.frame()

# Process site regional data
mov_region_site <- get_region_code(bsg, pathway = "farm") %>%
  mutate(mov_origin = "site")

# Combine site data if available
if ("site" %in% mov_herd$mov_origin) {
  mov_region_site <- mov_herd %>%
    inner_join(mov_region_site)
  
  mov_region <- bind_rows(mov_region, mov_region_site)
}

# Process pasture data
mov_pasture <- tidy_panel(panel = "pasture") %>%
  filter(!is.na(pasture_id)) %>%
  inner_join(mov_herd) %>%
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
  left_join(mov_herd)

if (nrow(mov_region_other) > 0) {
  mov_region_other <- mov_herd %>%
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
## -------------------------------------------------------------------------------------------------------------------------
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
## -------------------------------------------------------------------------------------------------------------------------
#Origin
mov_herd <- mov_animal %>%
  left_join(mov_region) %>%
  left_join(pathogen_animal, relationship = "many-to-many") %>%
  hjoin_region(pathogen_region, add_keys = "animal_category") %>%
  left_join(pathogen) %>%
  left_join(mov_health_origin) %>%
  left_join(pathogen_status)

#' 
## -------------------------------------------------------------------------------------------------------------------------
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
## -------------------------------------------------------------------------------------------------------------------------
#All data
mov_data <- mov_herd %>%
  left_join(mov_test_origin) %>%
  mutate(animals_p = NULL) #desambiguate

mov_data <- mov_data %>%
  select(setdiff(names(mov_data), names(mov_veh_animal)) |
           matches("mov_id|farm_id|animal_category|veh_id")) %>% #desambiguate
  left_join(mov_veh_animal, relationship = "many-to-many") %>%
  left_join(pathogen_surface)

#' 
#' #### Separate data by transport (first tranport: oneway or round to, second transport: round from)
#' 
## -------------------------------------------------------------------------------------------------------------------------
#Separate data by vehicle
mov_first_data <- filter(mov_data, veh_direction == "round_to" |
                           veh_direction == "oneway")

message("\nmov_to_data (", paste(dim(mov_first_data), collapse = ", "), ") created")

mov_second_data <- filter(mov_data,
                          veh_direction == "round_from" | veh_direction == "oneway") %>%
  mutate(plan_sensi = ifelse(veh_direction == "oneway", 1, plan_sensi)) #we keep all rows for compatibility but only calculate for round movements

message("\nmov_from_data (", paste(dim(mov_second_data), collapse = ", "), ") created")

#' 
#' #### Admin what-if
#' 
## -------------------------------------------------------------------------------------------------------------------------
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
## -------------------------------------------------------------------------------------------------------------------------
mov_herd_data <- mov_first_data %>%
  select(-starts_with("veh_"))

message("\nmov_herd_data (", paste(dim(mov_herd_data), collapse = ", "), ") created")

#' 
#' ### Admin what-if
#' 
## -------------------------------------------------------------------------------------------------------------------------
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
## -------------------------------------------------------------------------------------------------------------------------
mov_first_data <- group_match(x = mov_first_data, y = mov_herd_data)
mov_second_data <- group_match(mov_second_data, mov_herd_data)

#mov_first_data$health<-mov_herd_data$health
#mov_second_data$health<-mov_herd_data$health

#' 
#' ### Evaluate
#' 
## -------------------------------------------------------------------------------------------------------------------------
mov_herd_expression <- c(
  mov_herd_link = mov_herd_link_expression,
  mov_herd_herd_select = herd_select_expression,
  mov_herd_animal_select = animal_select_expression,
  mov_herd_test = test_expression,
  mov_mov_type_link = mov_type_link_expression
)

mov_herd <- eval_model_expression(model_expression = mov_herd_expression, 
                                  data =mov_herd_data)

# Totals for one-way movements
# Not detected infected animals
mov_herd <- at_least_one(
  mcmodule = mov_herd,
  mc_names = c(
    "own_a_no_detect",
    "own_a_no_detect_pi",
    "own_a_no_detect_tr"
  )
)

mov_herd <- get_totals(
  mcmodule = mov_herd,
  mc_names = c("own_a_no_detect_all"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd",
  subsets_p = "h_inf"
)

mov_herd <- get_agg_totals(mcmodule = mov_herd,
                           mc_name = c("own_b_no_detect_all_herd"))

# Annually
mov_herd<-get_totals(
  mcmodule = mov_herd,
  mc_names = "own_b_no_detect_all_herd",
  trials_n = "mov_frequency",
  subsets_n = NULL,
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "set"
)
mov_herd <- get_agg_totals(mcmodule = mov_herd,
                          mc_name = c("own_b_no_detect_all_herd_set"))


# Not detected infectious animals (infectious during quarantine)
mov_herd <- get_totals(
  mcmodule = mov_herd,
  mc_name = c("own_a_no_detect_infectious"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd",
  subsets_p = "h_inf",
  )

mov_herd <- get_totals(
  mcmodule = mov_herd,
  mc_name = c("own_a_no_detect_pi"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd",
  subsets_p = "h_inf"
  )

# Totals for round movements: animal mix
# Not detected infectious animals (infectious during animal mix)
mov_herd <- get_totals(
  mcmodule = mov_herd,
  mc_name = c("other_a_no_detect_infectious"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd",
  subsets_p = "h_inf",
  keep_variates = TRUE,
  summary=FALSE
  )

mov_herd <- get_totals(
  mcmodule = mov_herd,
  mc_name = c("other_a_no_detect_pi"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd",
  subsets_p = "h_inf",
  keep_variates = TRUE,
  summary=FALSE
)



mov_herd <- add_prefix(mov_herd)

#' 
#' ## Animal mix module
#' 
#' ### Evaluate
#' 
## -------------------------------------------------------------------------------------------------------------------------
if(exists_round) {
  mix_expression <- c(mov_mix_link = mix_link_expression,
                      mov_mix_dir_contact = dir_contact_expression)
  
  mov_mix <- eval_model_expression(
    model_expression = mix_expression,
    prev_mcmodule = mov_herd,
    data = mov_herd_data
  )
  
  mov_mix <- at_least_one(
    mcmodule = mov_mix,
    mc_names = c(
      "a_dir_contact",
      "a_dir_contact_pi"
    )
  )
    
  mov_mix <- get_totals(
    mcmodule = mov_mix,
    mc_names = c("a_dir_contact_all"),
    trials_n = "mov_own_animals_n", 
    subsets_n = "mov_own_farm_n", #Always 1, your farm,
    agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
    suffix = "herd",
  )
  
  
  mov_mix <- get_agg_totals(mcmodule = mov_mix,
                            mc_name = c("b_dir_contact_all_herd"))
  
  # Annually
  mov_mix<-get_totals(
    mcmodule = mov_mix,
    mc_names = "b_dir_contact_all_herd",
    trials_n = "mov_frequency",
    subsets_n = NULL,
    agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
    suffix = "set"
  )
  mov_mix <- get_agg_totals(mcmodule = mov_mix,
                            mc_name = c("b_dir_contact_all_herd_set"))
  
  mov_mix <- add_prefix(mov_mix)
}

#' 
#' ### Combine modules herd and mix
#' 
## -------------------------------------------------------------------------------------------------------------------------
if(exists_round) {
  movement <- combine_modules(mov_herd, mov_mix)
  
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_herd_own_b_no_detect_all_herd",
      "mov_mix_b_dir_contact_all_herd"
    ),
    name = "mov_b_livestock_all_herd",
    prefix = "mov"
  )
  

} else{
  movement <- mov_herd
  
  movement$node_list$mov_b_livestock_all_herd <-
    movement$node_list$mov_herd_own_b_no_detect_all_herd

}


movement<-get_agg_totals(
  mcmodule = movement,
  mc_name = "mov_b_livestock_all_herd",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
  suffix = "mov"
)

movement<-get_agg_totals(mcmodule = movement, mc_name = "mov_b_livestock_all_herd")

# Annually
movement<-get_totals(
  mcmodule = movement,
  mc_names = "mov_b_livestock_all_herd",
  trials_n = "mov_frequency",
  subsets_n = NULL,
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "set"
)

movement<-get_agg_totals(
  mcmodule = movement,
  mc_name = "mov_b_livestock_all_herd_set",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
  suffix = "mov"
)

movement<-get_agg_totals(mcmodule = movement,
                         mc_name = c("mov_b_livestock_all_herd_set")
)

#' 
#' ## Unknown farm module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------------
unk_origin_by <- c("mov_id", "farm_id", "animal_category", "pathogen")

#Other origin
mov_unk_farm_data <- mov_animal %>%
  left_join(mov_region) %>%
  left_join(pathogen_animal, relationship = "many-to-many") %>%
  left_join(pathogen) %>%
  hjoin_region(pathogen_region, add_keys = "animal_category") %>%
  #left_join(mov_health_origin)%>%
  mutate(status = "unk") %>%
  left_join(pathogen_status) %>%
  distinct()

#Add vehicle information
mov_unk_farm_data <- mov_unk_farm_data %>%
  select(setdiff(names(mov_unk_farm_data), names(mov_veh_animal)) |
           matches("mov_id|farm_id|animal_category")) %>% #desambiguate
  left_join(mov_veh_animal, relationship = "many-to-many") %>%
  add_group_id(by = unk_origin_by) %>%
  mutate(pregnant_p = 0.5) #Does not affect risk but needed to calc origin_expression

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
## -------------------------------------------------------------------------------------------------------------------------
#By unknwon origin keys
mov_first_unk_origin_data <- group_match(mov_first_unk_origin_data, mov_first_data, unk_origin_by)

mov_second_unk_origin_data <- group_match(mov_second_unk_origin_data, mov_second_data, unk_origin_by)

#' 
#' ### Evaluate first transport unknown origin
#' 
## -------------------------------------------------------------------------------------------------------------------------
unk_origin_expression <- c(mov_unk_link = unk_link_expression, 
                           mov_unk_herd_select = herd_select_expression,
                           mov_unk_animal_select = animal_select_expression)

mov_first_unk_farm <- eval_model_expression(model_expression = unk_origin_expression, data =
                                              mov_first_unk_origin_data)

mov_first_unk_farm <- get_totals(
  mcmodule = mov_first_unk_farm,
  mc_names = c("a_inf_infectious", "a_inf_pi"),
  trials_n = "other_animals_n",
  subsets_n = "veh_farms_from_n",
  subsets_p = "h_inf",
  prefix = "other",
  all_mc_names = FALSE
)

mov_first_unk_farm <- get_totals(
  mcmodule = mov_first_unk_farm,
  mc_names = c("a_inf_infectious", "a_inf_pi"),
  trials_n = "prev_animals_n",
  subsets_n = "veh_farms_from_n",
  subsets_p = "h_inf",
  prefix = "prev",
  all_mc_names = FALSE
)

mov_first_unk_farm <- get_agg_totals(
  mcmodule = mov_first_unk_farm,
  mc_name = c("prev_b_inf_infectious"),
  keys_names = c("pathogen", "farm_id", "mov_id"),
  keep_variates = TRUE
)

mov_first_unk_farm <- get_agg_totals(
  mcmodule = mov_first_unk_farm,
  mc_name = c("prev_b_inf_pi"),
  keys_names = c("pathogen", "farm_id", "mov_id"),
  keep_variates = TRUE
)

mov_first_unk_farm <- get_agg_totals(
  mcmodule = mov_first_unk_farm,
  mc_name = c("other_b_inf_infectious"),
  keys_names = c("pathogen", "farm_id", "mov_id"),
  keep_variates = TRUE
)

mov_first_unk_farm <- get_agg_totals(
  mcmodule = mov_first_unk_farm,
  mc_name = c("other_b_inf_pi"),
  keys_names = c("pathogen", "farm_id", "mov_id"),
  keep_variates = TRUE
)

mov_first_unk_farm <- add_prefix(mov_first_unk_farm)

mov_herd <- combine_modules(mov_herd, mov_first_unk_farm)

#' 
#' ### Evaluate second transport unknown origin
#' 
## -------------------------------------------------------------------------------------------------------------------------
#Check if trasnport to and from are equal
from_equal_to <- all(mov_first_data[, !names(mov_first_data) %in% c("veh_direction", "hg")] ==
                       mov_second_data[, !names(mov_second_data) %in% c("veh_direction", "hg")], na.rm =
                       TRUE)

#If it they are not not equal calcualte unk farm "from" module
if (!from_equal_to) {
  mov_second_unk_farm <- eval_model_expression(model_expression = unk_origin_expression, data =
                                                 mov_second_unk_origin_data)
  
  mov_second_unk_farm <- get_totals(
    mcmodule = mov_second_unk_farm,
    mc_names = c("a_inf_infectious", "a_inf_pi"),
    trials_n = "other_animals_n",
    subsets_n = "veh_farms_from_n",
    subsets_p = "h_inf",
    prefix = "other",
    all_mc_names = FALSE
  )
  
  mov_second_unk_farm <- get_totals(
    mcmodule = mov_second_unk_farm,
    mc_names = c("a_inf_infectious", "a_inf_pi"),
    trials_n = "prev_animals_n",
    subsets_n = "veh_farms_from_n",
    subsets_p = "h_inf",
    prefix = "prev",
    all_mc_names = FALSE
  )
  
  mov_second_unk_farm <- get_agg_totals(
    mcmodule = mov_second_unk_farm,
    mc_name = c("prev_b_inf_infectious"),
    keys_names = c("pathogen", "farm_id", "mov_id"),
    keep_variates = TRUE
  )
  
  mov_second_unk_farm <- get_agg_totals(
    mcmodule = mov_second_unk_farm,
    mc_name = c("prev_b_inf_pi"),
    keys_names = c("pathogen", "farm_id", "mov_id"),
    keep_variates = TRUE
  )
  
  mov_second_unk_farm <- get_agg_totals(
    mcmodule = mov_second_unk_farm,
    mc_name = c("other_b_inf_infectious"),
    keys_names = c("pathogen", "farm_id", "mov_id"),
    keep_variates = TRUE
  )
  
  mov_second_unk_farm <- get_agg_totals(
    mcmodule = mov_second_unk_farm,
    mc_name = c("other_b_inf_pi"),
    keys_names = c("pathogen", "farm_id", "mov_id"),
    keep_variates = TRUE
  )
  
  mov_second_unk_farm <- add_prefix(mov_second_unk_farm)
  
}

#' 
#' ## Transport module
#' 
#' ### Evaluate first transport
#' 
## -------------------------------------------------------------------------------------------------------------------------
transport_expression <- c(
  mov_transport_link = transport_link_expression,
  mov_transport_dir_contact = dir_contact_expression,
  mov_transport_indir_contact = indir_contact_expression
)

mov_transport_first <- eval_model_expression(
  model_expression = transport_expression,
  prev_mcmodule = mov_first_unk_farm,
  data = mov_first_data)

mov_transport_first <- get_totals(
  mcmodule = mov_transport_first,
  mc_names = c("a_dir_contact", "a_dir_contact_pi"),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n"
)

mov_transport_first <- get_totals(
  mcmodule = mov_transport_first,
  mc_names = c("a_indir_contact", "a_indir_contact_pi"),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n"
)

mov_transport_first <- get_totals(
  mcmodule = mov_transport_first,
  mc_names = c("a_dir_contact_all", "a_indir_contact_all"),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "a_contact_all"
)

mov_transport_first <- get_agg_totals(mcmodule = mov_transport_first, mc_name =
                                        c("a_contact_all"))

mov_transport_first <- get_agg_totals(mcmodule = mov_transport_first, mc_name =
                                        c("b_contact_all"))

mov_transport_first <- add_prefix(mov_transport_first)

#' 
#' ### Evaluate second transport
#' 
## -------------------------------------------------------------------------------------------------------------------------
#If it is equal copy unk farm "to" module
if (from_equal_to) {
  mov_transport_second <- add_prefix(mov_transport_first,
                                     prefix = "mov_transport_second",
                                     rewrite_module = "mov_transport_first")
  message("\nmov_transport_second mcmodule copied from mov_transport_first")
  #Else calculate unk farm "from" module
} else{
  mov_transport_second <- eval_model_expression(
    model_expression = transport_expression,
    prev_mcmodule = mov_second_unk_farm,
    data = mov_second_data
  )
  
  mov_transport_second <- get_totals(
    mcmodule = mov_transport_second,
    mc_names = c("a_dir_contact", "a_dir_contact_pi"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n"
  )
  
  mov_transport_second <- get_totals(
    mcmodule = mov_transport_second,
    mc_names = c("a_indir_contact", "a_indir_contact_pi"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n"
  )
  
  mov_transport_second <- get_totals(
    mcmodule = mov_transport_second,
    mc_names = c("a_dir_contact_all", "a_indir_contact_all"),
    trials_n = "mov_own_animals_n",
    subsets_n = "mov_own_farm_n",
    name = "a_contact_all"
  )
  
  mov_transport_second <- get_agg_totals(mcmodule = mov_transport_second, mc_name =
                                           c("a_contact_all"))
  
  mov_transport_second <- get_agg_totals(mcmodule = mov_transport_second, mc_name =
                                           c("b_contact_all"))
  
  mov_transport_second <- add_prefix(mov_transport_second)
}

#' 
#' ### Combine modules first transport and second transport
#' 
## -------------------------------------------------------------------------------------------------------------------------
mov_transport <- combine_modules(mov_transport_first, mov_transport_second)

mov_transport <- get_totals(
  mcmodule = mov_transport,
  mc_names = c(
    "mov_transport_first_a_dir_contact_all",
    "mov_transport_second_a_dir_contact_all"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "a_dir_contact_all"
)

mov_transport <- get_totals(
  mcmodule = mov_transport,
  mc_names = c(
    "mov_transport_first_a_indir_contact_all",
    "mov_transport_second_a_indir_contact_all"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "a_indir_contact_all"
)

mov_transport <- get_totals(
  mcmodule = mov_transport,
  mc_names = c(
    "mov_transport_first_a_contact_all",
    "mov_transport_second_a_contact_all"
  ),
  trials_n = "mov_own_animals_n",
  subsets_n = "mov_own_farm_n",
  name = "a_contact_all"
)

mov_transport <- get_agg_totals(
  mcmodule = mov_transport,
  mc_name = "b_contact_all",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd"
)

mov_transport <- get_agg_totals(
  mcmodule = mov_transport,
  mc_name = "b_contact_all",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
  suffix = "mov"
)


mov_transport <- get_agg_totals(mcmodule = mov_transport, mc_name = c("b_contact_all"))

# Annually
mov_transport <- get_totals(
  mcmodule = mov_transport,
  mc_names = "b_contact_all",
  trials_n = "mov_frequency",
  subsets_n = NULL,
  agg_keys =  c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd",
)

mov_transport <- get_agg_totals(
  mcmodule = mov_transport,
  mc_name = "b_contact_all_herd_set",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
  suffix = "mov"
)

mov_transport <- get_agg_totals(mcmodule = mov_transport, mc_name = c("b_contact_all_herd_set"))

mov_transport <- add_prefix(mov_transport)

#' 
#' ### Combine modules transport and livestock
#' 
## -------------------------------------------------------------------------------------------------------------------------
movement <- combine_modules(movement, mov_transport)

movement <- at_least_one(
  mcmodule = movement,
  mc_names = c("mov_transport_b_contact_all_herd", "mov_b_livestock_all_herd"),
  name = "mov_b_inf_all_herd",
  prefix = "mov"
)

movement <- at_least_one(
  mcmodule = movement,
  mc_names = c("mov_transport_b_contact_all_herd_set", "mov_b_livestock_all_herd_set"),
  name = "mov_b_inf_all_herd_set",
  prefix = "mov"
)

movement <- get_agg_totals(mcmodule = movement, mc_name = c("mov_b_inf_all_herd_set"))

#For single level calculations
if (exists_round) {
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_transport_a_contact_all",
      "mov_mix_a_dir_contact_all"
    ),
    name = "mov_a_contact_all",
    prefix = "mov"
  )
} else{
  movement$node_list$mov_a_contact_all <- movement$node_list$mov_transport_a_contact_all
}

#' 
#' ## Quarantine module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------------
#Tidy diagnostic tests before transport by disease, farm and type of test
mov_test_quarantine <- mov_health_quarantine %>%
  left_join(pathogen_test)

#Tidy quarantine parameters
quarantine_bsg_data <- tidy_prefix(bsg, module = "quarantine", rm_prefix = FALSE)

#Add movement and tests info
mov_quarantine_data <- quarantine_bsg_data %>%
  left_join(mov_herd_data[, !names(mov_herd_data) %in% "test"]) %>%
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

message("\nquarantine_data (", paste(dim(mov_quarantine_data), collapse =
                                       ", "), ") created")

#' 
#' ### Admin what-if
#' 
## -------------------------------------------------------------------------------------------------------------------------
if(admin_wif) {
  #Match index
  mov_quarantine_data <- group_match(mov_quarantine_data, mov_herd_data)
  
  #Clean quarantine equipement
  mov_quarantine_data <- wif_quarantine(mov_quarantine_data,quarantine, 
                                        scenario = "Isolate new animals")
  #Test  during quarantine
  mov_quarantine_data <- wif_test(mov_quarantine_data,quarantine,
                                  scenario = "Test during quarantine",
                                  prev_wif = "wif_quarantine")
  
  #Clean quarantine equipement
  mov_quarantine_data <- wif_clean(mov_quarantine_data,quarantine,
                                   scenario = "Clean and disinfect quarantine equipment")
  
  #Exclusive quarantine material
  mov_quarantine_data <- wif_exclusive_material(mov_quarantine_data,quarantine,
                                                scenario = "Exclusive quarantine material")
  
  #Test during quarantine with exclusive material
  mov_quarantine_data <- combine_wif(
    data = mov_quarantine_data,
    scenario = "Test during quarantine with exclusive material",
    wif_list = c("quarantine", "exclusive_material", "test")
  )
  
}

#' 
#' ### Evaluate test multilevel
#' 
## -------------------------------------------------------------------------------------------------------------------------
quarantine_test_multilevel_expression <- c(
  mov_quarantine_herd_select = herd_select_expression,
  mov_quarantine_test_link = quarantine_test_link_expression,
  mov_quarantine_test = test_expression)

mov_quarantine_test_multilevel <- eval_model_expression(
  model_expression = quarantine_test_multilevel_expression,
  prev_mcmodule = movement,
  param_names = c(
    a_inf = "mov_herd_own_a_no_detect",
    a_inf_pi = "mov_herd_own_a_no_detect_pi",
    a_inf_tr = "mov_herd_own_a_no_detect_tr"
  ),
  data = mov_quarantine_data,
  match_prev = TRUE
)

# NOT detected infectious animal
mov_quarantine_test_multilevel <- get_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_names = c("a_no_detect", "a_no_detect_pi", "a_no_detect_tr"),
  subsets_p = "h_inf",
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd"
)

mov_quarantine_test_multilevel <- get_agg_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = c("a_no_detect_all_herd"))

mov_quarantine_test_multilevel <- get_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = c("a_no_detect_infectious"),
  subsets_p = "h_inf",
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd")

 
# Detected infectious animal
mov_quarantine_test_multilevel <- get_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = c("a_detect_infectious", "a_detect_pi"),
  name = "a_detect_infectious_all",
  subsets_p = "h_inf",
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd"
)

#No agg
mov_quarantine_test_multilevel <- get_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = c("a_detect_infectious", "a_detect_pi"),
  name = "a_detect_infectious_all",
  subsets_p = "h_inf"
)

mov_quarantine_test_multilevel <- get_agg_totals(
  mcmodule = mov_quarantine_test_multilevel,
  mc_name = c("a_detect_infectious_all_herd"))

mov_quarantine_test_multilevel <- add_prefix(mov_quarantine_test_multilevel)

#' 
#' ### Evaluate test single level
#' 
## -------------------------------------------------------------------------------------------------------------------------
quarantine_test_single_level_expression <- c(
  mov_quarantine_test_link = quarantine_test_link_expression,
  mov_quarantine_test = test_expression)

mov_quarantine_test_single_level <- eval_model_expression(
  model_expression = quarantine_test_single_level_expression,
  prev_mcmodule = movement,
  param_names = c(
    a_inf = "mov_a_contact_all",
    a_inf_pi = "mov_herd_no_risk",
    a_inf_tr = "mov_herd_no_risk"
  ),
  data = mov_quarantine_data,
  match_prev = TRUE
)

# NOT detected infectious animal
mov_quarantine_test_single_level <- get_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_names = c("a_no_detect", "a_no_detect_pi", "a_no_detect_tr"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd"
)

mov_quarantine_test_single_level <- get_agg_totals(
  mcmodule = mov_quarantine_test_single_level, 
  mc_name = c("a_no_detect_all_herd"))

mov_quarantine_test_single_level <- get_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_name = c("a_no_detect_infectious"),
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd")


# Detected infectious animal
mov_quarantine_test_single_level <- get_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_name = c("a_detect_infectious", "a_detect_pi"),
  name = "a_detect_infectious_all",
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd"
)

#No agg
mov_quarantine_test_single_level <- get_totals(
  mcmodule = mov_quarantine_test_single_level,
  mc_name = c("a_detect_infectious", "a_detect_pi"),
  name = "a_detect_infectious_all"
)

mov_quarantine_I <- get_agg_totals(mcmodule = mov_quarantine_test_single_level,
                                   mc_name = c("a_detect_infectious_all_herd"))

mov_quarantine_test_single_level <- add_prefix(mov_quarantine_test_single_level)

#' 
#' ### Combine modules quarantine test single level and multilevel
#' 
## -------------------------------------------------------------------------------------------------------------------------
mov_quarantine_test <- combine_modules(mov_quarantine_test_single_level,
                                       mov_quarantine_test_multilevel)

#Animal level
mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_t_no_detect_all_herd",
    "mov_quarantine_test_multilevel_t_no_detect_all_herd"
  ),
  name = "mov_quarantine_test_a_no_detect_all_herd",
)

#Batch level
mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_b_no_detect_all_herd",
    "mov_quarantine_test_single_level_b_no_detect_all_herd"
  ),
  name = "mov_quarantine_test_b_no_detect_all_herd",
)

# At least one NOT detected infectious animal
mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_b_no_detect_herd",
    "mov_quarantine_test_single_level_b_no_detect_herd"
  ),
  name = "mov_quarantine_test_b_no_detect_herd"
)

mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_b_no_detect_infectious_herd",
    "mov_quarantine_test_single_level_b_no_detect_infectious_herd"
  ),
  name = "mov_quarantine_test_b_no_detect_infectious_herd"
)

mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_b_no_detect_pi_herd",
    "mov_quarantine_test_single_level_b_no_detect_pi_herd"
  ),
  name = "mov_quarantine_test_b_no_detect_pi_herd"
)

# At least one detected (during quarantine) infectious animal FOR FOMITES
mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_b_detect_infectious",
    "mov_quarantine_test_single_level_b_detect_infectious"
  ),
  name = "mov_quarantine_test_b_detect_infectious"
)

mov_quarantine_test <- at_least_one(
  mcmodule = mov_quarantine_test,
  mc_names = c(
    "mov_quarantine_test_multilevel_b_detect_pi",
    "mov_quarantine_test_single_level_b_detect_pi"
  ),
  name = "mov_quarantine_test_b_detect_pi"
)

movement <- combine_modules(movement, mov_quarantine_test)

#' 
#' ### Evaluate fomites
#' 
## -------------------------------------------------------------------------------------------------------------------------
quarantine_fomite_expression <- c(
  mov_quarantine_fomite_link = quarantine_fomite_link_expression,
  mov_quarantine_fomite_contact = indir_contact_expression)

# TODO HOW MANY SUSCEPTIBLE ANIMALS? 1 FARM!
mov_quarantine_fomite <- eval_model_expression(
  model_expression = quarantine_fomite_expression,
  prev_mcmodule = movement,
  param_names = c(
    b_detect_infectious = "mov_quarantine_test_b_detect_infectious",
    b_detect_pi = "mov_quarantine_test_b_detect_pi"
  ),
  data = mov_quarantine_data,
  match_prev = TRUE
)

mov_quarantine_fomite <- get_totals(
  mcmodule = mov_quarantine_fomite,
  mc_names = c("a_indir_contact", "a_indir_contact_pi"),
  subsets_n = NULL,
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "herd")

mov_quarantine_fomite <- get_agg_totals(mcmodule = mov_quarantine_fomite,
                                        mc_name = c("b_indir_contact_all_herd"))

mov_quarantine_fomite <- add_prefix(mov_quarantine_fomite)

#' 
#' ### Combine modules test and fomites
#' 
## -------------------------------------------------------------------------------------------------------------------------
movement <- combine_modules(movement, mov_quarantine_fomite)

movement <- at_least_one(
  mcmodule = movement,
  mc_names = c(
    "mov_quarantine_test_b_no_detect_all_herd",
    "mov_quarantine_fomite_b_indir_contact_all_herd"
  ),
  name = "mov_quarantine_b_entry_all_herd",
  prefix = "mov_quarantine"
)

movement <- get_agg_totals(mcmodule = movement,
                           mc_name = c("mov_quarantine_b_entry_all_herd"))

#Annually
movement <- get_totals(
  mcmodule = movement,
  mc_names = "mov_quarantine_b_entry_all_herd",
  trials_n = "mov_frequency",
  subsets_n = NULL,
  agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id", "health_id"),
  suffix = "set"
)

movement <- get_agg_totals(mcmodule = movement,
                           mc_name = c("mov_quarantine_b_entry_all_herd_set"))

#By movement
movement <- get_agg_totals(
  mcmodule = movement,
  mc_name = "mov_quarantine_b_entry_all_herd",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
  suffix = "mov"
)

#All year
movement <- get_agg_totals(
  mcmodule = movement,
  mc_name = "mov_quarantine_b_entry_all_herd_set",
  keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
  suffix = "mov"
)

#' 
#' ### Evaluate weights pre-quarantine
#' 
## -------------------------------------------------------------------------------------------------------------------------
# TODO TIS IS A PROVISIONAL KLUDGE
quarantine_weight_expression <- c(mov_weight_expression = mov_weight_expression)

mov_quarantine_weight <- eval_model_expression(
  model_expression = quarantine_weight_expression,
  prev_mcmodule = movement,
  data = NULL,
  create_nodes=FALSE,
  match_prev = TRUE,
  summary = TRUE
)

mov_quarantine_weight <- get_agg_totals(
  mcmodule = mov_quarantine_weight, 
  mc_name ="mov_b_livestock_all_herd_set_weight")

mov_quarantine_weight <- get_agg_totals(
  mcmodule = mov_quarantine_weight,
  mc_name = "mov_transport_b_contact_all_herd_set_weight")

movement <- combine_modules(movement, mov_quarantine_weight)

#' 
#' ## Wildlife area effect module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------------
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
    left_join(pathogen_surface) %>%
    hjoin_region(wildlife_region) %>%
    hjoin_region(wildlife_pathogen_region) %>%
    left_join(wildlife_point_density) %>%
    hjoin_region(pathogen_region) %>%
    left_join(pathogen) %>%
    #Filter those that have wildlife prevalence
    filter(pathogen %in%
             unique(wildlife_pathogen_region$pathogen[wildlife_pathogen_region$wl_prev_mode >
                                                        0]))
  
  message("\nmov_wildlife_area_data (",
          paste(dim(mov_wildlife_area_data), collapse = ", "),
          ") created")
} else{
  message("\nNo recurrent movement to pasture")
}


#' 
#' ### Prepare what-if
#' 
## -------------------------------------------------------------------------------------------------------------------------
if(admin_wif&
   exists_pasture) {
  #There are no admin what-ifs in this module, but we set up wif for compatibility
  mov_wildlife_area_data <- set_up_wif(mov_wildlife_area_data)
}

#' 
#' ### Evaluate
#' 
## -------------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  wildlife_area_expression <- c(mov_wildlife_area_link = wildlife_area_link_expression,
                                mov_wildlife_area_inf = area_inf_expression)
  
  mov_wildlife <- eval_model_expression(
    model_expression = wildlife_area_expression,
    data = mov_wildlife_area_data,
    param_names = c(risk_days = "recurrent_days")
  )
  
  mov_wildlife <- get_agg_totals(mcmodule = mov_wildlife, mc_name = c("area_inf"))
  
  #By mov
  mov_wildlife <- get_agg_totals(
    mcmodule = mov_wildlife,
    mc_name = "area_inf",
    keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
    suffix = "mov"
  )
  
  #Annually
  mov_wildlife <- get_totals(
    mcmodule = mov_wildlife,
    mc_names = "area_inf",
    trials_n = "mov_frequency",
    subsets_n = NULL,
    agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id"),
    suffix = "set"
  )
  
  mov_wildlife <- get_agg_totals(mcmodule = mov_wildlife, mc_name = c("area_inf_set"))

  mov_wildlife <- add_prefix(mov_wildlife)
}

#' 
#' ## Wildlife contact points module
#' 
#' ### Prepare data
#' 
## -------------------------------------------------------------------------------------------------------------------------
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
## -------------------------------------------------------------------------------------------------------------------------
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
## -------------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  wildlife_water_expression <- c(
    mov_wildlife_link = wildlife_water_link_expression,
    mov_wildlife_time = time_expression,
    mov_wildlife_indir_contact = indir_contact_expression
  )
  
  mov_wildlife_water <- eval_model_expression(
    model_expression = wildlife_water_expression,
    data = mov_wildlife_water_data,
    param_names = c(risk_days = "recurrent_days")
  )
  
  mov_wildlife_water <- get_totals(mcmodule = mov_wildlife_water, mc_name =
                                     c("a_indir_contact"))
  
  mov_wildlife_water <- get_agg_totals(mcmodule = mov_wildlife_water, mc_name =
                                         c("b_indir_contact"))
  
  #By mov
  mov_wildlife_water <- get_agg_totals(
    mcmodule = mov_wildlife_water,
    mc_name = "b_indir_contact",
    keys_names = c("pathogen", "farm_id", "scenario_id", "mov_id"),
    suffix = "mov"
  )
  
  #Annually
  mov_wildlife_water <- get_totals(
    mcmodule = mov_wildlife_water,
    mc_names = "b_indir_contact",
    trials_n = "mov_frequency",
    subsets_n = NULL,
    agg_keys = c("pathogen", "farm_id", "scenario_id", "mov_id"),
    suffix = "set"
  )
  
  mov_wildlife_water <- get_agg_totals(mcmodule = mov_wildlife_water, mc_name =
                                         c("b_indir_contact_set"))
    
  mov_wildlife_water <- add_prefix(mov_wildlife_water)
}

#' 
#' ## Combine modules wildlife area, waterpoints and pasture
#' 
## -------------------------------------------------------------------------------------------------------------------------
if(exists_pasture) {
  mov_wildlife <- combine_modules(mov_wildlife, mov_wildlife_water)
  
  mov_wildlife <- at_least_one(
    mcmodule = mov_wildlife,
    mc_names = c(
      "mov_wildlife_area_inf_agg",
      "mov_wildlife_water_b_indir_contact_agg"
    ),
    name = "mov_wildlife_inf_agg"
  )
  
  mov_wildlife <- at_least_one(
    mcmodule = mov_wildlife,
    mc_names = c(
      "mov_wildlife_area_inf_set_agg",
      "mov_wildlife_water_b_indir_contact_set_agg"
    ),
    name = "mov_wildlife_inf_set_agg"
  )
  movement <- combine_modules(movement, mov_wildlife)
  
  
  #All pasture modules
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c("mov_quarantine_b_entry_all_herd", "mov_wildlife_inf_agg"),
    name = "mov_entry_agg"
  )
  
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_quarantine_b_entry_all_herd_set_agg",
      "mov_wildlife_inf_set_agg"
    ),
    name = "mov_entry_set_agg"
  )
  
  #By movement
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_wildlife_area_inf_mov",
      "mov_wildlife_water_b_indir_contact_mov"
    ),
    name = "mov_wildlife_all_mov"
  )
  
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c("mov_quarantine_b_entry_all_herd_mov", "mov_wildlife_all_mov"),
    name = "mov_entry_mov"
  )
  
  movement <- at_least_one(
    mcmodule = movement,
    mc_names = c(
      "mov_quarantine_b_entry_all_herd_set_mov",
      "mov_wildlife_all_mov"
    ),
    name = "mov_entry_mov_set"
  )
  
} else{
  movement$node_list$mov_entry_agg <- movement$node_list$mov_quarantine_b_entry_all_herd
  
  movement$node_list$mov_entry_mov <- movement$node_list$mov_quarantine_b_entry_all_herd_mov
  
  movement$node_list$mov_entry_set_agg <- movement$node_list$mov_quarantine_b_entry_all_herd_set_agg
  
  movement$node_list$mov_entry_mov <- movement$node_list$mov_quarantine_b_entry_all_herd_mov
  
  movement$node_list$mov_entry_mov_set <- movement$node_list$mov_quarantine_b_entry_all_herd_set_mov
}

#' 
## -------------------------------------------------------------------------------------------------------------------------
message("\nMOVEMENT PATHWAY OK!")

#' 
#' ## Save results
#' 
