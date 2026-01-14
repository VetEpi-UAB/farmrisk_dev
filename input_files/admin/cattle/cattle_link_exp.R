#### Farm link expression ####

farm_link_exp <- quote({
  #Assuming that the probability of the animal being infected just before testing is negligible
  test_time <- test_opt_time
})

#### Other site herd link expression ####

mov_herd_link_exp <- quote({
  # Effective number of other-farm animals each of our herd is exposed to:
  contacts_n<-(mov_other_animals_n* ((site_bulls * 0.30) + ((1 - site_bulls) * 0.15))/mov_own_animals_n) * animal_category_p

  # Number of distinct other farms involved in the contact event.
  # - For round movements this is mov_other_farm_n (other farms sharing the site).
  # - For not round movements this is mov_own_farm_n (number of origin farms for the incoming animals).
  farms_n <- mov_round * mov_other_farm_n + (1 - mov_round) * mov_own_farm_n
  
  #Assuming that the probability of the animal being infected just before testing is negligible
  test_time <- test_opt_time
})


#### Unknown Farm link expression ####

unk_link_exp <- quote({
  #OTHER FARMS (in current journeys)

  #PROBABILITY MULTIPLE ORIGINS - Did your animals and the others come from the same farm? (veh_epiunit)
  #if the vehicle veh_epiunit="Yes", probability of sharing is 0 (veh_mult_origin=1)
  #if the vehicle veh_epiunit="Unknown", probability of sharing is is taken from literature (veh_mult_origin=veh_mult_origin_p)
  #if the vehicle veh_epiunit="No", probability of sharing is 1 (veh_mult_origin=1)

  #Equivalent to: ifelse(veh_epiunit_yes,0, ifelse(veh_epiunit_unk,veh_mult_origin_p,1))
  veh_mult_origin <- (1 - veh_epiunit_yes) *
    (veh_epiunit_unk * veh_mult_origin_p + (1 - veh_epiunit_unk))

  #NUMBER OF ORIGINS IN A SHARED TRANSPORT
  #Value is taken from the literature if the probability of multiple origins is not zero.

  #Equivalent to:ifelse(veh_mult_origin>0,veh_farms_from_n,0)
  other_farms_n <- veh_farms_from_n * (veh_mult_origin > 0)

  #Other animals (by farm) in the same vehicle
  #NAs form 0/0 are 0

  other_animals_n <- mcnode_na_rm(other_all_animals_n / other_farms_n)

  #PREVIOUS FARMS (in previous journeys)

  #NUMBER OF PREVIOUS FARMS - Is the vehicle shared with other ruminant farms? (veh_otherfarms)
  #If veh_own=TRUE, number of previous farms is given by user bsg survey (prev_farms_n=veh_otherfarms_cattle_n)
  #If veh_own=FALSE, number of previous farms is taken from literature (prev_farms_n=veh_farms_from_n)
  #If the vehicle veh_otherfarms=FALSE ("No"), number of previous farms is 0 (prev_farms_n=0)
  #Probability the previous journey had animals from several farms (veh_mult_origin_p)

  #Equivalent to: ifelse(veh_own,ifelse(veh_otherfarms,veh_otherfarms_cattle_n,0),veh_farms_from_n)
  #Expected number of farms if the vehicle had more than one origin
  prev_farms_mult_n <- veh_own *
    veh_otherfarms *
    veh_otherfarms_cattle_n +
    (1 - veh_own) * veh_farms_from_n

  #Number of farms
  #If prev_farms_mult=0 it means there are no previous farms
  #If prev_farms_mult=>0
  # the previous movement only carried animals from one (1) farm with a probability of 1-veh_mult_origin_p
  # previous movement carried animals from more than one (prev_farms_mult_n) holding with a probability of veh_mult_origin_p
  prev_farms_n <- (prev_farms_mult_n > 0) *
    veh_mult_origin_p +
    prev_farms_mult_n * (1 - veh_mult_origin_p)

  #Previous animals (by farm) in the same vehicle
  prev_animals_n <- mcnode_na_rm(prev_all_animals_n / prev_farms_n)
})


#### Transport link expression ####

transport_link_exp <- quote({
  #Probabilty that the vehicle carried at least one infected animal on a previous journey
  prev_infectious_total <- prev_infectious_hag_allherds
  prev_pi_total <- prev_pi_hag_allherds

  #Probabilty that the vehicle carries at least one infected animal on the current journey
  other_infectious_total <- other_infectious_hag_allherds
  other_pi_total <- other_pi_hag_allherds

  #PROBABILITY SHARED TRANSPORT - Were only the animal for this farm transported in this movement? (veh_exclusive)
  #if the vehicle  veh_exclusive="Yes", probability of sharing is 0 (veh_share=0)
  #if the vehicle veh_exclusive="Unknown", probability of sharing is taken from literature (veh_share=veh_share_p)
  #if the vehicle veh_exclusive="No", probability of sharing is 1 (veh_share=1)

  #Equivalent to: ifelse(veh_exclusive_yes,0, ifelse(veh_exclusive_unk,veh_share_p,1))
  veh_share <- (1 - veh_exclusive_yes) *
    (1 - veh_exclusive_unk * (1 - veh_share_p))

  #PROBABILITY DIRECT CONTACT: other animals in the vehicle and animals come from diferent farms
  direct <- veh_share * veh_mult_origin * (1 - veh_none)

  #PROBABILITY INDIRECT CONTACT:
  #If veh_own=TRUE and veh_otherfarms=FALSE then the probability of indirect contact is 0
  #Else the probability of indirect contact is 1
  indir <- (1 - (veh_own * (1 - veh_otherfarms))) * (1 - veh_none)

  #PROBABILITY CLEANING AND DISINFECTION
  #If veh_cleaning = "Each time the vehicle is used"  then veh_cleaning=TRUE
  cleaning <- veh_cleaning
  disinfection <- veh_cleaning

  #Time between unloading animals in a farm and loading new animals
  risk_contact_time <- veh_time_between

  #Level of indirect contact risk (1=High, 2=Moderate, 3=Low, 4=Very low)
  indir_level <- 1

  #Number of times indirect contact happens
  n_times <- 1
})

#### Quarantine link test expression ####

quarantine_test_link_exp <- quote({
  #Time when diagnostic tests are performed during quarantine
  test_time <- quarantine_test_time
})

#### Quarantine indir link expression ####
quarantine_fomite_link_exp <- quote({
  #Probability direct contact
  direct <- (1 - quarantine) * quarantine_direct

  #Probability at least one animal is infectious during quarantine but is detected and will be removed
  prev_infectious_total <- detect_infectious_hag_allherds
  prev_pi_total <- detect_pi_hag_allherds

  #Probability indirect contact
  indir <- quarantine * (1 - quarantine_direct) * quarantine_equipment

  cleaning <- quarantine_cleaning
  disinfection <- quarantine_disinfection

  #Time between quarantine visit and herd visit
  risk_contact_time <- (1 - quarantine_visits_never) * quarantine_visits_time

  #Level of indirect contact risk (1=High, 2=Moderate, 3=Low, 4=Very low)
  indir_level <- 2

  #Number of times indirect contact happens
  n_times <- mcnode_na_rm(quarantine_time / quarantine_frequency)
})


#### Movement Weighted risk ####

mov_weight_exp <- quote({
  # see https://github.com/NataliaCiria/biosecurity_model/issues/187

  # By single movement - without considering movement frequency
  # Proportion of introduction risk attributed to transport by hag (health_id, mov_id, pathogen and scenario_id)
  mov_transport_contact_all_allherds_hag_weight_p <- mcnode_na_rm(
    mov_transport_contact_all_allherds_hag /
      (mov_transport_contact_all_allherds_hag + mov_livestock_all_hag)
  )

  # Proportion of introduction risk attributed to livestock (origin or mix) by hag (health_id, mov_id, pathogen and scenario_id)
  mov_livestock_all_hag_weight_p <- mcnode_na_rm(
    mov_livestock_all_hag /
      (mov_transport_contact_all_allherds_hag + mov_livestock_all_hag)
  )

  # Introduction risk attributed to transport by hag (health_id, mov_id, pathogen and scenario_id)
  mov_transport_contact_all_allherds_hag_weight <-
    mov_quarantine_entry_all_hag *
    mov_transport_contact_all_allherds_hag_weight_p

  # Introduction risk attributed to livestock (origin or mix) by hag (health_id, mov_id, pathogen and scenario_id)
  mov_livestock_all_hag_weight <-
    mov_quarantine_entry_all_hag * mov_livestock_all_hag_weight_p

  # WEIGHTS - considering movement frequency (see https://github.com/NataliaCiria/biosecurity_model/issues/187)
  # BY YEAR - considering movement frequency
  # Proportion of introduction risk attributed to transport by hag (health_id, mov_id, pathogen and scenario_id)
  mov_transport_contact_all_allherds_hag_weight_year_p <- mcnode_na_rm(
    mov_transport_contact_all_allherds_hag_year /
      (mov_transport_contact_all_allherds_hag_year + mov_livestock_all_hag_year)
  )

  # Proportion of introduction risk attributed to livestock (origin or mix) by hag (health_id, mov_id, pathogen and scenario_id)
  mov_livestock_all_hag_weight_year_p <- mcnode_na_rm(
    mov_livestock_all_hag_year /
      (mov_transport_contact_all_allherds_hag_year + mov_livestock_all_hag_year)
  )

  # Introduction risk attributed to transport by hag (health_id, mov_id, pathogen and scenario_id)
  mov_transport_contact_all_allherds_hag_year_weight <-
    mov_quarantine_entry_all_hag_year *
    mov_transport_contact_all_allherds_hag_weight_year_p

  # Introduction risk attributed to livestock (origin or mix) by hag (health_id, mov_id, pathogen and scenario_id)
  mov_livestock_all_hag_year_weight <-
    mov_quarantine_entry_all_hag_year * mov_livestock_all_hag_weight_year_p
})


#### Visits previous herds link expression ####

visit_prev_link_exp <- quote({
  #Disease control plan sensitivity in visited farms (assumed to be unknown)
  plan_sensi <- 0
  herd_pos <- 0

  #Proportion of pregnant cows
  #not relevant for direct or indirect contact, but needed for origin calc
  pregnant_p <- 0

  #Previous farms visited by fomite (visit_enter_p from bibliography)
  farms_n <- visit_farms_n * visit_enter_p

  #Previous animals (by farm) contacted by fomite
  animals_n <- visit_animals_n * animal_category_p
})

#### Visits indirect contacts link expression ####
visit_indir_link_exp <- quote({
  #Probability fomites entering the farm (no excusive boots/equipment/vehicle_enters)
  indir <- visit_livestock * visit_enter

  # Probability fomite was contacted by infectious animal
  prev_infectious_total <- prev_infectious_hag_allherds

  # Probability fomite was contacted by BVD PI animal
  prev_pi_total <- prev_infected_pi_hag_allherds

  #Probability cleaning and disinfecting fomites in visited farms
  #visit_cleaning=1 ("Always")
  #visit_cleaning=0.5 ("Sometimes"),
  #visit_cleaning=0 ("Never"),
  #If visit_cleaning=-1 ("Unknown"), then visit_cleaning_p from bibliography
  #Equivalent to: ifelse(visit_cleaning<0,visit_cleaning_p,visit_cleaning)
  cleaning <- visit_cleaning *
    (visit_cleaning >= 0) +
    visit_cleaning_p * (visit_cleaning < 0)

  disinfection <- cleaning

  #Time between surface contamination and contact with susceptible animal
  risk_contact_time <- visit_time_between

  #Level of indirect contact risk (1=High, 2=Moderate, 3=Low, 4=Very low)
  #if they enter in direct contact with herd animals: Moderate,
  #if they are close to farm animals (eg. they help loading other animals): Low,
  #if they don't contact any animal: Very low
  indir_level <- (visit_direct * 2) +
    (visit_close * (1 - visit_direct) * 3) +
    ((1 - visit_close) * (1 - visit_direct) * 4)

  #Number of times indirect contact happens
  n_times <- visit_frequency
})

#### Neighbour link expression ####

neighbour_link_exp <- quote({
  #We assume probability of direct or indirect contact with neighbour farms
  #is dependent on the number of farms
  farms_n <- neighbour_n

  #PROBABILITY DIRECT CONTACT (one neighbour farm)
  direct <- neighbour_direct
  
  #If the farm ins infected at least one animal will for sure be infected
  other_infectious_total <- herd_inf
  other_pi_total <- herd_inf

  #ZONE EFFECT (PROBABILITY INDIRECT CONTACT one neighbour farm)
  area_effect <- neighbour_farms
  exponent <- farms_n
  indir_level <- 3
  area_prev <- herd_prev
})

#### Wildlife area link expression ####

wildlife_area_link_exp <- quote({
  #We assume probability of contact with
  #is dependent on the number of wildlife visits per day
  #and wildlife pathogen prevalence
  #ZONE EFFECT (PROBABILITY INDIRECT CONTACT infected wildlife)
  area_effect <- access * fencing_wildlife + access * (1 - fencing_perimeter)
  exponent <- contact_rate_wildlife
  indir_level <- 3
  area_prev <- wildlife_prev
})

#### Wildlife contact link expression ####

wildlife_contact_link_exp <- quote({
  farms_n <- 1

  contact_point_n <- livestock_units * contact_point_ratio * contact_point_p

  #1 Time link
  other_contact_rate <- mcnode_na_rm(contact_rate_wildlife / contact_point_n)
  other_infected <- wildlife_prev
  contact_rate <- mcnode_na_rm(contact_rate_livestock / contact_point_n)

  #2 Indirect contact with mud at waterer
  indir <- (access * fencing_wildlife + access * (1 - fencing_perimeter)) *
    mud_p *
    contact_point_access_wildlife

  #Probability cleaning and disinfecting contact_points
  cleaning <- 0
  disinfection <- 0

  #Probability of infected wild host visit
  prev_infectious_total <- other_contact_rate * other_infected
  prev_pi_total <- 0

  #Level of indirect contact risk (1=High, 2=Moderate, 3=Low, 4=Very low)
  #if they enter in direct contact with animal: medium, if not: minimun
  indir_level <- 2

  #Number of times indirect contact happens
  #surface_p represents the probability the indirect contact happens in a certain surface (eg. mud 50% vs pastic 50%)
  n_times <- risk_days
})


#### Pasture animal mix link expression ####

pasture_mix_link_exp <- quote({
  #Direct is the probability of each animal of having direct contact with animals from the other herd during pasture
  #If pasture_cattlebull=true, then we assume  75%  animals of our farm will have direct contact with the other farm animals
  #If pasture_cattlebull=false, then we assume  50%  animals of our farm will have direct contact with the other farm animals

  #PROBABILITY DIRECT CONTACT
  direct <- pasture_share
  pasture_mix_animals_n <- pasture_other_animals_n *
    ((pasture_cattlebull * 0.75) + ((1 - pasture_cattlebull) * 0.5))
  other_infectious_total <- 1 -
    (1 - no_detect_infectious)^(pasture_other_farm_n * pasture_mix_animals_n)
  other_pi_total <- 1 -
    (1 - no_detect_pi)^(pasture_other_farm_n * pasture_mix_animals_n)
})

#### Animal mix link expression ####

mix_link_exp <- quote({
  #Direct is the probability of each animal of having direct contact with animals from the other herd during recurrent
  #If site_bulls=true, then we assume  75%  animals of our farm will have direct contact with the other farm animals
  #If site_bulls=false, then we assume  50%  animals of our farm will have direct contact with the other farm animals

  #PROBABILITY DIRECT CONTACT
  direct <- site_share * mov_round
  other_infectious_total <- other_no_detect_infectious_hag_otherherd
  other_pi_total <- other_no_detect_pi_hag_otherherd
})

#### Mov type link expression ####

mov_type_link_exp <- quote({
  #If movement is round, farm module represent "other farm sharing sites"
  other_no_detect_infectious <- mov_round * no_detect_infectious
  other_no_detect <- mov_round * no_detect
  other_no_detect_pi <- mov_round * no_detect_pi
  other_no_detect_tr <- mov_round * no_detect_tr

  #If movement is one-way, farm module represent "farm where our new animals come from"
  own_no_detect_infectious <- (1 - mov_round) * no_detect_infectious
  own_no_detect <- (1 - mov_round) * no_detect
  own_no_detect_pi <- (1 - mov_round) * no_detect_pi
  own_no_detect_tr <- (1 - mov_round) * no_detect_tr

  #No risk - all 0 (to use later when prev_nodes are not calculated)
  no_risk <- mov_round * 0
})
