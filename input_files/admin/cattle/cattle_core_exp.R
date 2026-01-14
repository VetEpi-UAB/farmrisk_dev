
#### Origin #### 


origin_exp <- quote({
  #Probability of a herd being free of disease
  herd_free <- ((1-herd_prev )/(1-herd_prev*plan_sensi))*(1-herd_pos)
  
  #Probability of a herd being infected
  herd_inf <- 1-herd_free
  
  #probability that an animal in a herd is infected
  infected <- herd_inf*within_prev
  
  #Probability that an infectd animal is infectious
  infectious <- infected*infectious_inf
  
  #Probability that an animal in a herd is a BVD PI
  infected_pi <- herd_inf*within_prev_pi
  
  #Incidence rate (asuming steady-state)
  incidence_rate <- (within_prev/(1-within_prev))/inf_time
  
  #Cumulative incidence during susceptibility window
  incidence_cum <- 1-(1-incidence_rate)^sus_time
  
  #Within-herd prevalence of Trojan cows
  within_prev_tr <- incidence_cum*pregnant_p
  
  #Probability that cow in a herd is a BVD TR
  infected_tr <- herd_inf*within_prev_tr
  
  #Probability that an animal is infected in any form of the disease
  #(for BVD that means TI, PI, or TR)
  infected_all <- 1-(1-infected)*(1-infected_pi)*(1-infected_tr)
  
  
  #Output for module analysis
  output <- infected_all
  
})

#### Herd selection #### 

herd_select_exp <- quote({
  #Probability of a herd being free of disease
  herd_free <- ((1-herd_prev )/(1-herd_prev*plan_sensi))*(1-herd_pos)
  
  #Probability of a herd being infected
  herd_inf <- 1-herd_free
  
  #Output for module analysis
  output <- herd_inf
})

#### Animal selection #### 
#Assuming infected herd
animal_select_exp <- quote({
  #probability that an animal in an infected herd is infected
  infected <- within_prev
  
  #Probability that an infectd animal is infectious
  infectious <- infected*infectious_inf
  
  #Probability that an animal in an infected herd is a BVD PI
  infected_pi <- within_prev_pi
  
  #Incidence rate (asuming steady-state)
  inc_rate <- (within_prev/(1-within_prev))/inf_time
  
  #Cumulative incidence during susceptibility window
  inc_cum <- 1-(1-inc_rate)^sus_time
  
  #Within-herd prevalence of Trojan cows
  within_prev_tr <- inc_cum*pregnant_p
  
  #Probability that cow in an infected herd is a BVD TR
  infected_tr <- within_prev_tr
  
  #Probability that an animal is infected in any form of the disease
  #(for BVD that means TI, PI, or TR)
  infected_all <- 1-(1-infected)*(1-infected_pi)*(1-infected_tr)
  
  
  #Output for module analysis
  output <- infected_all
  
})



#### Test #### 


test_exp <- quote({
  
  #Test adjusted sensitivity for recent infection (if test time is over test optimun time, test sensi does not change)
  
  #Equivalent to: ifelse(test_time>test_opt_time, test_opt_time, test_time)
  test_time_adj <- test_time * (test_time <= test_opt_time) + test_opt_time * (test_time > test_opt_time)
  
  #NAs form 0/0 are 0
  test_sensi_adj <- mcnode_na_rm(test_sensi*(test_time_adj/test_opt_time))
  
  #Probability a tested infected animal is a false negative
  #NAs form 0/0 are 0
  false_neg <- mcnode_na_rm(
    test_useful_i*(infected*(1-test_sensi_adj))/((1-test_sensi_adj)*infected+(test_spec*(1-infected))))
  
  #Probability an infectd animal is not tested
  no_test <- (1-test_useful_i)*infected
  
  #Probability an infected animal is no detected (false negative or not tested)
  no_detect <- false_neg+no_test
  
  #Probability a not detected animal is infectious
  no_detect_infectious <- no_detect*infectious_inf
  
  #Probability an infected animal is detected (and removed)
  detect <- (1-false_neg)*infected
  
  #Probability a detected animal is infectious
  detect_infectious <- detect*infectious_inf

  #FOR BVD PI ANIMALS
  #Probability a tested PI animal is a false negative
  #NAs form 0/0 are 0
  false_neg_pi <- mcnode_na_rm(
    test_useful_pi*(infected_pi*(1-test_sensi))/((1-test_sensi)*infected_pi+(test_spec*(1-infected_pi))))
  
  #Probability a PI animal is not tested
  no_test_pi <- (1-test_useful_pi)*infected_pi
  
  #Probability a PI animal is no detected (false negative or not tested)
  no_detect_pi <- false_neg_pi+no_test_pi
  
  #Probability a PI animal is detected (and removed)
  detect_pi <- (1-false_neg_pi)*infected_pi
  
  
  #FOR BVD TR COWS
  #Probability a tested trojan cow is a false negative
  false_neg_tr <- mcnode_na_rm(
    test_useful_tr*(infected_tr*(1-test_sensi))/((1-test_sensi)*infected_tr+(test_spec*(1-infected_tr))))
  
  #Probability an trojan cow is not tested
  no_test_tr <- (1-test_useful_tr)*infected_tr
  
  #Probability an trojan cow is no detected (false negative or not tested)
  no_detect_tr <- false_neg_tr+no_test_tr
  
  #Probability an trojan cow is detected (and removed)
  detect_tr <- 1-no_detect_tr
  
  #Output for module analysis
  output <- 1-(1-no_detect)*(1-no_detect_pi)*(1-no_detect_tr)
  
})


#### Direct contact #### 


dir_contact_exp <- quote({
  #Probability one animal is infected due to direct contact with an infectious animal
  dir_contact <- direct*other_infectious_total*inf_dc
  
  #Probability one animal is infected due to direct contact with a BVD PI animal
  dir_contact_pi <- direct*other_pi_total*inf_dc_pi
  
  #Output for module analysis
  output <- 1-(1-dir_contact)*(1-dir_contact_pi)
  
})


#### Indirect contact #### 


indir_contact_exp <- quote({
  #Probability of cleaning and disinfection efficacy
  hygiene_eff <- cleaning*cleaning_eff*disinfection*disinfection_eff
  
  #Probability of pathogen survival at time contact
  survival_contact_time <- log10(1 + (10^survival_init- 1) * exp(-survival_k * risk_contact_time))
  
  #FOR INFECTIOUS ANIMALS
  #Probability of surface contamination by an infectious animal
  inf_surface_init <- prev_infectious_total*indir
  
  #Probability one animal is infected due to one contact with a contaminated surface
  indir_contact_one <- inf_surface_init*(1-hygiene_eff)*survival_contact_time*(inf_ic)^indir_level
  
  #Probability one animal is infected due to contact in any of times it has contact with the surface
  indir_contact <- 1-(1-indir_contact_one)^n_times
  
  
  
  #FOR BVD PI ANIMALS
  #Probability of surface contamination by a pi animal
  inf_surface_init_pi <- prev_pi_total*indir
  
  #Probability one animal is infected due to one contact with a contaminated surface
  indir_contact_one_pi <- inf_surface_init_pi*(1-hygiene_eff)*survival_contact_time*(inf_ic_pi)^indir_level
  
  #Probability one animal is infected due to contact in any of times it has contact with the surface
  indir_contact_pi <- 1-(1-indir_contact_one_pi)^n_times
  
  #Output for module analysis
  output <- 1-(1-indir_contact)*(1-indir_contact_pi)

})



#### Time #### 


time_exp <- quote({
  
  #Rate at which infected animals contact with the risk point surface
  contamination_rate <- other_contact_rate*other_infected
  
  #Rate at which animals contact with the risk point surface
  risk_contact_rate <- contamination_rate+contact_rate
  
  #Time between contamination and risk contact
  #Assuming random exponential variable (Poisson process)
  #risk_contact_time <- mcstoc(rexp, rate=risk_contact_rate)
  
  #Using only mean contact time to reduce variability
  risk_contact_time <- 1/risk_contact_rate
  
})



#### Area effect infection #### 

area_exp <- quote({
  #Probability a farm gets infected if it has n_times close herds, 
  #with a herd_inf probability of being infected
  area_inf <- 1-((1-(1-(1-area_effect*area_prev*inf_ic^indir_level)))^exponent)^(risk_days/360)
  
  #Output for module analysis
  output <- area_inf
}) 