#' Infected animals by indirect transmission with wild animals
#' 

wild_transmision<-function(n_riskp=1, wl_riskp_mud=1, c_visits=60, period=720){
  
  wl_riskp_plastic<-1-wl_riskp_mud

  #Survival curve parameters
  #on "mud" (soil poor proxy)
  s0_mud<-mcdata(merge_data$P0_soil, type="0", nvariates=nrow(merge_data))
  k_mud<-mcdata(merge_data$kmax_soil, type="0", nvariates=nrow(merge_data))
  
  #on plastic
  s0_plastic<-mcdata(merge_data$P0_rubber, type="0", nvariates=nrow(merge_data))
  k_plastic<-mcdata(merge_data$kmax_rubber, type="0", nvariates=nrow(merge_data))
  
  #Transmission probability
  trans_ic_i_min<-mcdata(merge_data$trans_ic_i_min, type="0", nvariates=nrow(merge_data))
  
  trans_ic_i_max<-mcdata(merge_data$trans_ic_i_max, type="0", nvariates=nrow(merge_data))
  
  Ptrans_ic_i<-mcstoc(runif, type="U", 
                      nvariates=nrow(merge_data), 
                      min=trans_ic_i_min, 
                      max=trans_ic_i_max, 
                      lhs=TRUE)
  
  #probability indirect contact happening before time t
  contact_wic<-function(t){
    ((1-w)^t*(log(1-c)+log(1-w))-log(1-c))*(1-c)^t-log(1-w)*(1-w)^t
  }
  
  #pathogen surviving until time t
  survival_wic<-function(t, s0, k){
    log10((10^s0- 1) * exp(-k * t) + 1)
  }
  
  #prevalence in wild animals
  wl_tp_lower<-mcdata(merge_data$mod_lower, type="0", nvariates=nrow(merge_data))
  wl_tp_mean<-mcdata(merge_data$mod_est, type="0", nvariates=nrow(merge_data))
  wl_tp_upper<-mcdata(merge_data$mod_upper, type="0", nvariates=nrow(merge_data))
  
  P_wl_tp<-mcstoc(rpert, type="U", 
                  nvariates=nrow(merge_data), 
                  min=wl_tp_lower,
                  mode=wl_tp_mean,
                  max=wl_tp_upper, 
                  lhs=TRUE)
  
  P_wl_tp[is.na(P_wl_tp)]<-0
  
  P_inf_wic_a<-mcdata(0, type="U", nvariates=nrow(merge_data))
  
  #Loop to calculate risk of indirect transmission by density level
  for(i in 1:5){
    max_visits<-paste0("max_visits_",i)
    min_visits<-paste0("min_visits_",i)
    level<-paste0("level_",i)
    
    w_visits_max<-mcdata(merge_data[[max_visits]], type="0", nvariates=nrow(merge_data))
    
    w_visits_min<-mcdata(merge_data[[min_visits]], type="0", nvariates=nrow(merge_data))
    
    w_visits<-mcstoc(runif, type="U", 
                     min=w_visits_min, 
                     max=w_visits_max, 
                     nvariates=nrow(merge_data))
    
    w_visits[is.na(w_visits)]<-0
    
    w_level<-mcdata(merge_data[[level]], type="0", nvariates=nrow(merge_data))
    
    w_level[is.na(w_level)]<-0
    
    w_total <- (w_visits/period)#wildboar/time unit
    c_total <- c_visits/period #cow/time unit
    
    w<-(w_total*P_wl_tp)/n_riskp #infected wildboar/risk point
    c<-c_total
    
    R_contact_wic<-mcdata(sample(1:period,nrow(merge_data)*ndunc(),replace=TRUE), type="U", nvariates=nrow(merge_data))
    
    P_contact_wic<-contact_wic(R_contact_wic)
    
    #Sample by variate (improve)
    #first variate
    R_contact_wic_1<-extractvar(R_contact_wic, 1)
    P_contact_wic_1<-extractvar(P_contact_wic, 1)
    if(any(P_contact_wic_1>0)){
      T_contact_wic_1<-mcstoc(rempiricalD,
                              values=unmc(R_contact_wic_1),
                              prob=unmc(P_contact_wic_1), 
                              type="U")
    }else{
      T_contact_wic_1<-mcdata(0, type="U")
    }
    
    T_contact_wic<-T_contact_wic_1
    
    #loop for all other variates
    for (i in 2:nrow(merge_data)) {
      R_contact_wic_i<-extractvar(R_contact_wic, i)
      P_contact_wic_i<-extractvar(P_contact_wic, i)
      if(any(P_contact_wic_i>0)){
        T_contact_wic_i<-mcstoc(rempiricalD,
                                values=unmc(R_contact_wic_i),
                                prob=unmc(P_contact_wic_i),
                                type="U")
      }else{
        T_contact_wic_i<-mcdata(0, type="U")
      }
      T_contact_wic<-addvar(T_contact_wic, T_contact_wic_i)
    }
    
    P_survival_wic_mud<-ifelse(P_contact_wic>0, survival_wic(T_contact_wic,s0_mud,k_mud),0)
    
    P_survival_wic_plastic<-ifelse(P_contact_wic>0, survival_wic(T_contact_wic,s0_plastic,k_plastic),0)
    
    P_survival_wic<-P_survival_wic_mud*wl_riskp_mud+
      P_survival_wic_plastic*wl_riskp_plastic
    
    P_inf_wic_a_level<-P_survival_wic*Ptrans_ic_i
    
    if(!any(is.na(w_level))){
      P_inf_wic_a<-P_inf_wic_a+(P_inf_wic_a_level*w_level)
    }
    
  }
  
  return(P_inf_wic_a)

}
