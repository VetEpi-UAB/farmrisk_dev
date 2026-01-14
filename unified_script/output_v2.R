#' ---
#' title: "Export outputs"
#' author: "Natalia Ciria"
#' editor: visual
#' bibliography: references.bib
#' execute:
#'   output: false
#' ---
#' 

#' 
#' ### Combine all pathways
#' 
## --------------------------------------------------------------------------------------------
message("\nOUTPUTS: ")

#' 
## --------------------------------------------------------------------------------------------
message("\nMerging mcmodules: ")
if(exists("wildlife")){
  message("\n- wildlife: YES")
  surroundings<-combine_modules(neighbour, wildlife)
  
  surroundings<-at_least_one(mcmodule=surroundings, mcnodes=c("neighbour_inf_agg","wildlife_inf_agg"), name="surroundings_inf_agg")
  
  outputs_surroundings<-c(neigbour_total="neighbour_inf_agg",
                          wildlife_total="wildlife_inf_agg")
  
  outputs_hg_surroundings<-c(farm_neigbour="neighbour_inf",
                          farm_wildlife_water="wildlife_water_b_indir_contact",
                          farm_wildlife_area= "wildlife_area_inf")

}else{
  message("\n- wildlife: NO")

  surroundings<-neighbour
  
  surroundings$node_list$surroundings_inf_agg<-
    surroundings$node_list$neighbour_inf_agg
  
  outputs_surroundings<-c(neigbour_total="neighbour_inf_agg")
  outputs_hg_surroundings<-c(farm_neigbour="neighbour_inf")

}

if(exists("visit")){
  message("\n- visit: YES")

  no_purchase<-combine_modules(surroundings, visit)
  
  no_purchase<-at_least_one(mcmodule=no_purchase, mcnodes=c("visit_a_indir_contact_all_agg","surroundings_inf_agg"), name="no_purchase_inf_agg")
  
  outputs_visits<-c(visits_total="visit_a_indir_contact_all_agg")
                    #visits_type="visit_a_indir_contact_type"
  outputs_hg_visits<-c(farm_visits="visit_a_indir_contact_all")
  
}else{
  message("\n- visit: NO")
  
  no_purchase<-surroundings
  
  no_purchase$node_list$no_purchase_inf_agg<-
    surroundings$node_list$surroundings_inf_agg
  
  outputs_visits<-c()
}


#If farm has purchase movements
if(exists("purchase")){
  message("\n- purchase: YES")

  no_pasture<-combine_modules(purchase, no_purchase)

  no_pasture<-at_least_one(mcmodule=no_pasture, mcnodes=c("no_purchase_inf_agg","b_entry_agg"), name="no_pasture_agg")
    
  outputs_purchase<-c(purchase_origin_prequaran="farm_b_no_detect_all_agg",
           purchase_veh_prequaran="transport_b_contact_all_agg",
           purchase_prequaran="purchase_b_inf_all_agg",
           purchase_total="b_entry_agg",
           purchase_origin="quarantine_farm_b_no_detect_all_weight_agg",
           purchase_veh="quarantine_transport_b_contact_all_weight_agg")
  
  outputs_hg_purchase<-c(
           purchase_origin="quarantine_farm_b_no_detect_all_weight",
           purchase_veh="quarantine_transport_b_contact_all_weight")
  
}else{
  message("\n- purchase: NO")
  
  no_pasture<-no_purchase
  
  no_pasture$node_list[["no_pasture_agg"]]<-no_pasture$node_list$no_purchase_inf_agg
  
  outputs_purchase<-c()
}

#If farm has pasture movements
if(exists("pasture")){
  message("\n- pasture: YES")
  
  intro<-combine_modules(pasture, no_pasture)

  intro<-at_least_one(mcmodule=intro, mcnodes=c("no_pasture_agg","pasture_b_livestock_all_agg"), name="total_agg")
  
  outputs_pasture<-c(pasture_mix="pasture_mix_b_dir_contact_all_agg",
           pasture_veh="pasture_transport_b_contact_all_agg",
           #pasture_wildlife="purchase_b_inf_all_agg",
           pasture_total="pasture_b_livestock_all_agg")
  
  outputs_hg_pasture<-c(pasture_mix="pasture_mix_b_dir_contact_all",
         pasture_veh="pasture_transport_b_contact_all")
  
  if(exists("purchase")){
      intro<-at_least_one(mcmodule=intro, mcnodes=c("b_entry_agg","pasture_b_livestock_all_agg"), name="mov_total_agg")
  }
  
}else{
  message("\n- pasture: NO")
  
  intro<-no_pasture
  
  intro$node_list[["total_agg"]]<-intro$node_list$no_pasture_agg
  
  outputs_purchase<-c()
}

#' 

#' 
#' ### Select result mcnodes
#' 
## --------------------------------------------------------------------------------------------
outputs_all<-c(neigbour_total="neighbour_inf_agg",
               wildlife_total="wildlife_inf_agg",
               visits_total="visit_a_indir_contact_all_agg",
               purchase_origin="farm_b_no_detect_all_agg",
               purchase_veh="transport_b_contact_all_agg",
               purchase_prequaran="purchase_b_inf_all_agg",
               purchase_total="b_entry_agg",
               no_purchase_total="no_purchase_inf_agg",
               pasture_mix="pasture_mix_b_dir_contact_all_agg",
               pasture_veh="pasture_transport_b_contact_all_agg",
               #pasture_wildlife="purchase_b_inf_all_agg",
               pasture_total="pasture_b_livestock_all_agg",
               total="total_agg")


outputs<-c(outputs_pasture,
           outputs_purchase,
           outputs_visits,
           outputs_surroundings,
           no_purchase_total="no_purchase_inf_agg",
           no_pasture_total="no_pasture_agg",
           mov_total="mov_total_agg",
           #total="no_pasture_agg")
           total="total_agg")

#' 

#' 
#' ### Output by hg
#' 
## --------------------------------------------------------------------------------------------
if(hg_csv){
  outputs_hg<-c(outputs_hg_pasture,
           outputs_hg_purchase,
           outputs_hg_visits,
           outputs_hg_surroundings)
  
  hg_df<-data.frame()
for(i in 1:length(outputs_hg)){
  hg_df_i<-mc_to_long_df(intro, outputs_hg[i], hg_keys=TRUE)
  hg_df_i$mcnode<-outputs_hg[i]
  hg_df_i$pathway<-names(outputs_hg[i])
  hg_df_i$farm_id<-farm_id
  hg_df_i$timestamp<-Sys.time()
  hg_df<-bind_rows(hg_df,hg_df_i)
}
  
  write.table(hg_df, file="output_files/hg_df.csv", row.names=FALSE, col.names=!file.exists("output_files/hg_df.csv"), append=append_csv, sep=";")
  
  message(nrow(hg_df), "hg simulations of farm ", farm_id," added to output_files/hg_df.csv")
}

#' 
#' ### Output summary
#' 
## --------------------------------------------------------------------------------------------


