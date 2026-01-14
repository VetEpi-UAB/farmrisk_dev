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
## ------------------------------------------------------------------------------------------------------------------
message("\nOUTPUTS: ")

#' 
## ------------------------------------------------------------------------------------------------------------------
message("\nMerging mcmodules: ")
#rm(wildlife)
#message("\nWILDLIFE MODULE REMOVED UNTIL #143 BUG IS SOLVED")
if(exists("wildlife")){
  message("\n- wildlife: YES")
  surroundings<-combine_modules(neighbour, wildlife)
  
  surroundings<-at_least_one(mcmodule=surroundings, mcnodes=c("neighbour_inf_agg","wildlife_inf_agg"), name="surroundings_inf_agg")
  
  outputs_surroundings<-c(neighbour_total="neighbour_inf_agg",
                          wildlife_total="wildlife_inf_agg")
  
  outputs_hg_surroundings<-c(farm_neigbour="neighbour_inf",
                          farm_wildlife_water="wildlife_water_b_indir_contact",
                          farm_wildlife_area= "wildlife_area_inf")

}else{
  message("\n- wildlife: NO")

  surroundings<-neighbour
  
  surroundings$node_list$surroundings_inf_agg<-
    surroundings$node_list$neighbour_inf_agg
  
  outputs_surroundings<-c(neighbour_total="neighbour_inf_agg")
  outputs_hg_surroundings<-c(farm_neigbour="neighbour_inf")

}

if(exists("visit")){
  message("\n- visit: YES")

  no_purchase<-combine_modules(surroundings, visit)
  
  no_purchase<-at_least_one(mcmodule=no_purchase, mcnodes=c("visit_a_indir_contact_all_agg","surroundings_inf_agg"), name="no_purchase_inf_agg")
  

  outputs_visits<-c(visit_total="visit_a_indir_contact_all_agg")

  outputs_hg_visits<-c(farm_visits="visit_a_indir_contact_all")
  
  outputs_agg_visits<-c(visit_type="visit_a_indir_contact_all_type",
                        visit_name="visit_a_indir_contact_all_name",
                        visit_veh="visit_a_indir_contact_all_veh")
  
}else{
  message("\n- visit: NO")
  
  no_purchase<-surroundings
  
  no_purchase$node_list$no_purchase_inf_agg<-
    surroundings$node_list$surroundings_inf_agg
  
  outputs_visits<-c()
  outputs_hg_visits<-c()
  outputs_agg_visits<-c()

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
           purchase_origin="quarantine_II_farm_b_no_detect_all_weight_agg",
           purchase_veh="quarantine_II_transport_b_contact_all_weight_agg")
  
  outputs_hg_purchase<-c(
           purchase_origin="quarantine_II_farm_b_no_detect_all_weight",
           purchase_veh="quarantine_II_transport_b_contact_all_weight")
  
  outputs_agg_purchase<-c(purchase_by_mov="b_entry_mov",
                          purchase_by_mov_animal="b_entry_mov_animal")
  
}else{
  message("\n- purchase: NO")
  
  no_pasture<-no_purchase
  
  no_pasture$node_list[["no_pasture_agg"]]<-no_pasture$node_list$no_purchase_inf_agg
  
  outputs_purchase<-c()
  
  outputs_hg_purchase<-c()
  
  outputs_agg_purchase<-c()
}

#If farm has pasture movements
if(exists("pasture")){
  message("\n- pasture: YES")
  
  intro<-combine_modules(pasture, no_pasture)

  intro<-at_least_one(mcmodule=intro, mcnodes=c("no_pasture_agg","pasture_inf_agg"), name="total_agg")
  
  outputs_pasture<-c(pasture_mix="pasture_mix_b_dir_contact_all_agg",
           pasture_veh="pasture_transport_b_contact_all_agg",
           pasture_livestock="pasture_b_livestock_all_agg",
           pasture_wildlife="pasture_wildlife_inf_agg",
           pasture_total="pasture_inf_agg")
  
  outputs_hg_pasture<-c(pasture_mix="pasture_mix_b_dir_contact_all",
         pasture_veh="pasture_transport_b_contact_all")
  
  outputs_agg_pasture<-c(pasture_livestock_by_mov="pasture_b_livestocK_all_mov",
                         pasture_wildlife_by_mov="pasture_wildlife_all_mov",
                         pasture_by_mov="pasture_all_mov")

  
  if(exists("purchase")){
      intro<-at_least_one(mcmodule=intro, mcnodes=c("b_entry_agg","pasture_inf_agg"), name="mov_total_agg")
  }
  
}else{
  message("\n- pasture: NO")
  
  intro<-no_pasture
  
  intro$node_list[["total_agg"]]<-intro$node_list$no_pasture_agg
  
  outputs_pasture<-c()
  outputs_hg_pasture<-c()
  outputs_agg_pasture<-c()

}

#Reassure class
class(intro)<-"mcmodule"

#' 

#' 
#' ### Select result mcnodes
#' 
## ------------------------------------------------------------------------------------------------------------------
outputs_all<-c(neighbour_total="neighbour_inf_agg",
               wildlife_total="wildlife_inf_agg",
               visit_total="visit_a_indir_contact_all_agg",
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

outputs_agg<-c(outputs_agg_visits,outputs_agg_purchase,outputs_agg_pasture)


outputs<-c(outputs_pasture,
           outputs_purchase,
           outputs_visits,
           outputs_surroundings,
           no_purchase_total="no_purchase_inf_agg",
           no_pasture_total="no_pasture_agg",
           total="total_agg")

#' 

#' 
#' ### Output by hg (all simulations)
#' 
## ------------------------------------------------------------------------------------------------------------------
if(exists("hg_csv")&&hg_csv){
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
#' ### Output medians
#' 
## ------------------------------------------------------------------------------------------------------------------
#Create a table with the mean of the selected mcnodes
wif_median<-intro$node_list[[outputs["total"]]][["summary"]]%>%
  select(pathogen, scenario_id, median = `50%`)

names(wif_median)[names(wif_median)=="median"]<-names(outputs["total"])



for(i in 1:length(outputs[!names(outputs)=="total"])){
  wif_median_i<-intro$node_list[[outputs[i]]][["summary"]]%>%
    select(pathogen, scenario_id, median = `50%`)
  names(wif_median_i)[names(wif_median_i)=="median"]<-names(outputs[i])
  
  wif_median<-left_join(wif_median, wif_median_i, by = c("pathogen", "scenario_id"))
}

#Add outputs that have not been calculated (probability equals zero)
wif_median[names(outputs_all[!outputs_all%in%outputs])]<-0
wif_median[is.na(wif_median)]<-0



#Create a table with the mean of the selected mcnodes
wif_median<-intro$node_list[[outputs["total"]]][["summary"]]%>%
  select(pathogen, scenario_id, median = `50%`)

names(wif_median)[names(wif_median)=="median"]<-names(outputs["total"])



for(i in 1:length(outputs[!names(outputs)=="total"])){
  wif_median_i<-intro$node_list[[outputs[i]]][["summary"]]%>%
  select(pathogen, scenario_id, median = `50%`)
  names(wif_median_i)[names(wif_median_i)=="median"]<-names(outputs[i])

  wif_median<-left_join(wif_median, wif_median_i, by = c("pathogen", "scenario_id"))
}

#Add outputs that have not been calculated (probability equals zero)
wif_median[names(outputs_all[!outputs_all%in%outputs])]<-0
wif_median[is.na(wif_median)]<-0


#' 
#' #### Output by scenario
#' 
## ------------------------------------------------------------------------------------------------------------------

#Current risk table
current_median <- wif_median%>%
  filter(scenario_id=="0")%>%
  mutate(scenario_id=NULL)

#Calculate relative risk
wif_median<-wif_median%>%
  left_join(current_median[c("pathogen","total")], by="pathogen",suffix=c("_wif","_current"))%>%
  mutate(
    relative=total_wif/total_current,
    relative=ifelse(relative>=1&scenario_id!="Current",
                             1-(1*10^-6),
                             relative),
  )

#Create table to plot risk reduction
wif_median_plot<-wif_median%>%
  select(pathogen,scenario_id,total=relative)%>%
  mutate(yes=total,
         no=1-total)%>%
  pivot_longer(
    cols = c(yes, no), 
    names_to = "yesno", 
    values_to = "value",
  )


#Current risk table
current_median <- wif_median%>%
  mutate(total=total_wif)%>%
  select(pathogen, scenario_id, total, 
         pasture_mix,pasture_veh,
         purchase_origin,purchase_veh,
         visit_total, neighbour_total,wildlife_total)%>%
  pivot_longer(cols=-c(pathogen, scenario_id, total))%>%
  filter(scenario_id=="0")%>%
  transmute(pathogen,
            pathway=name,
            abs_total=total,
            rel_pathway=value/total,
            abs_pathway=value)


#' 
## ------------------------------------------------------------------------------------------------------------------
summary_median<-wif_median%>%
  arrange(relative)%>%
  transmute(Pathogen=pathogen,
            Scenario=ifelse(scenario_id=="0","Current",scenario_id),
         `Risk disease entry`=paste0(Signif(total_wif*100,2),"%"),
         `Risk animal infected from origin`=paste0(Signif(purchase_origin*100,2),"%"),
         `Risk animal infected during purchase transport`=paste0(Signif(purchase_veh*100,2),"%"),
         `Risk disease entry by visits`=paste0(Signif(visit_total*100,2),"%"),
         `Risk disease entry by neigbour farms`=paste0(Signif(neighbour_total*100,2),"%"),
          `Risk disease entry by wildlife near the farm`=paste0(Signif(wildlife_total*100,2),"%"),
         `Risk disease entry by movement to pastures`=paste0(Signif(pasture_total*100,2),"%"))

summary_median[summary_median=="NA%"]<-"-"


#' 
#' #### Save summary tables
#' 
## ------------------------------------------------------------------------------------------------------------------
if(exists("save_tables")&&save_tables){

  save_table_summary(list(wif_median=wif_median, 
                          wif_median_plot=wif_median_plot, 
                          current_median=current_median,
                          summary_median=summary_median))
}

#' 
#' ### Print-cat summary
#' 
## ------------------------------------------------------------------------------------------------------------------
cat("\nResults")
print_summary<-unique(summary_median[summary_median$Scenario=="Current",
                                     !names(summary_median)%in%"Scenario"])
cat("\n\nIBR current median:\n")
print.table(print_summary[print_summary$Pathogen=="IBR",
                          !names(print_summary)%in%"Pathogen"])

cat("\n\nBVD current median:\n")
print.table(print_summary[print_summary$Pathogen=="BVD",
                          !names(print_summary)%in%"Pathogen"])

cat("\n\nTB current median:\n")
print.table(print_summary[print_summary$Pathogen=="TB",
                          !names(print_summary)%in%"Pathogen"])


  message("\n
\n####################################
\n FARM ", farm_id, " COMPLETED!
\n####################################
\n")

