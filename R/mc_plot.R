#' Plot mcnode summary by key
#' 
#' @param mcmodule
#' @param mc_name
#'
#' @return summary data frame
#' 
#' @examples
#' mc_to_long_df(mcmodule=visit,mc_name="visit_a_indir_contact")


mc_to_long_df<-function(mcmodule, mc_name, hg_keys=FALSE, keys_names=NULL){
  
  node_summary<-mcmodule$node_list[[mc_name]][["summary"]]
  
  if(hg_keys){
    keys_names<-mcmodule$node_list[[mc_name]][["keys"]]
    message("keys_names: ", paste(keys_names, collapse = ", "))
  }else{
    if(is.null(keys_names)){
      if(is.null(mcmodule$node_list[[mc_name]][["agg_keys"]])){
        keys_names=c("pathogen","scenario_id")
      }else{
        keys_names<-mcmodule$node_list[[mc_name]][["agg_keys"]]
        message("keys_names: ", paste(keys_names, collapse = ", "))
      }
    }
  }
  
  if(is.null(node_summary)){
    data_index<-data.frame(scenario_id=mcmodule$node_list[[mc_name]][["scenario"]],
                     hg=mcmodule$node_list[[mc_name]][["hg"]])
    
    if(is.data.frame(mcmodule$data)){
      data_module<-mcmodule$data
    }else{
      data_module<-mcmodule$data[[mcmodule$node_list[[mc_name]][["prefix"]]]]
    }
    
    
    data<-left_join(data_index,data_module)
    
    node_summary<-mc_summary(data=data,
                           mcnode=mcmodule$node_list[[mc_name]][["mcnode"]],
                           mc_name=mc_name,
                           keys_names = keys_names)

  }


  if(length(node_summary$mean)>0){
    central_col<-node_summary$mean
  }else{
    central_col<-node_summary$NoUnc
    
  }
  
  my_mcnode<-as.data.frame(t(unmc(mcmodule$node_list[[mc_name]][["mcnode"]])))
  
  results<-cbind(node_summary[c(keys_names)], central_col,my_mcnode)%>%
    pivot_longer(cols=starts_with("V", ignore.case = FALSE))%>%
    unite(key, all_of(keys_names), remove = FALSE)
  
  return(results)
}
  
  

#' Plot mcnode summary by key
#' 
#' @param mcsummary data frame containing a mc_summary() result
#'
#' @return summary data frame
#' 
#' @examples
#' mc_summary_plot(mcnode=inf_dc)

require(ggplot2)

mc_summary_plot<-function(mcsummary=NULL, colour_key="mean", facet_key="pathogen", box_legend=FALSE, mcmodule=NULL, mc_name=NULL){
  
  #get mcsummary from module object
  if(is.null(mcsummary)){
    if(is.null(mc_name)|is.null(mcmodule)){
      stop("mcsummary or mcmodule and mc_name must be provided")
    }else{
      mcsummary<-mcmodule$node_list[[mc_name]][["summary"]]
    }
  }
  
  
  keys_name<-mc_summary_keys(mcsummary)
  
  if("keys"%in%keys_name){
    facet_key<-NULL
  }
  
  y_keys_name<-keys_name[!keys_name%in%c(colour_key,facet_key)]
  
  if(!length(y_keys_name)>0){
    y_keys_name<-keys_name[!keys_name%in%facet_key]
  }
  
  x_axis_title<-paste0("Probability ", unique(mcsummary$mc_name))
    
  if("region_code"%in%keys_name){
    mcsummary<-mcsummary%>%
      mutate(region_code=gid_to_name(region_code))
  }
  
  mcsummary<-mcsummary%>%
    unite(col="keys", all_of(keys_name), sep=", ", remove=FALSE)%>%
    unite(col="y_keys", all_of(y_keys_name), sep=", ", remove=FALSE)%>%
    mutate(
      keys=as.factor(keys),
      y_min=as.numeric(keys)-0.5,
      y_max=as.numeric(keys)+0.4
    )
  
  mc_plot<-mcsummary%>%
    ggplot(aes(ymin=y_min, ymax=y_max, fill=.data[[colour_key]]))+
    geom_rect(aes(xmin=`Min`, xmax=`Max`),stat="identity",alpha=0.1)+
    geom_rect(aes(xmin=`2.5%`, xmax=`97.5%`),stat="identity",alpha=0.2)+
    geom_rect(aes(xmin=`25%`, xmax=`75%`),stat="identity",alpha=0.3)+
    geom_rect(aes(xmin=`50%`, xmax=`50%`,colour=.data[[colour_key]]),stat="identity", linewidth =1)+
    geom_segment(aes(x=mean-sd,xend=mean+sd,y=(y_min+y_max)/2, yend=(y_min+y_max)/2), colour="grey20")+
    geom_point(aes(x=mean,y=(y_min+y_max)/2), colour="grey20", show.legend = FALSE)+
    labs(
      x=x_axis_title,
      y="")+
    theme_light()+
    theme(axis.ticks.y=element_blank())+
    scale_y_continuous(breaks=(mcsummary$y_min+mcsummary$y_max)/2,
                       labels=mcsummary$y_keys,
                       minor_breaks = NULL)
  
  if(!is.null(facet_key)){
    mc_plot<-mc_plot+facet_grid(cols=vars(.data[[facet_key]]),scales="free")
  }
  
  return(mc_plot)
  
}


#' Plot raw mcnode by key
#' 
#' @param raw_mcnode raw_mcnode to plot
#' 
#' @return summary data frame
#' 
#' @examples
#' 
#' mc_raw_plot(mcmodule=visit, mc_name="visit_inf_surface_init")


mc_raw_plot<-function(mcmodule, mc_name, nvar=NULL){
  if(is.data.frame(mcmodule$data)){
    pathogen_col<-mcmodule$data["pathogen"]
  }else{
    pathogen_col<-mcmodule$data[[1]]["pathogen"]
  }
  
  my_summary<-mcmodule$node_list[[mc_name]][["summary"]]
  
  if(length(my_summary$mean)>0){
    central_col<-my_summary$mean
  }else{
    central_col<-my_summary$NoUnc
    
  }
  
  my_mcnode<-as.data.frame(t(unmc(mcmodule$node_list[[mc_name]][["mcnode"]])))
  
  if(!is.null(nvar)){
    my_mcnode<-my_mcnode[,1:nvar]
  }
  
  
  results<-cbind(pathogen_col,central_col,my_mcnode)%>%
    mutate(keys=row_number())%>%
    pivot_longer(cols=starts_with("V"))
  
  results%>%
    ggplot(aes(x=reorder(keys,value), y=value, fill=value,  colour=value))+
    geom_point(aes(x=reorder(keys,value), y=value), position=position_jitter(width = .15, height = 0) ,size = 0.5, alpha = 0.2)+
    geom_boxplot(alpha=0.4, aes(colour= central_col), fill="white", outlier.alpha = 0) +
    scale_color_gradient(low="#119da4", high="#fe6847", guide = NULL)+
    scale_fill_gradient(low="#119da4", high="#fe6847", guide = NULL)+
    theme_minimal() +
    coord_flip()+
    facet_grid(cols=vars(pathogen),scales="free")+
    theme(
      axis.text.x = element_text(angle = 45, hjust=1),
      axis.text.y = element_text(size=12),
      legend.position="left",
      plot.title = element_text(size=12)
    )

}


#' Plot nice_boxplot by key
#' 
#' @param raw_mcnode raw_mcnode to plot
#' 
#' @return summary data frame
#' 
#' @examples
#' 
#' mc_boxplot(data=results)


mc_boxplot<-function(data, value="value", key_label="scenario_id", central_col="central_col"){

  my_labels<-data%>%
    group_by_at(c("key",key_label))%>%
    summarise(central_col=mean(value), .groups = "keep")
  
  my_labels<-my_labels[order(my_labels$central_col),][[key_label]]

  data%>%
    ggplot(aes(x=reorder(key,value), y=value, fill=value,  colour=value))+
    geom_point(aes(x=reorder(key,value), y=value), position=position_jitter(width = .15, height = 0) ,size = 0.5, alpha = 0.2)+
    geom_boxplot(alpha=0.4, aes(colour= central_col), fill="white", outlier.alpha = 0) +
    scale_color_gradient(low="#119da4", high="#fe6847", guide = NULL)+
    scale_fill_gradient(low="#119da4", high="#fe6847", guide = NULL)+
    scale_x_discrete(labels=my_labels)+
    theme_minimal() +
    coord_flip()+
    theme(
      axis.text.x = element_text(angle = 45, hjust=1),
      axis.text.y = element_text(size=12),
      legend.position="left",
      plot.title = element_text(size=12)
    )
  
}



#' Plot default output boxplots
#' 
#' @param raw_mcnode raw_mcnode to plot
#' 
#' @return summary data frame
#' 
#' @examples
#' 
#' wif_boxplots(mcmodule=intro, mcnode="total_agg")


wif_boxplot<-function(mcmodule, mcnode){
  result <- mc_to_long_df(mcmodule, mcnode)
  
  # Lists to store boxplots and summary tables
  boxplots <- list()
  summary_tables <- list()
  
  # Loop over unique values of "pathogen"
  for (i in 1:length(unique(result$pathogen))) {
    pathogen_i <- unique(result$pathogen)[i]
    
    # Filter data for the current pathogen
    result_i <- filter(result, pathogen == pathogen_i)
    
    # Perform pairwise Wilcoxon tests for each scenario
    pair_wilcox <- pairwise.wilcox.test(x = result_i$value, g = result_i$scenario_id, p.adjust.method = "holm")
    
    # Create logical vectors based on significance levels
    risk_reduction_05 <- c("0" = FALSE, pair_wilcox$p.value[, c("0")] < 0.05)
    risk_reduction_01 <- c("0" = FALSE, pair_wilcox$p.value[, c("0")] < 0.01)
    risk_reduction_001 <- c("0" = FALSE, pair_wilcox$p.value[, c("0")] < 0.001)
    
    # Generate a string of asterisks based on significance levels
    p_value_asterisks <- ifelse(risk_reduction_001, "***",
                                ifelse(risk_reduction_01, "**",
                                       ifelse(risk_reduction_05, "*", "")))
    
    # Modify the "scenario_id" by appending asterisks based on the p-values
    result_i$scenario_id <- paste0(result_i$scenario_id, p_value_asterisks[result_i$scenario_id])
    
    # Generate a boxplot using "mc_boxplot"
    boxplot_i <- mc_boxplot(result_i)+
      ggtitle(paste(farm_id, pathogen_i, mcnode))
    
    # Calculate height for the boxplot in inches
    height_inch <- 1 + length(unique(result_i$scenario_id)) * 0.5
    
    # Save the boxplot as a PNG file in the "output_files" directory
    ggsave(plot = boxplot_i, filename = paste0("output_files/", farm_id, "/", farm_id, "_", pathogen_i, ".png"),
           width = 9, height = height_inch)
    
    # Store the boxplot in the list
    boxplots[[as.character(pathogen_i)]] <- boxplot_i
    print(boxplot_i)
  }
  
  return(boxplots)
  
}
