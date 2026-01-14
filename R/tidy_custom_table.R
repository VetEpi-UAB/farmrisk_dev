#' @title Tidy and Merge User and Admin Inputs
#' @description A collection of functions for handling bsg and mov inputs and 
#' merge them with admin parameters by pathways and modules. This functions are based on survey.js 
#' answer structure and and its conversion to csv.
#' 
#' Tidy prefix
#'
#' Processes dataframe columns by selecting those with a specific prefix and optionally removing that prefix.
#' @param data Dataframe
#' @param pathway Optional pathway filter
#' @param module Optional module filter
#' @param rm_prefix Logical indicating whether to remove prefix from column names (TRUE by default)
#' @return A cleaned dataframe with standardized column names
#' @export
#' @examples
#' tidy_prefix(data = bsg, module = "quarantine")
tidy_prefix<-function(data, module=NULL, pathway=NULL, rm_prefix=TRUE){
  
  if(is.null(pathway)){
    key<-module
  }else if(is.null(module)){
    key<-pathway
  }else{
    key<-paste0(pathway,"_",module)
  }
  
  
  tidy_table<-data%>%
    select(ends_with("_id"), starts_with(key))
    
  if(rm_prefix==TRUE){
    tidy_table<-tidy_table%>%
      rename_with(~gsub(paste0(key,"_"),"",.), -ends_with("_id"))
    
  }else if(!rm_prefix==FALSE){
    tidy_table<-tidy_table%>%
      rename_with(~gsub(paste0(rm_prefix,"_"),"",.), -ends_with(paste0(rm_prefix,"_id")))
  }
  
  return(tidy_table)
  
}


#' Tidy panel
#' 
#' Creates a long dataframe from a wide dataframe with series of questions repeated n times.
#' 
#' @param data Dataframe containing a panel (bsg by default)
#' @param panel Name of the panel
#' @return Dataframe with rows by panel entries
#' @export
#' @examples
#' tidy_from_panel(data=bsg,panel="veh")
tidy_panel<-function(data=bsg, panel){
  data<-data%>%
    select(starts_with(panel)&matches(".*[0-9]"),ends_with("_id")) %>%
    pivot_longer(cols = starts_with(panel)&matches(".*[0-9]"),
                 names_to = c(".value", "panel_id"),
                 names_pattern = "(\\w+)_(\\d)")%>%
    mutate(panel_id=paste0(panel,"_",panel_id))
    
    if(!paste0(panel,"_id")%in%names(data)){
      data<-data%>%
        rename_at(vars("panel_id"),~paste0(panel,"_id"))
    }else{
      data<-data%>%
        mutate(panel_id=NULL)
    }
    return(data)
}



#' Tidy animal table
#' 
#' Restructures animal movements data into a table
#' 
#' @param data Dataframe containing animal movements records (bsg by default)
#' @param pathway Pathway filter
#' @param module Optional module filter
#' @return Dataframe with animal counts by category
#' @export
#' @examples
#' tidy_animals_table(data=mov,pathway="purchase",module="quarantine")

tidy_animal_table<-function(data=mov,pathway="mov",module=NULL){
  if(is.null(module)){
    key<-pathway
  }else{
    key<-paste0(pathway,"_",module)
  }
  

  animal_table<-data%>%
    select(starts_with(paste0(key,"_", animal$animal_category)),
           pregnant_n=matches(paste0(key,"_pregnant_n")),
           matches(paste0("^",key,"_farms_n")),
           ends_with("_id"))%>%
    relocate(pregnant_n,.after = last_col())%>%
    pivot_longer(cols = starts_with(paste0(key,"_", animal$animal_category)),
                 names_to = "animal_category",
                 values_to = "animals_n",
                 names_pattern = paste0(key,"_(.*)"))%>%
    mutate(animal_category = sub("_n$", "", animal_category))%>%
    rename(farms_total=paste0(key,"_farms_n"))%>%
    mutate(farms_total=ifelse(is.na(farms_total),1,farms_total),
           animals_n=animals_n/farms_total)%>%
    left_join(animal[c("animal_category","pregnant")],by="animal_category")%>%
    filter(!is.na(animals_n))
  
  #calculate proportion of pregnant animals
  pregnant_table<-animal_table%>%
    filter(pregnant)%>%
    group_by(across(c(-animal_category,-animals_n)))%>%
    summarise(.groups="keep",sus_pregnant_n=sum(animals_n*farms_total))%>%
    ungroup()%>%
    mutate(pregnant_p=pregnant_n/sus_pregnant_n,
           sus_pregnant_n=NULL)
  
  suppressMessages({
    animal_table<-animal_table%>%
      left_join(pregnant_table)%>%
      mutate(pregnant_n=NULL,
             pregnant=NULL)
  })
  
  return(animal_table)
    
}


#' Tidy animal census
#' 
#' Restructures animal census data into a table
#' 
#' @param data Dataframe containing animal census records (bsg by default)
#' @param census_key Identifier for census data ("farm_census" by default)
#' @return Dataframe with animal counts by category
#' @export
#' @examples
#' tidy_census_table(data = bsg, census_key = "farm_census")
tidy_census_table<-function(data=bsg, census_key="farm_census"){
  
  
  census_table<-data%>%
    select(starts_with(paste0(census_key,"_", animal$animal_category)),
           matches(paste0(census_key,"_farms_n")),
           ends_with("_id"))%>%
    pivot_longer(cols = starts_with(paste0(census_key,"_", animal$animal_category)),
                 names_to = "animal_category",
                 values_to = "animals_n",
                 names_pattern = paste0(census_key,"_(.*)"))%>%
    mutate(animal_category = sub("_n$", "", animal_category))%>%
    left_join(animal[c("animal_category","pregnant")],by="animal_category")%>%
    filter(!is.na(animals_n))
  
  
  return(census_table)
  
}

#' Tidy diagnostic test data
#' 
#' Restructures diagnostic test data into a table
#' 
#' @param data Dataframe containing diagnostic test records (mov by default)
#' @param pathway Pathway filter
#' @param module Module filter
#' @return Dataframe with diagnostic tests by movement, pathogen and farm
#' @export
#' @examples
#' tidy_test_table(data = mov, pathway = "mov", module = "origin")
tidy_test_table<-function(data=mov,pathway,module){
  
  tidy_farms_pre<-data%>%
    select(paste0(pathway,"_farms_n"),ends_with("_id"))
  
  tidy_farms<-data.frame()
  
  for(i in 1:nrow(tidy_farms_pre)){
    tidy_farms_i<-tidy_farms_pre[i,]
    farms_n<-tidy_farms_i[[paste0(pathway,"_farms_n")]]
    #If farms_n is NA only one farm by default
    farms_n<-ifelse(is.na(farms_n),1,farms_n)
    farms_df<-data.frame(farm=paste0("farm",1:farms_n))
    tidy_farms_i<-cross_join(tidy_farms_i,farms_df)
    tidy_farms<-rbind(tidy_farms,tidy_farms_i)
  }
  
  farms<-unique(tidy_farms[,"farm"])
  
  
  tidy_table<-data%>%
    select(starts_with(paste0(pathway,"_test")),ends_with("_id"))%>%
    pivot_longer(cols = matches(".*[0-9]"), 
                 names_to = c(".value", "row"),
                 names_pattern = "(\\w+)_(\\d)")%>%
    pivot_longer(cols = matches(".*farm[0-9].*|.*_all"), 
                 names_to = c("farm"),
                 values_to = c("farm_value"),
                 names_pattern = ".*_farm_(.*)")
  
  #If all is NA means that only one farm was answered (or empty)?
  if(all(is.na(tidy_table$farm_value))){
    tidy_table<-tidy_table%>%
      mutate(farm_value=ifelse(farm=="farm1",TRUE,NA))
    
  }

  tidy_table<-tidy_table%>%
    filter(farm_value)%>%
    select(ends_with("_id"),
           test=paste0(pathway,"_test_",module),
           farm=farm,
           pathogen=paste0(pathway,"_test_disease"))%>%
    mutate(test=as.character(test))
      

  tidy_table_all<-tidy_table%>%
    filter(farm=="all")%>%
    mutate(farm=NULL)%>%
    expand_grid(farms)%>%
    mutate(farm=farms,
           farms=NULL)

  tidy_table<-tidy_table%>%
    filter(!farm=="all")%>%
    rbind(tidy_table_all)
  
  tidy_table<-tidy_table[!duplicated(tidy_table),]
  
  if(nrow(tidy_table)=="0"){
    tidy_table<-data%>%
      select(ends_with("_id"))%>%
      mutate(test=as.character(NA))
  }
  
  tidy_table<-tidy_farms%>%
    left_join(tidy_table,relationship = "many-to-many")
  
  return(tidy_table)
}

#' Tidy health status data
#' 
#' Restructures Tidy health status data into a table
#' 
#' @param data Dataframe containing Tidy health status data records (mov by default)
#' @param pathway Pathway filter
#' @param module Module filter
#' @return Dataframe with health status by movement, pathogen and farm
#' @export
#' @examples
#' tidy_status_table(data = mov, pathway = "purchase")
tidy_status_table<-function(data=mov,pathway){
  
  tidy_farms_pre<-data%>%
    select(paste0(pathway,"_farms_n"),ends_with("_id"))
  
  tidy_farms<-data.frame()
  
  for(i in 1:nrow(tidy_farms_pre)){
    tidy_farms_i<-tidy_farms_pre[i,]
    farms_n<-tidy_farms_i[[paste0(pathway,"_farms_n")]]
    #If farms_n is NA only one farm by default
    farms_n<-ifelse(is.na(farms_n),1,farms_n)
    farms_df<-data.frame(farm=paste0("farm",1:farms_n))
    tidy_farms_i<-cross_join(tidy_farms_i,farms_df)
    tidy_farms<-rbind(tidy_farms,tidy_farms_i)
  }

  farms<-unique(tidy_farms[,"farm"])
  
  tidy_table<-data%>%
    select(starts_with(paste0(pathway,"_health")),ends_with("_id"))%>%
    pivot_longer(cols = matches(".*[0-9]"), 
                 names_to = c(".value", "row"),
                 names_pattern = "(\\w+)_(\\d)")%>%
    pivot_longer(cols = matches(paste0(".*farm[0-9].*|.*_all")), 
                 names_to = c("farm"),
                 values_to = c("farm_value"),
                 names_pattern = ".*_farm_(.*)")%>%
    # If all farm[0-9] are FALSE/empty, set farm_value to TRUE for "all"
    group_by(across(ends_with("_id")), row) %>%
    mutate(farm_value = if_else(
      farm == "all" & !any(farm_value[farm != "all"], na.rm = TRUE),
      TRUE,
      farm_value
    )) %>%
    ungroup() %>%
    filter(farm_value)%>%
    select(ends_with("_id"),
           farm=farm,
           pathogen=paste0(pathway,"_health_disease"),
           status=paste0(pathway,"_health_status"))%>%
    filter(!is.na(pathogen)&!is.na(status))
    
  
  tidy_table_all<-tidy_table%>%
    filter(farm=="all")%>%
    mutate(farm=NULL)%>%
    expand_grid(farms)%>%
    mutate(farm=farms,
           farms=NULL)
  
  tidy_table<-tidy_table%>%
    filter(!farm=="all")%>%
    rbind(tidy_table_all)
  
  tidy_table<-tidy_table[!duplicated(tidy_table[!names(tidy_table)%in%"status"]),]
  
  #If status table is not empty
  if(nrow(tidy_table)>0){
    tidy_table<-tidy_farms%>%
      cross_join(pathogen["pathogen"])%>%
      left_join(tidy_table)%>%
      mutate(status=as.factor(ifelse(is.na(as.character(status)),"unk",as.character(status))))%>%
      select(ends_with("_id"),farm,pathogen,status)
    
  #If status table is empty (no status information provided)
  }else{
    tidy_table<-tidy_farms%>%
      cross_join(pathogen["pathogen"])%>%
      mutate(status=as.factor("unk"))%>%
      select(ends_with("_id"),farm,pathogen,status)
  }
    

  
  return(tidy_table)
  
}

#' Tidy farm groups
#' 
#'Aggregates farms with the same characteristics to create homogeneous groups
#' 
#' @param data Dataframe containing one rows for each individual farm
#' @return Summarized with the number of farms and the number of animals in each homogeneous group
#' @export
#' @examples
#' tidy_group(data)
tidy_group<-function(data){
  data%>%
    mutate(farm=NULL)%>%
    group_by_all()%>%
    summarise(farms_n=n())%>%
    ungroup()%>%
    relocate(ends_with("_id"),"animals_n","farms_n")
}


#' Column selection keeping ids
#' 
#' Extracts specific columns from datasets while preserving id fields.
#' 
#' @param data Dataframe
#' @param col Columns to extract
#' @param pathway Optional pathway filter
#' @param module Optional module filter
#' @return Filtered dataframe with selected columns
#' @export
#' @examples
#' get_columns(data = mov, col = "purchase_single_date")
get_columns<-function(data,col, pathway=NULL, module=NULL){
  if(is.null(module)){
    key<-pathway
  }else if(!is.null(pathway)){
    key<-paste0(pathway,"_",module)
  }else{
    key<-""
  }

  data%>%
    select(ends_with("_id"),any_of(paste0(key,"_",col)),any_of(col))
    
}


#' Column type comparison checker
#' 
#' Identifies column type mismatches between two dataframes that could cause binding issues.
#' 
#' @param data.x First comparison dataframe
#' @param data.y Second comparison dataframe
#' @return Detailed report of column type differences
#' @export
#' @examples
#' diff_colum_type(data.x = visit_veh_data, data.y = visit_people_data)
diff_colum_type<-function(data.x,data.y){
    
  common_cols<-intersect(names(data.x),names(data.y))
  type_x<-sapply(data.x[common_cols], class)
  type_y<-sapply(data.y[common_cols], class)
  same_type<-type_x==type_y
  
  df<-data.frame(common_cols,
             type_x,
             type_y,
             same_type)
  
  df<-df[!df$same_type,]

  return(df)

}
