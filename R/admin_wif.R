#' What-If Scenario Functions for Biosecurity Scenarios
#' 
#' A collection of functions to simulate different what-if scenarios for biosecurity
#' in farms Each function applies specific biosecurity measures and returns
#' modified data based on the scenario.
#' 
#' Common Parameters:
#' @param data Data frame containing the original dataset
#' @param scenario Character string specifying the scenario name (optional)
#' @param wif_only Logical indicating if only what-if records should be returned
#' @param prev_wif Character string specifying a previous what-if scenario (optional)
#' @return Modified data frame with what-if scenario results

#' @title Test Implementation Scenario
#' @description Simulates implementation of testing protocols
wif_test <- function(data, ... , scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "test"
  
  data <- set_up_wif(data)
  
  # Filter rows for what-if scenario
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number) %>%
    filter(...)%>%
    filter(is.na(test) |
             (pregnant_p>0 & !test_useful_tr==1) |
             ((is.na(pregnant_p)|pregnant_p==0) & within_prev_pi_mode>0 & !test_useful_pi==1) |
             !(test_useful_i==1|test_useful_pi==1|test_useful_tr==1))
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  # Apply what-if modifications
  new_wif <- data_wif %>%
    mutate(
      scenario_id=scenario,
      wif=TRUE,
      test=as.character(ifelse(is.na(pregnant_p), 
                  as.character(test_pd),
                  ifelse(pregnant_p>0, 
                         as.character(test_pd_tr), 
                         as.character(test_pd)))
    )) %>%
    select(ends_with("_id"), animal_category, pathogen, test, status, health) %>%
    left_join(pathogen_test)%>%
    mutate(health=paste0(test, "+ (",health,")"))
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title Own Vehicle Usage Scenario
#' @description Simulates exclusive use of own vehicles
wif_own_veh <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "own_veh"
  
  data <- set_up_wif(data)
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number) %>%
    filter(...)%>%
    filter(!veh_exclusive=="yes"|!veh_type=="own"|(veh_type=="own"&veh_otherfarms))
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  if(nrow(data_wif)>0) {
    data_wif[grepl("veh_",names(data_wif))] <- NA
  }
  
  new_wif <- data_wif %>%
    select(!starts_with("veh_")) %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      veh_type="own",
      veh_exclusive="yes",
      veh_otherfarms=FALSE
    )
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title No Shared Vehicle Scenario
#' @description Simulates avoiding shared transport with other farms
wif_no_share_veh <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "no_share_veh"
  
  data <- set_up_wif(data)
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number) %>%
    #filter(...)%>%
    filter(is.na(veh_exclusive)|!(veh_exclusive=="yes"))
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  new_wif <- data_wif %>%
    select(!starts_with("veh_")) %>%
    transmute(
      wif=TRUE,
      scenario_id=scenario,
      veh_exclusive="yes"
    )
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title Cleaning and Disinfection Scenario
#' @description Simulates implementation of cleaning and disinfection protocols
wif_clean <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "clean"
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("cleaning"),ends_with("disinfection")))
  prefix_wif_cols <- unique(gsub("cleaning|disinfection","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)
    
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  clean_logical <- is.logical(data_wif$cleaning)
  
  data_wif <- data_wif %>%
    mutate(cleaning=ifelse(clean_logical,cleaning,cleaning=="each")) %>%
    filter((!cleaning|is.na(cleaning))) %>%
    filter(if_any(matches("equipment"), ~.x))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      cleaning=ifelse(clean_logical,TRUE,"each"),
      disinfection=ifelse(clean_logical,TRUE,"each")
    )
  
  names(new_wif)[names(new_wif)%in%c("cleaning","disinfection")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("cleaning","disinfection")])
  names(data_wif)[names(data_wif)%in%c("cleaning","disinfection")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("cleaning","disinfection")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title Quarantine Scenario
#' @description Simulates implementation of quarantine protocols for new animals
wif_quarantine <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "quarantine"
  
  data <- set_up_wif(data)
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)%>%
    filter(!quarantine|is.na(quarantine)|is.na(quarantine_direct)|quarantine_direct)
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      quarantine=TRUE,
      quarantine_direct=FALSE,
      quarantine_time=quarantine_time_df,
      quarantine_test_time="end",
      quarantine_frequency="few"
    )
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title Exclusive Material Usage Scenario
#' @description Simulates use of exclusive equipment and materials
wif_exclusive_material <- function(data,..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "exclusive_material"
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("equipment")))
  prefix_wif_cols <- unique(gsub("equipment","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  data_wif <- data_wif %>%
    filter(equipment|is.na(equipment))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      equipment=FALSE
    )
  
  names(new_wif)[names(new_wif)%in%c("equipment")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("equipment")])
  names(data_wif)[names(data_wif)%in%c("equipment")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("equipment")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title No Fomites Scenario
#' @description Simulates prevention of fomite introduction
#' @param fomites Character vector specifying types of fomites to control
wif_no_fomites <- function(data, ..., fomites=NULL, scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) {
    scenario <- paste(c("no_fomites",fomites), collapse="_")
  }
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("enter")))
  prefix_wif_cols <- unique(gsub("enter","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)

  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  if(is.null(fomites)) {
    fomites <- unique(data_wif$fomite_type)
  }
  
  data_wif <- data_wif %>%
    filter(fomite_type%in%fomites &
             (!enter=="never"|is.na(enter)))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      enter="never"
    )
  
  names(new_wif)[names(new_wif)%in%c("enter")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("enter")])
  names(data_wif)[names(data_wif)%in%c("enter")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("enter")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title No Direct Contact Scenario
#' @description Simulates prevention of direct animal contact
wif_no_dc <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "no_dc"
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("direct")))
  prefix_wif_cols <- unique(gsub("direct","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  data_wif <- data_wif %>%
    filter(direct|is.na(direct))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      direct=FALSE
    )
  
  names(new_wif)[names(new_wif)%in%c("direct")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("direct")])
  names(data_wif)[names(data_wif)%in%c("direct")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("direct")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title No Shared Pasture Scenario
#' @description Simulates prevention of shared pasture usage
wif_no_share_pasture <- function(data,...,scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "no_share_pasture"
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("pasture_share")))
  prefix_wif_cols <- unique(gsub("pasture_share","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)

  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  data_wif <- data_wif %>%
    filter(pasture_share|is.na(pasture_share))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      pasture_share=FALSE
    )
  
  names(new_wif)[names(new_wif)%in%c("pasture_share")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("pasture_share")])
  names(data_wif)[names(data_wif)%in%c("pasture_share")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("pasture_share")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' @title No Shared Site Scenario
#' @description Simulates prevention of shared site
wif_no_share_site <- function(data,..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- "no_share_site"
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("site_share")))
  prefix_wif_cols <- unique(gsub("site_share","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  data_wif <- data_wif %>%
    filter(site_share|is.na(site_share))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      site_share=FALSE
    )
  
  names(new_wif)[names(new_wif)%in%c("site_share")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("site_share")])
  names(data_wif)[names(data_wif)%in%c("site_share")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("site_share")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}


#' @title No Mud Scenario
#' @description Simulates prevention of mud accumulation
wif_no_mud <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) scenario <- paste("no_mud")
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data,ends_with("mud")))
  prefix_wif_cols <- unique(gsub("mud","",old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_",gsub("wif_","",prev_wif),"(data_wif,wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"",old_wif_cols)
  
  data_wif <- data_wif %>%
    filter(contact_point_type%in%c("low","high")&mud)
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      mud=FALSE,
      mud_p=0
    )
  
  names(new_wif)[names(new_wif)%in%c("mud")] <- 
    paste0(prefix_wif_cols,names(new_wif)[names(new_wif)%in%c("mud")])
  names(data_wif)[names(data_wif)%in%c("mud")] <- 
    paste0(prefix_wif_cols,names(data_wif)[names(data_wif)%in%c("mud")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}
#' @title High Waterers Scenario
#' @description Simulates elevation of water points to prevent disease transmission
#' @param data Data frame containing the original dataset
#' @param scenario Character string specifying the scenario name (optional)
#' @param wif_only Logical indicating if only what-if records should be returned
#' @param prev_wif Character string specifying a previous what-if scenario (optional)
#' @return Modified data frame with what-if scenario results
wif_high_waterer <- function(data, ..., scenario=NULL, wif_only=FALSE, prev_wif=NULL) {
  if(is.null(scenario)) {
    scenario <- "high_waterer"
  }
  
  data <- set_up_wif(data)
  
  old_wif_cols <- names(select(data, ends_with("contact_point_type")))
  prefix_wif_cols <- unique(gsub("contact_point_type","", old_wif_cols))
  
  data_wif <- data %>%
    filter(scenario_id=="0") %>%
    mutate(hg=if("hg"%in%names(data)) hg else row_number)%>%
    filter(...)
  
  if(!is.null(prev_wif)) {
    wif_expression <- paste0("wif_", gsub("wif_","", prev_wif), "(data_wif, wif_only=TRUE)")
    data_wif <- eval(parse(text=wif_expression))
  }
  
  names(data_wif)[names(data_wif)%in%old_wif_cols] <- gsub(prefix_wif_cols,"", old_wif_cols)
  
  data_wif <- data_wif %>%
    filter(contact_point_type%in%c("low"))
  
  new_wif <- data_wif %>%
    transmute(
      scenario_id=scenario,
      wif=TRUE,
      contact_point_type="high",
      wildlife=wildlife
    ) %>%
    left_join(wildlife_point)
  
  names(new_wif)[names(new_wif)%in%c("contact_point_type")] <- 
    paste0(prefix_wif_cols, names(new_wif)[names(new_wif)%in%c("contact_point_type")])
  names(data_wif)[names(data_wif)%in%c("contact_point_type")] <- 
    paste0(prefix_wif_cols, names(data_wif)[names(data_wif)%in%c("contact_point_type")])
  
  output_wif_df <- output_wif(data, scenario, data_wif, new_wif, wif_only)
  return(output_wif_df)
}

#' Combine scenarios by applying multiple what-if functions sequentially
#' 
#' @title Combine What-If Scenarios
#' @description Applies multiple what-if (wif) functions in sequence to generate combined scenarios
#'
#' @param data Data frame containing mcnodes input data
#' @param scenario Character string naming the combined scenario. If NULL, constructed from wif_list
#' @param wif_list Character vector of what-if function names to apply in sequence
#' @param wif_only Logical indicating whether to return only the new scenario (TRUE) or all scenarios (FALSE)
#'
#' @return Data frame with either:
#'   \itemize{
#'     \item If wif_only=FALSE: Original data plus new wif scenario rows
#'     \item If wif_only=TRUE: Only the new wif scenario rows
#'   }
#'
#' @examples
#' combine_wif(
#'   data = quarantine_data,
#'   scenario = "Test during quarantine with exclusive material",
#'   wif_list = c("quarantine", "exclusive_material", "test")
#' )
combine_wif <- function(data, scenario = NULL, wif_list = NULL, wif_only = FALSE) {
  
  # Generate scenario name if not provided
  if (is.null(scenario)) {
    scenario <- paste0(gsub("wif_", "", wif_list), collapse = "_")
  }
  
  # Initialize homogeneous group ID if first scenario
  if (all(data$scenario_id == "0")) {
    data <- data %>%
      mutate(hg = row_number())
  }
  
  # Run all wif functions sequentially
  data_i <- data
  for (i in 1:length(wif_list)) {
    wif_expression <- paste0(
      "wif_", 
      gsub("wif_", "", wif_list[i]), 
      "(data_i, scenario='", wif_list[i], 
      "', wif_only=TRUE)"
    )
    data_i <- eval(parse(text = wif_expression))
    data_i <- mutate(data_i, scenario_id = "0") # Reset scenario_id
  }
  
  # Add final scenario ID
  data_wif <- data_i %>%
    mutate(scenario_id = scenario)
  
  # Check if new scenario was created
  if (nrow(data_wif) == 0) {
    wif_omit <- TRUE
    message("\nWif scenario omitted: '", scenario, 
            "' is already implemented in the current scenario.")
  } else {
    wif_omit <- FALSE
  }
  
  # Return appropriate output based on wif_only parameter
  if (wif_only) {
    if (wif_omit) {
      # If no new scenario, return current scenario
      data <- data %>%
        filter(scenario_id == "0") %>%
        mutate(hg = row_number())
    } else {
      # Return only new scenario
      data <- data_wif %>%
        relocate(hg)
    }
  } else {
    # Return all scenarios
    data <- data %>%
      bind_rows(data_wif) %>%
      relocate(hg)
  }
  
  return(data)
}

#' Initialize data for what-if scenarios
#'
#' @param data Data frame to initialize
#' @return Initialized data frame with homogeneous group IDs and wif flag
set_up_wif <- function(data) {
  if (all(data$scenario_id == "0")) {
    data <- data %>%
      mutate(
        row_number = row_number(),
        hg = if("hg"%in%names(data)) hg else row_number,  # Homogeneous group ID
        wif = FALSE         # Original value flag
      )
  }
  return(data)
}

#' Process what-if scenario output
#'
#' @param data Original data frame
#' @param scenario Scenario name
#' @param data_wif Modified data for new scenario
#' @param new_wif New values to apply
#' @param wif_only Whether to return only new scenario
#' @return Processed data frame with scenario changes applied
output_wif <- function(data, scenario, data_wif, new_wif, wif_only) {
  
  # Apply new values to scenario data
  data_wif[names(data_wif)[names(data_wif) %in% names(new_wif)]] <- 
    new_wif[names(data_wif)[names(data_wif) %in% names(new_wif)]]
  
  # Check if new scenario created
  if (nrow(data_wif) == 0) {
    wif_omit <- TRUE
    message("\nWif scenario omitted: '", scenario, 
            "' is already implemented in the current scenario.")
  } else {
    wif_omit <- FALSE
    
    # Add unchanged rows
    unchanged_hg <- data$hg[data$scenario_id == "0"][
      !data$hg[data$scenario_id == "0"] %in% data_wif$hg
    ]
    
    data_unchanged <- data %>%
      filter(scenario_id == "0" & hg %in% unchanged_hg) %>%
      mutate(
        scenario_id = scenario,
        wif = FALSE
      )
    
    # Combine changed and unchanged rows
    if (nrow(data_unchanged) > 0) {
      data_wif <- data_wif %>%
        bind_rows(data_unchanged)
    }
  }
  
  # Return appropriate output
  if (wif_only) {
    if (wif_omit) {
      # Return current scenario if no changes
      data <- data %>%
        filter(scenario_id == "0") %>%
        mutate(hg = if("hg"%in%names(data)) hg else row_number) %>%
        relocate(hg, scenario_id)
      
    } else {
      # Return only new scenario
      data <- data_wif %>%
        relocate(hg, scenario_id)
    }
  } else {
    # Return all scenarios
    data <- data %>%
      bind_rows(data_wif) %>%
      relocate(hg, scenario_id)
    
    if (!wif_omit) {
      message("\nWif scenario created: '", scenario, "'")
    }
  }
  
  return(data)
}
