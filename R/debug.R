#' Find mcnodes in a mcmodule that contain NAs
#' 
#' @param mcmodule
#'
#' @return names of the mcnodes containing NAs
#' 
#' @examples
#' mcmodule_nas(intro)


mcmodule_nas<-function(mcmodule){
  
  node_names<-names(mcmodule$node_list)
  
  mcnodes_na<-sapply(mcmodule$node_list[node_names], "[[", "mcnode")
  mcnodes_na<-sapply(mcnodes_na, "is.na")
  mcnodes_na<-sapply(mcnodes_na, "any")
  
  return(names(mcnodes_na[mcnodes_na]))

}

