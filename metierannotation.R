#'
#' Keep the functions for individual components of metiers honest (return NA when you don't know)
#' Any assumptions that needs to be made to fill mandatory fields should go in getMetierLvl5 and getMetierLvl6
#'
#'


#' Get metier level 5 for gear code and target assemblage
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @param targetAssemblage target assemblage to use
#' @return may return NA, if metier could not be defined
getMetierLvl5 <- function(NMDreferenceGearCode, targetAssemblage){
  gear <- getGear(NMDreferenceGearCode)
  
  if (is.na(targetAssemblage)){
    stop("Need target assembalge to get metier lvl 5")
  }

  return(paste(gear, targetAssemblage, sep="_"))  
}

#' Get metier level 6 for gear code and target assemblage
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @param targetAssemblage target assemblage to use
#' @return may return NA, if metier could not be defined
getMetierLvl6 <- function(NMDreferenceGearCode, targetAssemblage){
  gear <- getGear(NMDreferenceGearCode)
  
  if (is.na(targetAssemblage)){
    stop("Need target assembalge to get metier lvl 5")
  }
  
  meshrange <- NA
  meshsize <- getMeshSize(NMDreferenceGearCode)
  if (!is.na(meshsize)){
    if (gear==RDBESexchange:::codelist$GearType$purseseine){
       if (targetAssemblage=="SPF"){
         stop(paste("Mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
       }
      else{
        stop(paste("Mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
    }
    if (gear==RDBESexchange:::codelist$GearType$pelagictrawl){
      if (targetAssemblage=="SPF"){
        stop(paste("Mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
      else{
        stop(paste("Mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
    }
  }
  
  seldev <- getSelDev(NMDreferenceGearCode)
  
  seldevmesh <- NA
  if (!is.na(seldevmesh)){
    if (gear==RDBESexchange:::codelist$GearType$purseseine){
      if (targetAssemblage=="SPF"){
        stop(paste("Selectivity device mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
      else{
        stop(paste("Selectivity device mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
    }
    if (gear==RDBESexchange:::codelist$GearType$pelagictrawl){
      if (targetAssemblage=="SPF"){
        stop(paste("Selectivity device mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
      else{
        stop(paste("Selectivity device mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
    }
  }
  
  if (!is.na(gear) & !is.na(targetAssemblage) & !is.na(meshrange) & !is.na(seldev) & !is.na(seldevmesh)){
    return(paste(gear, targetAssemblage, meshrange, seldev, seldevmesh, sep="_"))  
  }
  else if (!is.na(gear) & !is.na(targetAssemblage) & !is.na(seldev)){
    return(paste(gear, targetAssemblage, "0", seldev, "0", sep="_"))  
  }
  else if (!is.na(gear) & !is.na(targetAssemblage)){
    return(paste(gear, targetAssemblage, "0", "0", "0", sep="_"))  
  }
  else{
    return(NA)
  }
}

#' Convert gear to FAO codes (RDBES metier codes)
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
getGear <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
    return(RDBESexchange:::codelist$GearType$purseseine)
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(RDBESexchange:::codelist$GearType$pelagictrawl)
  }
  if (NMDreferenceGearCode %in% c(3530)){
    return(RDBESexchange:::codelist$GearType$pelagictrawl)
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(RDBESexchange:::codelist$GearType$demershalseine)
  }
  if (NMDreferenceGearCode %in% c(5110)){
    return(RDBESexchange:::codelist$GearType$longline)
  }
  if (NMDreferenceGearCode %in% c(5211)){
    return(RDBESexchange:::codelist$GearType$mechjig)
  }
  if (NMDreferenceGearCode %in% c(4127,4143)){
    return(RDBESexchange:::codelist$GearType$setnet)
  }
  
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#' Look up mesh size for gear code
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @return may return NA, if mesh size not defined for gear code
getMeshSize <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3530)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("51")){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("52")){
    return(NA)
  }
  if (NMDreferenceGearCode == 4127){
    return(96)
  }
  if (NMDreferenceGearCode == 4143){
    return(96)
  }
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#' Looknup selectivity device for gear code
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @return may return NA, if selectivity device is not defined for gear code
getSelDev <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
  return(0) 
}
  if (NMDreferenceGearCode %in% c(3500)){
    return(NA) 
  }
  if (NMDreferenceGearCode %in% c(3530)){
    return(NA) 
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("51")){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("52")){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("41")){
    return(NA)
  }

  
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#' Lookup selectivity device mesh size for gear code
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @return may return NA, if selectivity device is not defined for gear code
getSelDevMeshSize <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
    return(0) 
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3530)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("51")){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("52")){
    return(NA)
  }
  if (substr(NMDreferenceGearCode,1,2) %in% c("41")){
    return(NA)
  }
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#
# Target assemblage contract
# Functions for assigning target assemblage must accept one parameter 'nmdbiotic' which is IMR biotic format as returned by Rstox parsing routines. They must return a single value being a valid target assemblage
# This abstraction will probably not be taken any further, and only used for generating constant assemblages as below.
#

#' Generates a target assembalge functions that return a specified assemblage, no mather what its arguments are
generateTargetAssemblageSpecified <- function(assemblage){return(function(nmdbiotic){return(assemblage)})}