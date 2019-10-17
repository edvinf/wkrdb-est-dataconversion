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
    if (gear==codelist$GearType$purseseine){
       if (targetAssemblage=="SPF"){
         stop(paste("Mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
       }
      else{
        stop(paste("Mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
    }
    if (gear==codelist$GearType$pelagictrawl){
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
    if (gear==codelist$GearType$purseseine){
      if (targetAssemblage=="SPF"){
        stop(paste("Selectivity device mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
      else{
        stop(paste("Selectivity device mesh size range not supported for gear and assemblage:", gear, targetAssemblage))
      }
    }
    if (gear==codelist$GearType$pelagictrawl){
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
  else{
    return(NA)
  }
}

#' Convert gear to FAO codes (RDBES metier codes)
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
getGear <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
    return(codelist$GearType$purseseine)
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(codelist$GearType$pelagictrawl)
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
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#
# Target assemblage contract
# Functions for assigning target assemblage must accept two parameters 'nmdbioticStation' and 'nmdbioticCatchsample' which is are data.tables formatted as IMR biotic format as returned by Rstox parsing routines. 
# nmdbioticStation: should have one row corresponding to the fishingoperation / landing that assemblage is computed for
# nmdbioticCatchsample: should have one row for each catchsample collected at that station
# They must return a single value being a valid target assemblage
#

#' Generates a target assembalge functions that return a specified assemblage, no mather what its arguments are
generateTargetAssemblageSpecified <- function(assemblage){return(function(nmdbioticStation, nmdbioticCatchsample){return(assemblage)})}