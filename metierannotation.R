#'
#' Keep the functions for individual components of metiers honest (return NA when you don't know)
#' Any assumptions that needs to be made to fill mandatory fields should go in getMetierLvl5 and getMetierLvl6
#'
#'

warning("Simplistic metier lvl 5 impl")
#' Get metier level 5 for gear code and target assemblage
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @param targetAssemblage target assemblage to use
#' @return may return NA, if metier could not be defined
getMetierLvl5 <- function(NMDreferenceGearCode, targetAssemblage){
  # Fix with lookup table from prepReca (possibly extract to metier annotation tool). Need to set to legal metiers based on gear and assemblage
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
  if (NMDreferenceGearCode %in% c(3100)){
    return(codelist$GearType$ottertrawlbottom) 
  }
  if (NMDreferenceGearCode %in% c(3400)){
    warning("exporting 3400 as OTB")
    return(codelist$GearType$ottertrawlbottom) 
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(codelist$GearType$pelagictrawl)
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(codelist$GearType$scottishseine)
  }
  if (NMDreferenceGearCode %in% c(5100)){
    return(codelist$GearType$setlongline)
  }
  if (NMDreferenceGearCode %in% c(5101)){
    return(codelist$GearType$setlongline)
  }
  if (NMDreferenceGearCode %in% c(5110)){
    return(codelist$GearType$setlongline)
  }
  if (NMDreferenceGearCode %in% c(5111)){
    return(codelist$GearType$setlongline)
  }
  if (NMDreferenceGearCode %in% c(5210)){
    return(codelist$GearType$mechjig)
  }
  if (NMDreferenceGearCode %in% c(5211)){
    return(codelist$GearType$mechjig)
  }
  if (NMDreferenceGearCode %in% c(5212)){
    return(codelist$GearType$manhjig)
  }
  if (NMDreferenceGearCode %in% c(4000)){
    return(codelist$GearType$gillnetunspecified)
  }
  if (NMDreferenceGearCode %in% c(4110:4129, 4140:4149)){
    return(codelist$GearType$setgillnet)
  }
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#' Look up mesh size for gear code
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @return may return NA, if mesh size not defined for gear code, or if mesh size is not specified for gear code
getMeshSize <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3100)){
    return(NA) 
  }
  if (NMDreferenceGearCode %in% c(3400)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5100)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5101)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5110)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5111)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5210)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5211)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5212)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(4000)){
    return(NA) #upsesifisert
  }
  if (NMDreferenceGearCode %in% c(4110)){
    return(NA) #upsesifisert
  }
  if (NMDreferenceGearCode %in% c(4113)){
    return(2*627.5/(6*3/4) ) #6 3/4 omfar
  }
  if (NMDreferenceGearCode %in% c(4114)){
    return(2*627.5/7 ) #7 omfar
  }
  if (NMDreferenceGearCode %in% c(4115)){
    return(2*627.5/10 ) #10 omfar
  }
  if (NMDreferenceGearCode %in% c(4120)){
    return(2*627.5/7 ) #7 omfar
  }
  if (NMDreferenceGearCode %in% c(4121)){
    return(2*627.5/9 ) #9 omfar
  }
  if (NMDreferenceGearCode %in% c(4123)){
    return(76*2 ) #76 maskevidde
  }
  if (NMDreferenceGearCode %in% c(4124)){
    return(110*2 ) #110 maskevidde
  }
  if (NMDreferenceGearCode %in% c(4125)){
    return(84*2 ) #84 maskevidde
  }
  if (NMDreferenceGearCode %in% c(4126)){
    return(66*2 ) #66 maskevidde
  }
  if (NMDreferenceGearCode %in% c(4127)){
    return(96*2) #96 maskevidde
  }
  if (NMDreferenceGearCode %in% c(4128)){
    return(105*2) #105 maskevidde
  }
  if (NMDreferenceGearCode %in% c(4140)){
    return(70*2 ) #70 halvmaske
  }
  if (NMDreferenceGearCode %in% c(4141)){
    return(80*2) #80 halvmaske
  }
  if (NMDreferenceGearCode %in% c(4142)){
    return(90*2) #90 halvmaske
  }
  if (NMDreferenceGearCode %in% c(4143)){
    return(100*2) #100 halvmaske
  }
  if (NMDreferenceGearCode %in% c(4144)){
    return(110*2 ) #110 halvmaske
  }
  if (NMDreferenceGearCode %in% c(4145)){
    return(120*2 ) #120 halvmaske
  }
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#' Looknup selectivity device for gear code
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @return may return NA, if selectivity device is not defined for gear code, or if use of selectivity device is not specified for gear code
getSelDev <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3712,3710)){
  return(0) 
  }
  if (NMDreferenceGearCode %in% c(3100)){
    return(NA) 
  }
  if (NMDreferenceGearCode %in% c(3400)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(NA) 
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5100)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5101)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5110)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5111)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5210)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5211)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5212)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(4000)){
    return(NA) #upsesifisert
  }
  if (NMDreferenceGearCode %in% c(4110:4128, 4140:4149)){
    return(NA)
  }
  else{
    stop(paste("NMD reference gear code ", NMDreferenceGearCode, "not supported."))
  }
}

#' Lookup selectivity device mesh size for gear code
#' @param NMDreferenceGearCode gear codes defined in NMD reference (NMD biotic)
#' @return may return NA, if selectivity device is not defined for gear code, or if use of selectivity device is not specified by gear code
getSelDevMeshSize <- function(NMDreferenceGearCode){
  if (NMDreferenceGearCode %in% c(3100)){
    return(NA) 
  }
  if (NMDreferenceGearCode %in% c(3400)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3500)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3600)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(3712,3710)){
    return(0) 
  }
  if (NMDreferenceGearCode %in% c(5100)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5101)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5110)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5111)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5210)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5211)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(5212)){
    return(NA)
  }
  if (NMDreferenceGearCode %in% c(4000)){
    return(NA) #upsesifisert
  }
  if (NMDreferenceGearCode %in% c(4110:4129, 4140:4149)){
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