#
# Generic export functions
#

source("code_lists.R")
source("geographic_lookup.R")
source("metierannotation.R")

#' Writes nl terminated, comma-separated line to stream
#' @param stream stream to write to
#' @param fields vector of atomic elements to be written as comma-separated entries
writeline <- function(stream, fields){
  fields[is.na(fields)]<-""
  write(fields, sep=",", file=stream, ncolumns = length(fields))
}

warning("getConversionFactor not implemented")
#' Returns conversion factor to whole weight
#' @param presentation code for presentation according to RS_Presentation
#' @param species aphia code for species
#' @return conversion factor that can be used to estimate whole weight by multiplying with weight for the given presentation
getConversionFactor <- function(presentation, species){
  return(NA)
}

warning("getLoCode not implemented")
#' Returns LOCODE for landing site
#' @param landingssite landing site code defined in NMD reference (used in NMD biotic)
#' @return LOCODE
getLoCode <- function(landingsite){
  return(NA)
}

#' formats an area code based on area and location parameters defined by the Norwegian Directorat of Fisheries
#' @description 
#' formats area as <area code>-<location code>
#' @return fomratted code, may be NA
formatFdirArea <- function(area, location){
  locstring <- area
  if (is.na(location)){
    return(area)
  }
  else if (is.na(area)){
    return(area)
  }
  else{
    return(paste(area, location, sep="-"))
  }
}

#' Get fishing depth. Use meandepth if not NA, otherwise use mean of start and stop, oterhwise use mean of max and min
#' @param meandepth mean depth of gear averaged over time, or representative depth
#' @param maxdepth deepest depth fished (gear depth)
#' @param mindepth least depth fished (gear depth)
#' @param startdepth depth at start of fishing operation
#' @param stopdepth depth at end of fishing operation
#' @return depth to use, may be NA
getFishingDepth <- function(meandepth, maxdepth, mindepth, startdepth, stopdepth){
  if (!is.na(meandepth)){
    return(meandepth)
  }
  else if(!is.na(startdepth) & !is.na(stopdepth)){
    return(mean(c(startdepth, stopdepth)))
  }
  else{
    return(mean(c(maxdepth, mindepth)))
  }
}

#' Get fishing depth. Use meandepth if not NA, otherwise use mean of start and stop
#' @param meandepth mean depth of seabed averaged over time, or representative depth
#' @param depthstart depth at start of fishing operation
#' @param depthstop depth at end of fishing operation
#' @return depth to use, may be NA
getBottomDepth <- function(meandepth, depthstart, depthstop){
  if (!is.na(meandepth)){
    return(meandepth)
  }
  else{
    return(mean(c(depthstart, depthstop)))
  }
}

#' Converts presentation codes from NMD reference (NMD biotic) to RDBES
#' @param NMDReferenceProducttype code for producttype as defined in NMD reference.
getPresentation <- function(NMDReferenceProducttype){
  if (is.na(NMDReferenceProducttype)){
    return(NA)
  }
  else if (NMDReferenceProducttype=="1"){
    return(codelist$RS_Presentation$whole)
  }
  else if (NMDReferenceProducttype=="4"){
    return(codelist$RS_Presentation$gutted)
  }
  else if (NMDReferenceProducttype=="3"){
    return(codelist$RS_Presentation$headless)
  }
  else{
    stop(paste("NMD product type not supported", NMDReferenceProducttype))
  }
}

#' Converts NMD reference (NMD biotic) aging structure codes to RDBES RS_MethodsForMeasurement codes
#' @param NMDreferenceAginstructure code used for aging structures in biotic
getRDBESagingMethod <- function(NMDreferenceAginstructure){
  if (NMDreferenceAginstructure=="2"){
    return(codelist$RS_MethodForMeasurement$otolith)
  }
  if (NMDreferenceAginstructure=="1"){
    return(codelist$RS_MethodForMeasurement$scale)
  }
  else{
    stop(paste("NMD age structure code not supported", NMDreferenceAginstructure))
  }
}

#' Converts sex codes from NMD reference (NMD biotic) to RDBES RS_Sex codes
getSex <-function(NMDreferenceSex){
  if (NMDreferenceSex==1){
    return(codelist$RS_Sex$female)
  }
  if (NMDreferenceSex==2){
    return(codelist$RS_Sex$male)
  }
  else{
    stop(paste("NMD sex code not supported", NMDreferenceSex))
  }
}

#' Export VD line for RDBES
#' @param stream
#' @param platformid code for platform in NMD reference (used by NMD biotic)
exportVesselDetails(stream, platformid){
  stop("exportVesselDetails not implemented")
}

#' Export SA lines for RDBES
#' @details 
#'  Assumes no replicate sampels were taken from catch.
#' @param catchsamples data table of catchsamples to process (subset of those found in nmdbiotic)
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions (for the entire exported data set).
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param catchfraction the fraction of the catch that was sampled code as RS_CatchFraction
#' @param selectionmethod the selectionmethod used for selecting sample
exportSA <- function(stream, catchsamples, nmdbiotic, lower_hierarchy, catchfraction, selectionmethod=codelist$RS_SelectionMethod$systematic){
  
  for (i in 1:nrow(catchsamples)){
    presentation <- getPresentation(catchsamples$sampleproducttype[i]) 
    if (is.na(presentation)){
      conv_factor <- NA
      formatted_conv_factor <- NA
    }
    else if (presentation == codelist$RS_Presentation$whole){
      conv_factor <- 1
      formatted_conv_factor <- format(conv_factor)
    }
    else{
      species <- catchsamples[i,"aphia"]
      conv_factor <- getConversionFactor(presentation, species)
      formatted_conv_factor <- format(conv_factor)
    }
    
    if (is.na(catchsamples$catchproducttype[i])){
      catchconv_factor <- NA
    }
    else if (getPresentation(catchsamples$catchproducttype[i]) == codelist$RS_Presentation$whole){
      catchconv_factor <- 1
    }
    else if (getPresentation(catchsamples$catchproducttype[i]) != codelist$RS_Presentation$whole){
      species <- catchsamples[i,"aphia"]
      catchpres <- getPresentation(catchsamples$sampleproducttype[i]) 
      catchconv_factor <- getConversionFactor(catchpres, species)
    }
    
    individuals <- merge(nmdbiotic$ReadBioticXML_BioticData_individual.txt, catchsamples[i,])
    if (nrow(individuals)==0){
      writeline(stream, c("SA", NA,codelist$RS_Stratfification$unstratified,"U",catchsamples$aphia[i],NA,presentation,catchfraction,NA,NA,NA,"U",codelist$RS_UnitType$basket, format(round(catchsamples$catchweight[i]*catchconv_factor*1000), scientific = F), NA, NA, 0, NA, selectionmethod, lower_hierarchy, codelist$RS_Sampler$self,NA,NA,NA,NA,formatted_conv_factor,NA))
    }
    else{
      writeline(stream, c("SA", NA,codelist$RS_Stratfification$unstratified,"U",catchsamples$aphia[i],NA,presentation,catchfraction,NA,NA,NA,"U",codelist$RS_UnitType$basket, format(round(catchsamples$catchweight[i]*catchconv_factor*1000), scientific = F), format(round(conv_factor*catchsamples$lengthsampleweight*1000), scientific = F), format((catchconv_factor*catchsamples$catchweight[i]) / (conv_factor*catchsamples$lengthsampleweight), scientific = F), 1, NA, selectionmethod, lower_hierarchy, codelist$RS_Sampler$self,NA,NA,NA,NA,formatted_conv_factor,NA))
      
      if (lower_hierarchy=="A"){
        stop("Not yet supported")
      }
      if (lower_hierarchy=="B"){
        stop("Not yet supported")
      }
      if (lower_hierarchy=="C"){
          exportBVunstratified(stream, individuals, nmdbiotic, agingstructure = catchsamples$agingstructure[i])        
      }
      if (lower_hierarchy=="D"){
        stop("Not yet supported")
      }  
    }    
  }
}

obs <- c(codelist$RS_BiologicalMeasurementType$length, codelist$RS_BiologicalMeasurementType$age, codelist$RS_BiologicalMeasurementType$weight, codelist$RS_BiologicalMeasurementType$sex)
#' Export BV lines for RDBES
#' @param individuals data frame of individuals to process (subset of those found in nmdbiotic)
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions (for the entire exported data set).
#' @param fishobservations list of parameters to export, code as RS_BiologicalMeasurementType
#' @param agingstructure agingstructure for sample, can be NULL if age is not amonv fishobservations
exportBVunstratified <- function(stream, individuals, nmdbiotic, fishobservations=obs, agingstructure=NULL){
  
  individuals <- individuals[order(individuals$specimenid),]
  
  for (i in 1:nrow(individuals)){
    
    # for generalization consider
    # check on multiple age readings
    # check on aginstructure at individual level vs agingstructure at sample level
    
    fishnumber <- individuals$specimenid[i]
    sampler <- codelist$RS_Sampler$observer
    stratification <- codelist$RS_Stratfification$unstratified
    unitscalelist <- "A"
    stratum <- "U"
    
    for (p in fishobservations){
      if (p==codelist$RS_BiologicalMeasurementType$age){
        if (!is.na(individuals[i,"age"])){
          #what should unit for age be ?
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$age, individuals[i,"age"], "Year", unitscalelist,getRDBESagingMethod(agingstructure), NA, nrow(individuals), sum(!is.na(individuals$age)), NA, codelist$RS_SelectionMethod$SRSWOR, sampler))
          #writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$age, individuals[i,"age"], "mm", unitscalelist,getRDBESagingMethod(agingstructure), NA, nrow(individuals), sum(!is.na(individuals$age)), NA, codelist$RS_SelectionMethod$SRSWOR, sampler))
        }
      } else if (p==codelist$RS_BiologicalMeasurementType$length){
        if (!is.na(individuals[i,"length"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$length, individuals[i,"length"]*1000, codelist$RS_UnitOfValue$mm, unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$length)), NA, codelist$RS_SelectionMethod$SRSWOR, sampler))  
        }
      } else if (p==codelist$RS_BiologicalMeasurementType$weight){
        if (!is.na(individuals[i,"individualweight"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$weight, individuals[i,"individualweight"]*1000, codelist$RS_UnitOfValue$g, unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$individualweight)), NA, codelist$RS_SelectionMethod$SRSWOR, sampler))          
        }
      } else if (p==codelist$RS_BiologicalMeasurementType$sex){
        if (!is.na(individuals[i,"sex"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$sex, getSex(individuals[i,"sex"]), NA, unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$sex)), NA, codelist$RS_SelectionMethod$SRSWOR, sampler))
          #writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$sex, getSex(individuals[i,"sex"]), "mm", unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$sex)), NA, codelist$RS_SelectionMethod$SRSWOR, sampler))
        }
      }
      else{
        stop(paste("Parameter", p, "not supported"))
      }
    }
  }
}






#
# species specific species list functions
#
exportHerringSL2018 <- function(stream){
  writeline(stream, c("SL","Herring",2018,126417,codelist$SpecASFIS$herring,codelist$RS_CatchRegistration$landed))
}
exportHerringSS <- function(stream){
  writeline(stream, c("SS", codelist$RS_Stratfification$unstratified, codelist$RS_ObservationActivityCode$haul,codelist$RS_CatchRegistration$landed,codelist$RS_ObservationType$volume,"U",codelist$RS_Clustering$unclustered,"U",codelist$RS_Sampler$self,"Herring",1,1,codelist$RS_SelectionMethod$CENSUS,NA,NA,NA,NA,NA))
  exportHerringSL2018(stream)
}

exportCommercialWhiteFishSS2018 <- function(stream){
  writeline(stream, c("SL","Cod",2018,126436,codelist$SpecASFIS$cod,codelist$RS_CatchRegistration$landed))
  writeline(stream, c("SL","Saithe",2018,126441,codelist$SpecASFIS$saithe,codelist$RS_CatchRegistration$landed))
  writeline(stream, c("SL","Haddock",2018,126437,codelist$SpecASFIS$haddock,codelist$RS_CatchRegistration$landed))
}
exportCommercialWhiteFishSS <- function(stream){
  writeline(stream, c("SS", codelist$RS_Stratfification$unstratified, NA, codelist$RS_CatchRegistration$landed,codelist$RS_ObservationType$volume,"U",codelist$RS_Clustering$unclustered,"U",codelist$RS_Sampler$self,"whitefish",1,1,codelist$RS_SelectionMethod$CENSUS,NA,NA,NA,NA,NA))
  exportCommercialWhiteFishSS2018(stream)
}
