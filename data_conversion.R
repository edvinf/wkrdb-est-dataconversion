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
  if (NMDReferenceProducttype=="1"){
    return(codelist$RS_Presentation$whole)
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

#' Export SA lines for RDBES
#' @param catchsamples data fram of catchsamples to process (subset of those found in nmdbiotic)
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions (for the entire exported data set).
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param catchfraction the fraction of the catch that was sampled code as RS_CatchFraction
exportSA <- function(stream, catchsamples, nmdbiotic, lower_hierarchy, catchfraction){
  
  for (i in 1:nrow(catchsamples)){
    presentation <- getPresentation(catchsamples$sampleproducttype[i]) 
    if (presentation==codelist$RS_Presentation$whole){
      conv_factor <- 1
    }
    else{
      stop("Conversion factors not implemented.")
      #when implementing conversion, check if it should also be applied to total weights
    }
    
    #writeline(stream, c("SA", NA,codelist$RS_Stratfification$unstratified,"U",catchsamples$aphia[i],NA,presentation,catchfraction,NA,NA,NA,"U",codelist$RS_UnitType$kg, format(round(catchsamples$catchweight[i]*1000), scientific = F), format(round(conv_factor*catchsamples$lengthsampleweight*1000), scientific = F), NA, NA, NA,codelist$RS_SelectionMethod$systematic, lower_hierarchy, codelist$RS_Sampler$self,NA,NA,NA,NA,format(conv_factor),NA))
    # got error message on number of SA fields. Removing the last a bit arbitrarily
    writeline(stream, c("SA", NA,codelist$RS_Stratfification$unstratified,"U",catchsamples$aphia[i],NA,presentation,catchfraction,NA,NA,NA,"U",codelist$RS_UnitType$kg, format(round(catchsamples$catchweight[i]*1000), scientific = F), format(round(conv_factor*catchsamples$lengthsampleweight*1000), scientific = F), NA, NA, NA,codelist$RS_SelectionMethod$systematic, lower_hierarchy, codelist$RS_Sampler$self,NA,NA,NA,NA,format(conv_factor)))
    individuals <- merge(nmdbiotic$ReadBioticXML_BioticData_individual.txt, catchsamples[i,])
    
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
    stratification <- 909
    unitscalelist <- "A"
    stratum <- "U"
    
    for (p in fishobservations){
      if (p==codelist$RS_BiologicalMeasurementType$age){
        if (!is.na(individuals[i,"age"])){
          #what should unit for age be ?
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$age, individuals[i,"age"], "mm", unitscalelist,getRDBESagingMethod(agingstructure), NA, nrow(individuals), sum(!is.na(individuals$age)), NA, codelist$RS_SelectionMethod$SRSWR, sampler))
        }
      } else if (p==codelist$RS_BiologicalMeasurementType$length){
        if (!is.na(individuals[i,"length"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$length, individuals[i,"length"]*1000, codelist$RS_UnitOfValue$mm, unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$length)), NA, codelist$RS_SelectionMethod$SRSWR, sampler))  
        }
      } else if (p==codelist$RS_BiologicalMeasurementType$weight){
        if (!is.na(individuals[i,"individualweight"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$weight, individuals[i,"individualweight"]*1000, codelist$RS_UnitOfValue$g, unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$weight)), NA, codelist$RS_SelectionMethod$SRSWR, sampler))          
        }
      } else if (p==codelist$RS_BiologicalMeasurementType$sex){
        if (!is.na(individuals[i,"sex"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, codelist$RS_BiologicalMeasurementType$sex, getSex(individuals[i,"sex"]), "mm", unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$sex)), NA, codelist$RS_SelectionMethod$SRSWR, sampler))
        }
      }
      else{
        stop(paste("Parameter", p, "not supported"))
      }
    }
  }
}




#
# Lottery-sampling specific functions
#
exportLotteryDE <- function(stream, samplingschemename, samplingframedesc, year){
  writeline(stream, c("DE", samplingschemename, year, samplingframedesc, codelist$YesNoFields$yes, codelist$RS_UpperHierarchy$h13))
}

#' Exports SD table for lottery sampling
#' @param samplingcounty consult RDBES documentatino
#' @param samplinginstitution consult RDBES documentatino
exportLotterySD <- function(stream, samplingcountry=codelist$ISO_3166$norway, samplinginstitution=codelist$EDMO$IMR){
  writeline(stream, c("SD", samplingcountry, samplinginstitution))
}

#' Export FO lines and lower hiearchy lines for lottery sampling.
#' @description 
#'  Fishing operations (FO) are treated as unstratified
#'  Catchfracrions are treated as landed.
#' @param stream stream to write output to
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param selectionProb data frame mapping hauls to selection probabilities. Contains columns: missiontype, missionnumber, platform, startyear, serialnumber, selectionprobability, where selectionprobability is the probability of selecting the haul and the other columns are for identifying a haul in nmdbiotic.
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportLotteryFO <- function(stream, nmdbiotic, lower_hierarchy, selectionProb, specieslistfunction, targetAssemblageFunction){
  
  stations <- merge(nmdbiotic$ReadBioticXML_BioticData_fishstation.txt, selectionProb)
  
  for (i in 1:nrow(stations)){
    
    fdirarea <- NA
    #Didn1t validate leave out for now
    #Get area and location code defined by Norwegian Directorate of fisheries
    #if (!is.na(stations$system[i]) & stations$system[i]=="2"){
    #  fdirarea <- formatFdirArea(stations$area[i], stations$location[i])      
    #}
    #if (is.na(fdirarea)){
    #  area <- getFDIRarea(stations$latitudestart[i], stations$longitudestart[i])
    #  location <- getFDIRlocation(stations$latitudestart[i], stations$longitudestart[i])
    #  fdirarea <- formatFdirArea(area, location)      
    #}
    
    stoplon <- stations$longitudeend[i]
    if (!is.na(stoplon)){
      stoplon <- sprintf("%2.5f", stoplon)  
    }
    stoplat <-stations$latitudeend[i]
    if (!is.na(stoplat)){
      stoplat <- sprintf("%2.5f", stoplat)
    }
    
    assemblage <- targetAssemblageFunction(nmdbiotic)
    writeline(stream, c("FO",
                        codelist$RS_Stratfification$unstratified, 
                        stations$station[i], 
                        "U", 
                        codelist$RS_Clustering$unclustered, 
                        "U",
                        codelist$RS_Sampler$self,
                        codelist$RS_AggregationLevel$haul,
                        codelist$RS_FishingValidity$valid,
                        codelist$RS_CatchRegistration$landed,
                        stations$stationstartdate[i],
                        stations$stationstarttime[i],
                        stations$stationstartdate[i], #mandatory, putting in startdate for now. stations$stationstopdate[i],
                        stations$stationstoptime[i],
                        NA,
                        sprintf("%2.5f", stations$latitudestart[i]),
                        sprintf("%2.5f", stations$longitudestart[i]),
                        stoplat,
                        stoplon,
                        #getEcoZone(stations$latitudestart[i], stations$longitudestart[i]),
                        NA,
                        getDCRareaLvl3(stations$latitudestart[i], stations$longitudestart[i]),
                        NA, # didnt valudate, take out for now: getDCRareaLvl5(stations$latitudestart[i], stations$longitudestart[i]),
                        fdirarea,
                        getFishingDepth(stations$fishingdepthmean[i],stations$fishingdepthmin[i], stations$fishingdepthmax[i], stations$fishingdepthstart[i], stations$fishingdepthstop[i]),
                        getBottomDepth(stations$bottomdepthmean[i], stations$bottomdepthstart[i], stations$bottomdepthstop[i]),
                        NA,
                        NA, #functional unit removed or not ?
                        getMetierLvl5(stations$gear[i], assemblage),
                        getMetierLvl6(stations$gear[i], assemblage),
                        getGear(stations$gear[i]),
                        getMeshSize(stations$gear[i]),
                        getSelDev(stations$gear[i]),
                        getSelDevMeshSize(stations$gear[i]),
                        assemblage,
                        codelist$RS_ObservationCode$hauling,
                        NA,
                        NA,
                        format(stations$selectionprobability[i]),
                        codelist$RS_SelectionMethod$SRSWR,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
    ))
    specieslistfunction(stream)
    
    catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stations[i,])
    
    exportSA(stream, catchsamples, nmdbiotic, lower_hierarchy, codelist$RS_CatchFraction$landed)
  }
  
}


#' Exports IMR lottery sampling to RDBES Hierarchy 13 with lower hiearchy C
#' @description 
#'  Output files are compatible with RDBES and can be imported via RDBES web-interface as part of data submission
#' @param outfile filename for writing out
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param selectionProb data frame mapping hauls to selection probabilities. Contains columns: missiontype, missionnumber, platform, startyear, serialnumber, selectionprobability, where selectionprobability is the probability of selecting the haul and the other columns are for identifying a haul in nmdbiotic.
#' @param specieselection function for export species list and species list details. Must accept a single argument 'stream' defined as for this function. Will be called at appropriate places to write SL and SS lines. E.g: exportHerringSS
#' @param year the year of the sampling
#' @param samplingschemename Name of the sampling program
#' @param samplingframedesc Description of the sampling frame
#' @param targetAssemblageFunction Function described by the target function assemblage contract in metierannotatin.R
exportLotteryRDBES <- function(outfile, nmdbiotic, selectionProb, speciesselection, year, samplingschemename, samplingframedesc, targetAssemblageFunction){
  stream <- file(outfile, open="w")
  exportLotteryDE(stream, samplingschemename, samplingframedesc, year)
  exportLotterySD(stream)
  exportLotteryFO(stream, nmdbiotic, codelist$RS_LowerHierarchy$BVonly, selectionProb, speciesselection, targetAssemblageFunction)
  close(stream)
}


#
# species specific functions
#
exportHerringSL2018 <- function(stream){
  writeline(stream, c("SL","Herring",2018,126417,codelist$SpecASFIS$herring,codelist$RS_CatchRegistration$landed))
}
exportHerringSS <- function(stream){
  writeline(stream, c("SS", codelist$RS_Stratfification$unstratified, codelist$RS_ObservationActivityCode$haul,codelist$RS_CatchRegistration$landed,codelist$RS_ObservationType$volume,"U",codelist$RS_Clustering$unclustered,"U",codelist$RS_Sampler$self,"Herring",1,1,codelist$RS_SelectionMethod$CENSUS,NA,NA,NA,NA,NA))
  exportHerringSL2018(stream)
}
