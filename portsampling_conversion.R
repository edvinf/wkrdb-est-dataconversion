source("data_conversion.R")
library(data.table)

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
    
    catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stations[i,])
    
    assemblage <- targetAssemblageFunction(stations[i,], catchsamples)
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
                        getEcoZone(stations$latitudestart[i], stations$longitudestart[i]),
                        getDCRareaLvl3(stations$latitudestart[i], stations$longitudestart[i]),
                        getDCRareaLvl5(stations$latitudestart[i], stations$longitudestart[i]),
                        fdirarea,
                        NA,
                        getFishingDepth(stations$fishingdepthmean[i],stations$fishingdepthmin[i], stations$fishingdepthmax[i], stations$fishingdepthstart[i], stations$fishingdepthstop[i]),
                        getBottomDepth(stations$bottomdepthmean[i], stations$bottomdepthstart[i], stations$bottomdepthstop[i]),
                        NA,
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
                        codelist$RS_SelectionMethod$UPSWOR,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
    ))
    specieslistfunction(stream)
    
    exportSA(stream, catchsamples, nmdbiotic, lower_hierarchy, codelist$RS_CatchFraction$landed)
  }
  
}

#' Export OS lines and lower hiearchy lines for port sampling.
#' @description 
#'  Catchfractions are treated as landed.
#' @param stream stream to write output to
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportPortSamplingOS <- function(stream, nmdbiotic, lower_hierarchy, specieslistfunction, targetAssemblageFunction){
  qm <- data.table(month=c("01","02","03","04","05","06","07","08","09","10","11","12"), 
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
  stations <- nmdbiotic$ReadBioticXML_BioticData_fishstation.txt
  stations$month <- substr(stations$stationstopdate,6,7)
  stations <- merge(stations, qm)
  for (q in c("Q1","Q2","Q3","Q4")){
    stratastations <- stations[stations$quarter==q,]
    exportPortSamplingOSstrata(stream, nmdbiotic, stratastations, lower_hierarchy, specieslistfunction, targetAssemblageFunction, q)
  }
}

exportPortSamplingOSstrata <- function(stream, nmdbiotic, stations, lower_hierarchy, specieslistfunction, targetAssemblageFunction, strata){
  buyerid <- stations$landingsite
  buyerid[is.na(buyerid)] <- paste("samp", stations[is.na(buyerid), "stationstartdate"])

  stop("Need to iterate of port days, in stead")
  for (i in 1:nrow(stations)){
    
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
    
    catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stations[i,])
    
    assemblage <- targetAssemblageFunction(stations[i,], catchsamples)
    
    writeline(stream, c("OS",
                        buyerid[i],
                        codelist$RS_Stratfification$stratified, 
                        getLoCode(stations$landingsite[i]),
                        stations$stationstartdate[i],
                        NA,
                        strata, 
                        codelist$RS_Clustering$unclustered, 
                        "U",
                        codelist$RS_Sampler$observer,
                        NA,
                        NA,
                        NA, #n total
                        length(unique(paste(buyerid, stations$startdate[i]))), #n sampled
                        NA,
                        codelist$RS_SelectionMethod$expert,
                        NA, #locType
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
    ))
    
    stop("Iterate over each station in port-day, support gear stratification")
    exportPortSamplingLE(stream, nmdbiotic, stations[i,], catchsamples, lower_hierarchy, specieslistfunction, targetAssemblageFunction)
  }
}

#' Export LE lines and lower hiearchy lines for port sampling.
#' @description 
#'  Catchfractions are treated as landed.
#' @param stream stream to write output to
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param station
#' @param catchsammples
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportPortSamplingLE <- function(stream, nmdbiotic, station, catchsamples, lower_hierarchy, specieslistfunction, targetAssemblageFunction){
  writeline(stream, c("LE"
                      ))
  specieslistfunction(stream)
  exportSA(stream, catchsamples, nmdbiotic, lower_hierarchy, codelist$RS_CatchFraction$landed, codelist$RS_SelectionMethod$expert)
}

#' 
exportPortSamplingDE <- function(stream, samplingschemename, samplingframedesc, year){
  writeline(stream, c("DE", samplingschemename, year, samplingframedesc, codelist$YesNoFields$yes, codelist$RS_UpperHierarchy$h5))
}

#' Exports SD table for lottery sampling
#' @param samplingcounty consult RDBES documentatino
#' @param samplinginstitution consult RDBES documentatino
exportPortSamplingSD <- function(stream, samplingcountry=codelist$ISO_3166$norway, samplinginstitution=codelist$EDMO$IMR){
  writeline(stream, c("SD", samplingcountry, samplinginstitution))
}

#' Exports IMR port sampling to RDBES Hierarchy 5 with lower hiearchy C
#' @description 
#'  Output files are compatible with RDBES and can be imported via RDBES web-interface as part of data submission
#' @param outfile filename for writing out
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieselection function for export species list and species list details. Must accept a single argument 'stream' defined as for this function. Will be called at appropriate places to write SL and SS lines. E.g: exportHerringSS
#' @param year the year of the sampling
#' @param samplingschemename Name of the sampling program
#' @param samplingframedesc Description of the sampling frame
#' @param targetAssemblageFunction Function described by the target function assemblage contract in metierannotatin.R
exportPortSamplingRDBES <- function(outfile, nmdbiotic, speciesselection, year, samplingschemename, samplingframedesc, targetAssemblageFunction){
  stream <- file(outfile, open="w")
  exportPortSamplingDE(stream, samplingschemename, samplingframedesc, year)
  exportPortSamplingSD(stream)
  exportPortSamplingOS(stream, nmdbiotic, codelist$RS_LowerHierarchy$BVonly, speciesselection, targetAssemblageFunction)
  close(stream)
}