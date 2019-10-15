source("data_conversion.R")

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
    
    catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stations[i,])
    
    exportSA(stream, catchsamples, nmdbiotic, lower_hierarchy, codelist$RS_CatchFraction$landed)
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