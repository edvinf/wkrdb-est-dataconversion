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

#' Export SA lines for RDBES
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
exportSA <- function(stream, nmdbiotic, lower_hierarchy){
  
  if (lower_hierarchy=="A"){
    stop("Not yet supported")
  }
  if (lower_hierarchy=="B"){
    stop("Not yet supported")
  }
  if (lower_hierarchy=="C"){
    exportBV(stream, nmdbiotic)
  }
  if (lower_hierarchy=="D"){
    stop("Not yet supported")
  }
}
exportBV <- function(stream, nmdbiotic){}


#
# Lottery-sampling specific functions
#
exportLotteryDE <- function(stream, samplingschemename, samplingframedesc, year){
  writeline(stream, c("DE", samplingschemename, year, samplingframedesc, codelist$YesNoFields$yes, "13"))
}

#' Exports SD table for lottery sampling
#' @param samplingcounty consult RDBES documentatino
#' @param samplinginstitution consult RDBES documentatino
exportLotterySD <- function(stream, samplingcountry="NOR", samplinginstitution="IMR"){
  writeline(stream, c("SD", samplingcountry, samplinginstitution))
}

#' Export FO lines and lower hiearchy lines for lottery sampling.
#' @description 
#'  Fishing operations (FO) are treated as unstratified
#' @param stream stream to write output to
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param selectionProb data frame mapping hauls to selection probabilities. Contains columns: missiontype, missionnumber, platform, startyear, serialnumber, catchsampleid, selectionprobability, where selectionprobability is the probability of selecting the haul and the other columns are for identifying a haul in nmdbiotic.
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportLotteryFO <- function(stream, nmdbiotic, lower_hierarchy, selectionProb, specieslistfunction, targetAssemblageFunction){
  
  stations <- merge(nmdbiotic$ReadBioticXML_BioticData_fishstation.txt, selectionProb)
  
  for (i in 1:nrow(stations)){
    
    fdirarea <- NA
    #Get area and location code defined by Norwegian Directorate of fisheries
    if (!is.na(stations$system[i]) & stations$system[i]=="2"){
        fdirarea <- formatFdirArea(stations$area[i], stations$location[i])      
    }
    if (is.na(fdirarea)){
      area <- getFDIRarea(stations$latitudestart[i], stations$longitudestart[i])
      location <- getFDIRlocation(stations$latitudestart[i], stations$longitudestart[i])
      fdirarea <- formatFdirArea(area, location)      
    }
    
    writeline(stream, c("FO",
                codelist$YesNoFields$no, 
                stations$station[i], 
                "U", 
                codelist$YesNoFields$no, 
                "U",
                codelist$RS_Sampler$self,
                codelist$RS_AggregationLevel$haul,
                codelist$RS_FishingValidity$valid,
                codelist$RS_CatchRegistration$landed,
                stations$stationstartdate[i],
                stations$stationstarttime[i],
                stations$stationstopdate[i],
                stations$stationstoptime[i],
                NA,
                stations$latitudestart[i],
                stations$longitudestart[i],
                stations$latitudeend[i],
                stations$longitudeend[i],
                getEcoZone(stations$latitudestart[i], stations$longitudestart[i]),
                getDCRareaLvl3(stations$latitudestart[i], stations$longitudestart[i]),
                getDCRareaLvl5(stations$latitudestart[i], stations$longitudestart[i]),
                fdirarea,
                getFishingDepth(stations$fishingdepthmean[i],stations$fishingdepthmin[i], stations$fishingdepthmax[i], stations$fishingdepthstart[i], stations$fishingdepthstop[i]),
                getBottomDepth(stations$bottomdepthmean[i], stations$bottomdepthstart[i], stations$bottomdepthstop[i]),
                NA,
                getMetierLvl5(),
                getMetierLvl6(),
                getGear(),
                getMeshSize(),
                getSelDev(),
                getSelDevMeshSize(),
                targetAssemblageFunction(nmdbiotic),
                codelist$RS_ObservationCode$None,
                NA,
                NA,
                stations$selectionprobability[i],
                codelist$RS_SelectionMethod$SRSWR,
                NA,
                NA,
                NA,
                NA,
                NA
    ))
    specieslistfunction(stream)
    exportSA(stream, nmdbiotic, lower_hierarchy)
  }
  
}


#' Exports IMR lottery sampling to RDBES Hierarchy 13 with lower hiearchy C
#' @description 
#'  Output files are compatible with RDBES and can be imported via RDBES web-interface as part of data submission
#' @param outfile filename for writing out
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param selectionProb data frame mapping hauls to selection probabilities. Contains columns: missiontype, missionnumber, platform, startyear, serialnumber, catchsampleid, selectionprobability, where selectionprobability is the probability of selecting the haul and the other columns are for identifying a haul in nmdbiotic.
#' @param specieselection function for export species list and species list details. Must accept a single argument 'stream' defined as for this function. Will be called at appropriate places to write SL and SS lines. E.g: exportHerringSS
#' @param year the year of the sampling
#' @param samplingschemename Name of the sampling program
#' @param samplingframedesc Description of the sampling frame
#' @param targetAssemblageFunction Function described by the target function assemblage contract in metierannotatin.R
exportLotteryRDBES <- function(outfile, nmdbiotic, selectionProb, speciesselection, year, samplingschemename, samplingframedesc, targetAssemblageFunction){
  stream <- file(outfile, open="w")
  exportLotteryDE(stream, samplingschemename, samplingframedesc, year)
  exportLotterySD(stream)
  exportLotteryFO(stream, nmdbiotic, selectionProb, "C", speciesselection, targetAssemblageFunction)
  close(stream)
}


#
# species specific functions
#
exportHerringSL <- function(stream){}
exportHerringSS <- function(stream){
  exportHerringSL(stream)  
}
