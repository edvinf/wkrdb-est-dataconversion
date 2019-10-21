source("data_conversion.R")
source("strataCodingPortSamling.R")
library(data.table)

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
  buyerid[is.na(buyerid)] <- paste("samp", stations[is.na(buyerid), "stationstopdate"])

  stations$port <- buyerid
  stations$portday <- paste(buyerid, stations$stationstopdate, sep=":")
  
  for (p in unique(stations$portday)){
    
    portdaystations <- stations[stations$portday == p,]
    port <- portdaystations[1, "port"]
    date <- portdaystations[1, "stationstopdate"]
    
    writeline(stream, c("OS",
                        port,
                        codelist$RS_Stratfification$stratified, 
                        getLoCode(port),
                        date,
                        NA,
                        strata, 
                        codelist$RS_Clustering$unclustered, 
                        "U",
                        codelist$RS_Sampler$observer,
                        NA,
                        NA,
                        NA, #n total
                        length(unique(stations$portday)), #n sampled
                        NA,
                        codelist$RS_SelectionMethod$expert,
                        NA, #locType
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
    ))
    
    for (gearstrata in getPbGearStrata()){
      stratastations <- portdaystations[portdaystations$gear %in% getPbStratumGear(gearstrata),]
      catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stratastations)  
      exportPortSamplingLEstrata(stream, nmdbiotic, stratastations, catchsamples, lower_hierarchy, specieslistfunction, targetAssemblageFunction, gearstrata)
    }
  }
}

#' Export LE lines and lower hiearchy lines for port sampling.
#' @description 
#'  Catchfractions are treated as landed.
#' @param stream stream to write output to
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param stations all landing events sampled in a given port day and strata
#' @param catchsammples
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
#' @param stratum the stratum LE record should be exported for. If null, LE will be exported as unstratified
exportPortSamplingLEstrata <- function(stream, nmdbiotic, stations, catchsamples, lower_hierarchy, specieslistfunction, targetAssemblageFunction, stratum=NULL){
  
  if (nrow(stations) == 0){
    if (!is.null(stratum)){
      #handle empty strata when totals are passed 
    }
    return()
  }
  
  if (is.null(stratum)){
    stratum = "U"
  }
  buyerid <- stations$landingsite
  buyerid[is.na(buyerid)] <- paste("samp", stations[is.na(buyerid), "stationstopdate"])
  
  for (i in 1:nrow(stations)){
    station <- stations[i,]    
    stationCatchsamples <- merge(station, catchsamples)
    assemblage <- targetAssemblageFunction(station, stationCatchsamples)

    exportVesselDetails(stream, station$catchplatform)
    
    writeline(stream, c("LE",
                        codelist$RS_Stratfification$stratified,
                        station$station,
                        NA,
                        stratum,
                        codelist$RS_Clustering$unclustered,
                        "U",
                        codelist$RS_Sampler$observer,
                        codelist$YesNoFields$no,
                        codelist$RS_CatchRegistration$landed,
                        getLoCode(buyerid),
                        NA, #locType
                        codelist$ISO_3166$norway,
                        station$stationstopdate,
                        NA,
                        getEcoZone(station$latitudestart, station$longitudestart),
                        getDCRareaLvl3(station$latitudestart, station$longitudestart),
                        getDCRareaLvl5(station$latitudestart, station$longitudestart),
                        NA, #subrect fdir area ?
                        NA,
                        getMetierLvl5(station$gear, assemblage),
                        getMetierLvl6(station$gear, assemblage),
                        getGear(station$gear),
                        getMeshSize(station$gear),
                        getSelDev(station$gear),
                        getSelDevMeshSize(station$gear),
                        assemblage,
                        NA, #LE total
                        nrow(stations),
                        NA,
                        codelist$RS_SelectionMethod$expert,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
                        
    ))
    specieslistfunction(stream)
    exportSA(stream, stationCatchsamples, nmdbiotic, lower_hierarchy, codelist$RS_CatchFraction$landed, codelist$RS_SelectionMethod$expert)  
  }
  
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