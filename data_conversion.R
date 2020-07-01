#
# Generic export functions
#
library(RDBESexchange)
source("metierannotation.R")

convert_nation <- function(NMDnationCode){
  if (NMDnationCode == 58){
    return("NOR")
  }
}

get_conversion_factor <- function(species, IMRproducttype){
  #COD
  if (species == 164712){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.18)
    }
  }
  #HAD
  if (species == 164744){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.14)
    }
  }
  #PLE
  if (species == 172902){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.1)
    }
  }
  #MON
  if (species == 164501){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.20)
    }
    if (IMRproducttype == 3){
      return(2.8)
    }
  }
  #LEM
  if (species == 172888){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.10)
    }
    if (IMRproducttype == 3){
      return(1.20)
    }
  }
  #HAL
  if (species == 172933){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.10)
    }
    if (IMRproducttype == 3){
      return(1.35)
    }
  }
  #Tusk
  if (species == 164740){
    #guttet with head
    if (IMRproducttype == 4){
      return(1.20)
    }
    if (IMRproducttype == 3){
      return(1.40)
    }
  }
  
  stop(paste("Product type", IMRproducttype, "not supported for species", species))
}

# convert HP to kW
convertPower <- function(powerLss){
  if (is.na(powerLss)){
    return(NA)
  }
  else{
    return(powerLss/1.36)
  }
}

getLengthCategory <- function(length){
  if (is.na(length)){
    return("UNKNOWN")
  }
  
  if (length < 8){
    return("<8")
  }
  if (length < 10){
    return("8-<10")
  }
  if (length > 12){
    return("10-<12")
  }
  if (length < 15){
    return("12-<15")
  }
  if (length < 18){
    return("15-<18")
  }
  if (length < 24){
    return("18-<24")
  }
  if (length < 40){
    return("24-<40")
  }
  if (length >= 40){
    return("40<")
  }
  
}

#' Make anonymized code for vessel
#' Version for test-data only.
encodeVessel <- function(platformCode){
  return(as.integer(platformCode)-1000 + 42)
}

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
    return(RDBESexchange:::codelist$RS_Presentation$whole)
  }
  if (NMDReferenceProducttype=="4"){
    return(RDBESexchange:::codelist$RS_Presentation$guttedwHead)
  }
  if (NMDReferenceProducttype=="3"){
    return(RDBESexchange:::codelist$RS_Presentation$guttedwoHead)
  }
  else{
    stop(paste("NMD product type not supported", NMDReferenceProducttype))
  }
}

#' Converts NMD reference (NMD biotic) aging structure codes to RDBES RS_MethodsForMeasurement codes
#' @param NMDreferenceAginstructure code used for aging structures in biotic
getRDBESagingMethod <- function(NMDreferenceAginstructure){
  if (NMDreferenceAginstructure=="2"){
    return(RDBESexchange:::codelist$RS_MethodForMeasurement$otolith)
  }
  if (NMDreferenceAginstructure=="1"){
    return(RDBESexchange:::codelist$RS_MethodForMeasurement$scale)
  }
  else{
    stop(paste("NMD age structure code not supported", NMDreferenceAginstructure))
  }
}

#' Converts sex codes from NMD reference (NMD biotic) to RDBES RS_Sex codes
getSex <-function(NMDreferenceSex){
  if (NMDreferenceSex==1){
    return(RDBESexchange:::codelist$RS_Sex$female)
  }
  if (NMDreferenceSex==2){
    return(RDBESexchange:::codelist$RS_Sex$male)
  }
  else{
    stop(paste("NMD sex code not supported", NMDreferenceSex))
  }
}

#' Export SA lines for RDBES
#' @details 
#'  Assumes no replicate sampels were taken from catch.
#' @param catchsamples data fram of catchsamples to process (subset of those found in nmdbiotic)
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions (for the entire exported data set).
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param catchfraction the fraction of the catch that was sampled code as RS_CatchFraction
#' @param seqnr sequence number for this sample
exportSA <- function(stream, catchsamples, nmdbiotic, lower_hierarchy, catchfraction, seqnr){
  
  for (i in 1:nrow(catchsamples)){
    presentation <- getPresentation(catchsamples$sampleproducttype[i]) 
    if (presentation==RDBESexchange:::codelist$RS_Presentation$whole){
      conv_factor <- NA
      sampW <- catchsamples$lengthsampleweight*1000
    }
    else{
      conv_factor <- get_conversion_factor(catchsamples$catchcategory[i], catchsamples$sampleproducttype[i])
      sampW <- catchsamples$lengthsampleweight*1000*conv_factor
    }
    
    if (catchsamples$catchproducttype[i] == 1){
      conv_factor_catch <- NA
      totW <- catchsamples$catchweight[i]*1000
    }
    else if (catchsamples$catchproducttype[i] == 4){
      conv_factor_catch <- get_conversion_factor(catchsamples$catchcategory[i], catchsamples$catchproducttype[i])
      totW <- catchsamples$catchweight[i]*conv_factor_catch*1000
    }
    else if (catchsamples$catchproducttype[i] == 3){
      conv_factor_catch <- get_conversion_factor(catchsamples$catchcategory[i], catchsamples$catchproducttype[i])
      totW <- catchsamples$catchweight[i]*conv_factor_catch*1000
    }
    else{
      stop(paste("product type", catchsamples$catchproducttype[i], "not supported"))
    }
    
    writeline(stream, c("SA", seqnr, NA, RDBESexchange:::codelist$RS_Stratfification$unstratified,"U",catchsamples$aphia[i],NA,presentation,RDBESexchange:::codelist$RS_SpecimensState$Dead,catchfraction,NA,NA,NA,"U",NA,NA,NA,"NA",NA,NA,NA,NA,NA,NA,NA,NA,RDBESexchange:::codelist$RS_UnitType$basket, format(round(totW), scientific = F), format(round(sampW), scientific = F), NA, NA, NA,NA,RDBESexchange:::codelist$RS_SelectionMethod$systematic, catchsamples$catchsampleid, lower_hierarchy, RDBESexchange:::codelist$RS_Sampler$self,RDBESexchange:::codelist$YesNoFields$yes,NA,NA,NA,conv_factor))
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

obs <- c(RDBESexchange:::codelist$RS_BiologicalMeasurementType$length, RDBESexchange:::codelist$RS_BiologicalMeasurementType$age, RDBESexchange:::codelist$RS_BiologicalMeasurementType$weight, RDBESexchange:::codelist$RS_BiologicalMeasurementType$sex)
#' Export BV lines for RDBES
#' @param individuals data frame of individuals to process (subset of those found in nmdbiotic)
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions (for the entire exported data set).
#' @param fishobservations list of parameters to export, code as RS_BiologicalMeasurementType
#' @param agingstructure agingstructure for sample, can be NULL if age is not amonv fishobservations
exportBVunstratified <- function(stream, individuals, nmdbiotic, fishobservations=obs, agingstructure=NULL){
  
  individuals <- individuals[order(individuals$specimenid),]
  
  if (nrow(individuals)<1){
    return()
  }
  
  for (i in 1:nrow(individuals)){
    
    # for generalization consider
    # check on multiple age readings
    # check on aginstructure at individual level vs agingstructure at sample level
    
    fishnumber <- individuals$specimenid[i]
    sampler <- RDBESexchange:::codelist$RS_Sampler$observer
    stratification <- RDBESexchange:::codelist$YesNoFields$no
    unitscalelist <- "A"
    stratum <- "U"
    
    for (p in fishobservations){
      if (p==RDBESexchange:::codelist$RS_BiologicalMeasurementType$age){
        if (!is.na(individuals[i,"age"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, RDBESexchange:::codelist$RS_BiologicalMeasurementType$age, individuals[i,"age"], "Year", getRDBESagingMethod(agingstructure), NA, nrow(individuals), sum(!is.na(individuals$age)), NA, NA, RDBESexchange:::codelist$RS_SelectionMethod$SRSWOR, fishnumber, sampler))
        }
      } else if (p==RDBESexchange:::codelist$RS_BiologicalMeasurementType$length){
        if (!is.na(individuals[i,"length"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, RDBESexchange:::codelist$RS_BiologicalMeasurementType$length, individuals[i,"length"]*1000, RDBESexchange:::codelist$RS_UnitOfValue$mm, NA, NA, nrow(individuals), sum(!is.na(individuals$length)), NA, NA, RDBESexchange:::codelist$RS_SelectionMethod$SRSWOR, fishnumber, sampler))  
        }
      } else if (p==RDBESexchange:::codelist$RS_BiologicalMeasurementType$weight){
        if (!is.na(individuals[i,"individualweight"])){
          writeline(stream, c("BV", fishnumber, stratification, stratum, RDBESexchange:::codelist$RS_BiologicalMeasurementType$weight, individuals[i,"individualweight"]*1000, RDBESexchange:::codelist$RS_UnitOfValue$g, NA, NA, nrow(individuals), sum(!is.na(individuals$individualweight)), NA, NA, RDBESexchange:::codelist$RS_SelectionMethod$SRSWOR, fishnumber, sampler))          
        }
      } else if (p==RDBESexchange:::codelist$RS_BiologicalMeasurementType$sex){
        if (!is.na(individuals[i,"sex"])){
          #writeline(stream, c("BV", fishnumber, stratification, stratum, RDBESexchange:::codelist$RS_BiologicalMeasurementType$sex, getSex(individuals[i,"sex"]), unitscalelist, NA, NA, nrow(individuals), sum(!is.na(individuals$sex)), NA, NA, RDBESexchange:::codelist$RS_SelectionMethod$SRSWOR, fishnumber, sampler))
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
  RDBESexhange::writeDE(stream, 
                        DEsampScheme = samplingschemename,
                        DEsampSchemeType = RDBESexchange:::codelist$RS_samplingSchemeType$nationalPilot,
                        DEyear = year,
                        DEstratumName = samplingframedesc,
                        DEhierarchyCor = RDBESexchange:::codelist$YesNoFields$yes, 
                        DEhierarchy = RDBESexchange:::codelist$RS_UpperHierarchy$h13
                        )
}

#' Exports SD table for lottery sampling
#' @param samplingcounty consult RDBES documentatino
#' @param samplinginstitution consult RDBES documentatino
exportLotterySD <- function(stream, samplingcountry=RDBESexchange:::codelist$ISO_3166$norway, samplinginstitution=RDBESexchange:::codelist$EDMO$IMR){
  RDBESexhange::writeSD(stream,
                        SDctry = RDBESexchange:::codelist$ISO_3166$norway,
                        SDinst = RDBESexchange:::codelist$EDMO$IMR
                        )
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

  seqnr <- 0
    
  for (i in 1:nrow(stations)){
    seqnr <- seqnr + 1
    
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
      stoplon <- stoplon
    }
    stoplat <-stations$latitudeend[i]
    if (!is.na(stoplat)){
      stoplat <- stoplat
    }
    
    assemblage <- targetAssemblageFunction(nmdbiotic)
    RDBESexchange::writeFO(stream,
                           FOseqNum = seqnr,
                           FOunitName = stations$serialnumber[i],
                           FOnumTotal = NA,  #fishing operations total, get from logbooks
                           FOnumSamp = nrow(stations), #fishing operations sampled, get from biotic
                           FOselProb = NA, #format(stations$selectionprobability[i]),
                           FOincProb = NA, #format(stations$inclusionprobability[i]),
                           FOselectMeth = RDBESexchange:::codelist$RS_SelectionMethod$UPSWOR, 
                           FOsampler = RDBESexchange:::codelist$RS_Sampler$self,
                           FOcatReg = RDBESexchange:::codelist$RS_CatchRegistration$landed, 
                           FOobsCo = RDBESexchange:::codelist$RS_ObservationCode$hauling, 
                           FOmetier5 = getMetierLvl5(stations$gear[i], assemblage),
                           FOmetier6 = getMetierLvl6(stations$gear[i], assemblage),
                           FOgear = getGear(stations$gear[i]), 
                           FOincBycMitigDev = "Unknow",
                           FOstopLat = stoplat,
                           FOstopLon = stoplon,
                           FOendDate = stations$stationstartdate[i],#mandatory, provide start date for now.
                           FOstartDate = stations$stationstartdate[i],
                           FOstartLat = stations$latitudestart[i], 
                           FOstartLon = stations$longitudestart[i], 
                           FOdep = getFishingDepth(stations$fishingdepthmean[i],stations$fishingdepthmin[i], stations$fishingdepthmax[i], stations$fishingdepthstart[i], stations$fishingdepthstop[i]),
                           FOwaterDep = getBottomDepth(stations$bottomdepthmean[i], stations$bottomdepthstart[i], stations$bottomdepthstop[i]),
                           FOmeshSize = getMeshSize(stations$gear[i]),
                           FOselDev = getSelDev(stations$gear[i]),
                           FOselDevMeshSize = getSelDevMeshSize(stations$gear[i]),
                           FOtarget = assemblage
                           )
    
    specieslistfunction(stream)
    
    catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stations[i,])
    
    exportSA(stream, catchsamples, nmdbiotic, lower_hierarchy, RDBESexchange:::codelist$RS_CatchFraction$landed, seqnr = seqnr)
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
  exportLotteryFO(stream, nmdbiotic, RDBESexchange:::codelist$RS_LowerHierarchy$BVonly, selectionProb, speciesselection, targetAssemblageFunction)
  close(stream)
}


#
# Port sampling export function
#

exportPbDE <- function(stream, samplingschemename, samplingframedesc, year){
  writeline(stream, c("DE", samplingschemename, RDBESexchange:::codelist$RS_samplingSchemeType$nationalMonitoring, year, samplingframedesc, RDBESexchange:::codelist$YesNoFields$no, RDBESexchange:::codelist$RS_UpperHierarchy$h6))
}

#' Exports SD table for lottery sampling
#' @param samplingcounty consult RDBES documentatino
#' @param samplinginstitution consult RDBES documentatino
exportPbSD <- function(stream, samplingcountry=RDBESexchange:::codelist$ISO_3166$norway, samplinginstitution=RDBESexchange:::codelist$EDMO$IMR){
  writeline(stream, c("SD", samplingcountry, samplinginstitution))
}

#' Export OS lines and lower hiearchy lines for port sampling sampling.
#' Prepared for testing purposes. Some fields are made up.
#' @param stream stream to write output to
#' @param nmdbiotic IMR biotic data for the entire data set. Formated by RStox parsing functions.
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportPbOS <- function(stream, nmdbiotic, lower_hierarchy, specieslistfunction, targetAssemblageFunction){
  stations <- nmdbiotic$ReadBioticXML_BioticData_fishstation.txt

  date <- stations$stationstartdate
  date[is.na(date)] <- stations$stationstopdate[is.na(date)]
  
  #assume one site pr day when site is not given
  stations$landingsite[is.na(stations$landingsite)] <- date[is.na(stations$landingsite)]
  stations$portday <- paste(date, stations$landingsite)
  
  stations$quarter <- quarters(as.POSIXct(date))
  stations$sdate <- date
  seqnr <- 0
  
  for (os in unique(stations$portday)){
    seqnr <- seqnr + 1
    osstations <- stations[stations$portday == os,]
    
    writeline(stream, c("OS",
                        seqnr,
                        osstations$landingsite[1],
                        RDBESexchange:::codelist$RS_Stratfification$stratified,
                        "BGO",
                        osstations$sdate[1],
                        NA,
                        osstations$quarter[1],
                        RDBESexchange:::codelist$RS_Clustering$unclustered,
                        "U",
                        RDBESexchange:::codelist$RS_Sampler$observer,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        RDBESexchange:::codelist$RS_SelectionMethod$systematic,
                        osstations$serialnumber[1],
                        RDBESexchange:::codelist$RS_locationtype$port,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
                        
    ))
    
    #export trips
    exportPbFT(stream, osstations, nmdbiotic, lower_hierarchy, specieslistfunction, targetAssemblageFunction)
  }
  
}

#' Export FT lines and lower hiearchy lines for port sampling sampling.
#' @description 
#'  Fishing tripss (FT) are treated as stratified by gear
#'  Catchfracrions are treated as landed.
#' @param stream stream to write output to
#' @param stations stations to export
#' @param nmdbiotic IMR biotic data for the entire data set. Formated by RStox parsing functions.
#' @param specieslistfunction function for export species list and species list details
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportPbFT <- function(stream, stations, nmdbiotic, lower_hierarchy, specieslistfunction, targetAssemblageFunction){
  
  seqnr <- 0
  
  for (i in 1:nrow(stations)){
    
    vesselCode <- encodeVessel(stations$platform[i])
    stationdate <- stations$stationstartdate[i]
    if (is.na(stationdate)){
      stationdate <- stations$stationstopdate[i]
    }
    
    seqnr <- seqnr + 1
    writeline(stream, c("FT",
                        vesselCode, 
                        seqnr,
                        RDBESexchange:::codelist$RS_Stratfification$unstratified,
                        "U",
                        RDBESexchange:::codelist$RS_Clustering$unclustered, 
                        "U",
                        RDBESexchange:::codelist$RS_Sampler$observer,
                        RDBESexchange:::codelist$RS_Samplingtype$onshore,
                        NA,
                        NA,
                        stationdate,
                        NA,
                        NA,
                        stationdate,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        RDBESexchange:::codelist$RS_SelectionMethod$systematic,
                        stations$serialnumber[i],
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
                        )
    )
    exportPbFO(stream, stations[i,], nmdbiotic, RDBESexchange:::codelist$RS_LowerHierarchy$BVonly, specieslistfunction, targetAssemblageFunction)
  }
  
}

#' Export FO lines and lower hiearchy lines for Pb sampling.
#' For test purposes, some fields may be wrong
exportPbFO <- function(stream, stations, nmdbiotic, lower_hierarchy, specieslistfunction, targetAssemblageFunction){
  
  if (nrow(stations)!=1){
    stop("")
  }
  
  seqnr <- 0
  
  for (i in 1:nrow(stations)){
    seqnr <- seqnr + 1
    
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
                        RDBESexchange:::codelist$RS_Stratfification$stratified, 
                        seqnr, 
                        getGear(stations$gear[i]), 
                        RDBESexchange:::codelist$RS_Clustering$unclustered, 
                        "U",
                        RDBESexchange:::codelist$RS_Sampler$observer,
                        RDBESexchange:::codelist$RS_AggregationLevel$haul,
                        RDBESexchange:::codelist$RS_FishingValidity$valid,
                        RDBESexchange:::codelist$RS_CatchRegistration$landed,
                        stations$sdate[i], #hack, sdate was appended on levels above.
                        stations$stationstarttime[i],
                        stations$sdate[i], #mandatory, putting in startdate for now. stations$stationstopdate[i],
                        stations$stationstoptime[i],
                        NA,
                        sprintf("%2.5f", stations$latitudestart[i]),
                        sprintf("%2.5f", stations$longitudestart[i]),
                        stoplat,
                        stoplon,
                        getEcoZone(stations$latitudestart[i], stations$longitudestart[i]),
                        getDCRareaLvl3(stations$latitudestart[i], stations$longitudestart[i]),
                        NA,#getDCRareaLvl5(stations$latitudestart[i], stations$longitudestart[i]),
                        fdirarea,
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
                        NA,
                        NA,
                        RDBESexchange:::codelist$RS_ObservationCode$None,
                        NA, #fishing operations total, get from logbooks
                        NA, #fishing operations sampled, get from biotic
                        NA,
                        NA,
                        RDBESexchange:::codelist$RS_SelectionMethod$CENSUS,
                        stations$serialnumber[i],
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA
    ))
    specieslistfunction(stream)
    
    catchsamples <- merge(nmdbiotic$ReadBioticXML_BioticData_catchsample.txt, stations[i,])
    
    catchsamples$sampleproducttype[is.na(catchsamples$sampleproducttype)] <- 1 #assume product type 1 if not provided
    
    exportSA(stream, catchsamples, nmdbiotic, lower_hierarchy, RDBESexchange:::codelist$RS_CatchFraction$landed, seqnr = seqnr)
  }
  
}


#' Exports IMR port sampling to RDBES Hierarchy 6 with lower hiearchy C.
#' @description 
#'  Output files are compatible with RDBES and can be imported via RDBES web-interface as part of data submission
#' @details 
#'  This port sampling is more appropriatly mapped ti hierarchy 5, but is exportet to 6 for testing purposes
#' @param outfile filename for writing out
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieselection function for export species list and species list details. Must accept a single argument 'stream' defined as for this function. Will be called at appropriate places to write SL and SS lines. E.g: exportPbSS
#' @param year the year of the sampling
#' @param samplingschemename Name of the sampling program
#' @param samplingframedesc Description of the sampling frame
#' @param targetAssemblageFunction Function described by the target function assemblage contract in metierannotatin.R
exportPortsamplingRDBES <- function(outfile, nmdbiotic, speciesselection, year, samplingschemename, samplingframedesc, targetAssemblageFunction){
  stream <- file(outfile, open="w")
  exportPbDE(stream, samplingschemename, samplingframedesc, year)
  exportPbSD(stream)
  exportPbOS(stream, nmdbiotic, RDBESexchange:::codelist$RS_LowerHierarchy$BVonly, speciesselection, targetAssemblageFunction)
  close(stream)
}


#
# species specific functions
#

# exports species list herring lottery
exportHerringSL2018 <- function(filename, samplingcountry=RDBESexchange:::codelist$ISO_3166$norway, samplinginstitution=RDBESexchange:::codelist$EDMO$IMR){
  stream <- file(filename, open="w")
  writeline(stream, c("SL", samplingcountry,samplinginstitution,"Herring",2018,RDBESexchange:::codelist$RS_CatchFraction$landed,126417,126417))
  close(stream)
}

exportHerringSS <- function(stream){
  writeline(stream, c("SS", 1, RDBESexchange:::codelist$RS_Stratfification$unstratified, RDBESexchange:::codelist$RS_ObservationActivityCode$haul,RDBESexchange:::codelist$RS_CatchRegistration$landed,RDBESexchange:::codelist$RS_ObservationType$volume,"U",RDBESexchange:::codelist$RS_Clustering$unclustered,"U",RDBESexchange:::codelist$RS_Sampler$self,"Herring",RDBESexchange:::codelist$YesNoFields$yes,1,1,1.0,1.0,RDBESexchange:::codelist$RS_SelectionMethod$CENSUS,"Herring",NA,NA,NA,NA,NA,RDBESexchange:::codelist$YesNoFields$yes,NA))
}

# exports species list pb
exportPbSL2018 <- function(filename, samplingcountry=RDBESexchange:::codelist$ISO_3166$norway, samplinginstitution=RDBESexchange:::codelist$EDMO$IMR){
  stream <- file(filename, open="w")
  writeline(stream, c("SL", samplingcountry,samplinginstitution,"PortSamplingCodHadPok",2018,RDBESexchange:::codelist$RS_CatchFraction$landed,126436,126436))
  writeline(stream, c("SL", samplingcountry,samplinginstitution,"PortSamplingCodHadPok",2018,RDBESexchange:::codelist$RS_CatchFraction$landed,126437,126437))
  writeline(stream, c("SL", samplingcountry,samplinginstitution,"PortSamplingCodHadPok",2018,RDBESexchange:::codelist$RS_CatchFraction$landed,126441,126441))
  close(stream)
}

exportPbSS <- function(stream){
  writeline(stream, c("SS", 1, RDBESexchange:::codelist$RS_Stratfification$unstratified, RDBESexchange:::codelist$RS_ObservationActivityCode$haul,RDBESexchange:::codelist$RS_CatchRegistration$landed,RDBESexchange:::codelist$RS_ObservationType$volume,"U",RDBESexchange:::codelist$RS_Clustering$unclustered,"U",RDBESexchange:::codelist$RS_Sampler$observer,"PortSamplingCodHadPok",RDBESexchange:::codelist$YesNoFields$yes,1,1,1.0,1.0,RDBESexchange:::codelist$RS_SelectionMethod$CENSUS,"CodHadPok",NA,NA,NA,NA,NA,NA))
}

