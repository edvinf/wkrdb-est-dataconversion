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
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param catchfraction the fraction of the catch that was sampled code as RS_CatchFraction
#' @param seqnr sequence number for this sample
exportSA <- function(stream, catchsamples, lower_hierarchy, catchfraction, seqnr){
  
  catches <- catchsamples[!duplicated(catchsamples$catchsampleid)]
  
  for (i in 1:nrow(catches)){
    presentation <- getPresentation(catches$sampleproducttype[i]) 
    if (presentation==RDBESexchange:::codelist$RS_Presentation$whole){
      conv_factor <- NA
      sampW <- catches$lengthsampleweight*1000
    }
    else{
      conv_factor <- get_conversion_factor(catches$catchcategory[i], catches$sampleproducttype[i])
      sampW <- catches$lengthsampleweight*1000*conv_factor
    }
    
    if (catches$catchproducttype[i] == 1){
      conv_factor_catch <- NA
      totW <- catches$catchweight[i]*1000
    }
    else if (catches$catchproducttype[i] == 4){
      conv_factor_catch <- get_conversion_factor(catches$catchcategory[i], catches$catchproducttype[i])
      totW <- catches$catchweight[i]*conv_factor_catch*1000
    }
    else if (catches$catchproducttype[i] == 3){
      conv_factor_catch <- get_conversion_factor(catches$catchcategory[i], catches$catchproducttype[i])
      totW <- catches$catchweight[i]*conv_factor_catch*1000
    }
    else{
      stop(paste("product type", catches$catchproducttype[i], "not supported"))
    }
    
    RDBESexchange::writeSA(stream,
                           SAseqNum=seqnr,
                           SAunitName=catchsamples$catchsampleid,
                           SAspeCode=catchsamples$aphia[i],
                           SApres=presentation,
                           SAspecState=RDBESexchange:::codelist$RS_SpecimensState$Dead,
                           SAcatchCat=catchfraction,
                           SAtotalWtLive=round(totW),
                           SAsampWtLive=round(sampW),
                           SAselectMeth=RDBESexchange:::codelist$RS_SelectionMethod$systematic,
                           SAlowHierarchy=lower_hierarchy,
                           SAsampler=RDBESexchange:::codelist$RS_Sampler$self,
                           SAconFacMesLive=conv_factor)
    individuals <- catchsamples[catchsamples$catchsampleid == catches$catchsampleid[i]]
    
    if (lower_hierarchy=="A"){
      stop("Not yet supported")
    }
    if (lower_hierarchy=="B"){
      stop("Not yet supported")
    }
    if (lower_hierarchy=="C"){
      exportBVunstratified(stream, individuals, agingstructure = catchsamples$agingstructure[i])
    }
    if (lower_hierarchy=="D"){
      stop("Not yet supported")
    }
  }
}

obs <- c(RDBESexchange:::codelist$RS_BiologicalMeasurementType$length, RDBESexchange:::codelist$RS_BiologicalMeasurementType$age, RDBESexchange:::codelist$RS_BiologicalMeasurementType$weight, RDBESexchange:::codelist$RS_BiologicalMeasurementType$sex)
#' Export BV lines for RDBES
#' @param individuals data frame of individuals to process (subset of those found in nmdbiotic)
#' @param fishobservations list of parameters to export, code as RS_BiologicalMeasurementType
#' @param agingstructure agingstructure for sample, can be NULL if age is not amonv fishobservations
exportBVunstratified <- function(stream, individuals, fishobservations=obs, agingstructure=NULL){
  
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
          RDBESexchange::writeBV(stream,
                                 BVfishId = fishnumber,
                                 BVunitName = fishnumber,
                                 BVtype = RDBESexchange:::codelist$RS_BiologicalMeasurementType$age,
                                 BVvalue = individuals$age[i],
                                 BVvalTyp = "Year",
                                 BVmethod = getRDBESagingMethod(agingstructure),
                                 BVnumTotal = nrow(individuals),
                                 BVnumSamp = sum(!is.na(individuals$age)),
                                 BVsampler = sampler)
        }
      } else if (p==RDBESexchange:::codelist$RS_BiologicalMeasurementType$length){
        if (!is.na(individuals[i,"length"])){
          RDBESexchange::writeBV(stream,
                                 BVfishId = fishnumber,
                                 BVunitName = fishnumber,
                                 BVtype = RDBESexchange:::codelist$RS_BiologicalMeasurementType$length,
                                 BVvalue = individuals$length[i]*1000,
                                 BVvalTyp = RDBESexchange:::codelist$RS_UnitOfValue$mm,
                                 BVnumTotal = nrow(individuals),
                                 BVnumSamp = sum(!is.na(individuals$length)),
                                 BVsampler = sampler)
        }
      } else if (p==RDBESexchange:::codelist$RS_BiologicalMeasurementType$weight){
        if (!is.na(individuals[i,"individualweight"])){
          RDBESexchange::writeBV(stream,
                                 BVfishId = fishnumber,
                                 BVunitName = fishnumber,
                                 BVtype = RDBESexchange:::codelist$RS_BiologicalMeasurementType$weight,
                                 BVvalue = individuals$individualweight[i]*1000,
                                 BVvalTyp = RDBESexchange:::codelist$RS_UnitOfValue$g,
                                 BVnumTotal = nrow(individuals),
                                 BVnumSamp = sum(!is.na(individuals$length)),
                                 BVsampler = sampler)
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
#' @param flatnmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieslistname reference to the species list name
#' @param lower_hierarchy code for which lower hiearchy to use for export of fish measurements
#' @param samplingProb data frame mapping hauls to selection probabilities. 
#' @param targetAssemblageFunction function for assigning target assemblage, described by the target function assemblage contract in metierannotatin.R
exportLotteryFO <- function(stream, flatnmdbiotic, lower_hierarchy, samplingProb, specieslistname){
  
  
  stations <- flatnmdbiotic[!duplicated(flatnmdbiotic$serialnumber),]
  stations <- merge(samplingProb, stations, all.x=T, by="serialnumber")
  
  seqnr <- 0
  for (i in 1:nrow(stations)){
    seqnr <- seqnr + 1
    
    #nonresponse
    if (is.na(stations$serialnumber[i])){
      RDBESexchange::writeFO(stream,
                             FOseqNum = seqnr,
                             FOunitName = stations$unitname[i],
                             FOnumSamp = nrow(stations),
                             FOnumTotal = stations$popTotal[i],
                             FOselProb = NA,
                             FOincProb = stations$i.prob[i],
                             FOselectMeth = RDBESexchange:::codelist$RS_SelectionMethod$UPSWOR, 
                             FOsampler = RDBESexchange:::codelist$RS_Sampler$self,
                             FOcatReg = RDBESexchange:::codelist$RS_CatchRegistration$landed, 
                             FOobsCo = RDBESexchange:::codelist$RS_ObservationCode$hauling, 
                             FOmetier5 = "MIS_MIS",
                             FOmetier6 = "MIS_MIS_0_0_0",
                             FOgear = "MIS", 
                             FOincBycMitigDev = "Unknown",
                             FOstopLat = NA,
                             FOstopLon = NA,
                             FOendDate = stations$datekey[i],
                             FOstartDate = NA,
                             FOstartLat = stations$START_LT[i], 
                             FOstartLon = stations$START_LG[i], 
                             FOdep = NA,
                             FOwaterDep = NA,
                             FOmeshSize = NA,
                             FOselDev = NA,
                             FOselDevMeshSize = NA,
                             FOtarget = NA,
                             FOsamp = RDBESexchange:::codelist$YesNoFields$no
      )
    }
    else{
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
      
      RDBESexchange::writeFO(stream,
                             FOseqNum = seqnr,
                             FOunitName = stations$unitname[i],
                             FOnumTotal = NA,  #fishing operations total, get from logbooks
                             FOnumSamp = length(unique(stations$serialnumber)), #fishing operations sampled, get from biotic
                             FOselProb = NA,
                             FOincProb = stations$i.prob[i],
                             FOselectMeth = RDBESexchange:::codelist$RS_SelectionMethod$UPSWOR, 
                             FOsampler = RDBESexchange:::codelist$RS_Sampler$self,
                             FOcatReg = RDBESexchange:::codelist$RS_CatchRegistration$landed, 
                             FOobsCo = RDBESexchange:::codelist$RS_ObservationCode$hauling, 
                             FOmetier5 = stations$metier5[i],
                             FOmetier6 = stations$metier6[i],
                             FOgear = stations$FAOgear[i], 
                             FOincBycMitigDev = "Unknown",
                             FOstopLat = stoplat,
                             FOstopLon = stoplon,
                             FOendDate = substring(stations$stationstartdate[i], 1, 10),#mandatory, provide start date for now.
                             FOstartDate = substring(stations$stationstartdate[i], 1, 10),
                             FOstartLat = stations$latitudestart[i], 
                             FOstartLon = stations$longitudestart[i], 
                             FOdep = getFishingDepth(stations$fishingdepthmean[i],stations$fishingdepthmin[i], stations$fishingdepthmax[i], stations$fishingdepthstart[i], stations$fishingdepthstop[i]),
                             FOwaterDep = getBottomDepth(stations$bottomdepthmean[i], stations$bottomdepthstart[i], stations$bottomdepthstop[i]),
                             FOmeshSize = getMeshSize(stations$gear[i]),
                             FOselDev = getSelDev(stations$gear[i]),
                             FOselDevMeshSize = getSelDevMeshSize(stations$gear[i]),
                             FOtarget = stations$assemblage
      )
      
      exportLotterySS(stream, specieslistname)
      
      exportSA(stream, flatnmdbiotic[flatnmdbiotic$serialnumber == stations$serialnumber[i],], lower_hierarchy, RDBESexchange:::codelist$RS_CatchFraction$landed, seqnr = seqnr)
    }  
  }
  
  
}



exportLotterySS <- function(stream, specieslistname){
  RDBESexchange::writeSS(stream, SSseqNum=1, 
                         SSobsActTyp=RDBESexchange:::codelist$RS_ObservationActivityCode$haul,
                         SScatchFra=RDBESexchange:::codelist$RS_CatchRegistration$landed,
                         SSobsTyp=RDBESexchange:::codelist$RS_ObservationType$volume,
                         SSsampler=RDBESexchange:::codelist$RS_Sampler$self,
                         SSspecListName=specieslistname)
}


#
#
#

#' Exports species list with single target species sampled from landed catch
exportMonoTargetListLanded <- function(filename, listname, year, speciesCode, commercialTaxon=speciesCode, samplingcountry=RDBESexchange:::codelist$ISO_3166$norway, samplinginstitution=RDBESexchange:::codelist$EDMO$IMR){
  stream <- file(filename, open="w")
  RDBESexchange::writeSL(stream,
                         SLcountry = samplingcountry,
                         SLinstitute = samplinginstitution,
                         SLspeciesListName = listname,
                         SLyear = year,
                         SLcatchFraction = RDBESexchange:::codelist$RS_CatchFraction$landed,
                         SLcommercialTaxon = commercialTaxon,
                         SLspeciesCode = speciesCode)
  close(stream)
}

exportPelSam <- function(filename, data, year, schemename, framedesc, speciesListName){
  warning("exportPelSam not implemented")
}

exportLottery <- function(filename, data, samplingProb, year, schemename, framedesc, speciesListName){
  stream <- file(filename, open="w")
  exportLotteryDE(stream, schemename, framedesc, year)
  exportLotterySD(stream)
  exportLotteryFO(stream, data, RDBESexchange:::codelist$RS_LowerHierarchy$BVonly, samplingProb, speciesListName)
  close(stream)
}
