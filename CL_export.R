#
# Notes from CL and CE exercise
# - metiermapping works satisfactory
# - LOCODES is a major headache. Note RDBES issue:
# https://github.com/ices-tools-dev/RDBES/issues/87
#
# - Need accurate days at sea for larger vessels. 
# - Detailed logbook/VMS for CE eskport (dep-messages (trip-start), arrival messages (locodes))
# - Current compilation done with very dubius days at sea calculation. Fishing days should be OK.
#
#
#
#
#

library(RstoxData)
library(RstoxFDA)
library(RDBESexchange)
library(readr)
landings <- RstoxData::readLssFile("~/landingsets/LSS/FDIR_HI_LSS_FANGST_2019_PR_2020-03-03.psv")
logbooks <- RstoxData::readErsFile("~/logbooks/FDIR_HI_ERS_2019_PR_2020-03-04.psv")

dc_species <- c("MAC", "WHB")
landingsDC <- landings[(landings$`Art FAO (kode)` %in% dc_species) & landings$`Fartøynasjonalitet (kode)`=="NOR" & landings$Rundvekt > 0,]
logbooksDC <- logbooks[logbooks$FANGSTART_FAO %in% dc_species,]

posImp <- readr::read_delim("resourcefiles/mainarea_fdir_from_2018_incl.txt", delim = "\t", col_types = "ccdd", na=c(""), trim_ws = T)
locale <- readr::locale()
locale$encoding <- "UTF-8"
locodesKommune <- readr::read_delim("resourcefiles/kommuneLoCodes.txt", delim = "\t", col_types = "cc", na=c(""), trim_ws = T)

logmetier <- RstoxFDA::readMetierTable("resourcefiles/logmetier.txt")
landmetier <- RstoxFDA::readMetierTable("resourcefiles/landmetier.txt")


assemblage <- readr::read_delim("resourcefiles/assembalge.txt", delim = "\t", col_types = "cc", na=c("","NA"), trim_ws = T)

annotate_assemblage <- function(logbooks, assemblage){
  logbooks$HOVEDART_FAO[is.na(logbooks$HOVEDART_FAO)] <- "MIS"
  
  missing <- logbooks$HOVEDART_FAO[!(logbooks$HOVEDART_FAO %in% assemblage$FAOspecies)]
  
  if (length(missing) > 0){
    stop("Not all species are mapped in assemblage. Missing: ", paste(unique(missing), collapse=", "))
  }
  
  lb <- merge(logbooks, assemblage, by.x="HOVEDART_FAO", by.y="FAOspecies", all.x=T)
  stopifnot(all(!is.na(lb$assemblage)))
  return(lb)
}

annotateLocodes <- function(landing, komMapping){
  
  missing <- unique(landing$Landingskommune[!(landing$Landingskommune %in% komMapping$kommune)])
  if (length(missing)>1){
    stop("Not all kommunes are mapped. Missing: ", paste(missing, collapse=", "))
  }
  
  landing <- merge(landing, komMapping, by.x="Landingskommune", by.y="kommune", all.x=T)
  landing$locode[is.na(landing$Landingskommune)] <- "*HS-*HS"
  stopifnot(all(!is.na(landing$locode)))
  landing$CLlandingLocation <- landing$locode
  return(landing)
}

annotateAreas <- function(landing, mapping){
  mapping$lok[is.na(mapping$lok)] <- "00"
  landing <- merge(landing, mapping, by.x=c("Hovedområde (kode)", "Lokasjon (kode)"), by.y=c("omr", "lok"), all.x=T)
  stopifnot(all(!is.na(landing$lat)))
  stopifnot(all(!is.na(landing$lon)))
  landing$CLarea <- getDCRareaLvl3(landing$lat, landing$lon)
  landing$CLstatisticalRectangle <- getDCRareaLvl5(landing$lat, landing$lon)
  landing$CLgsaSubarea <- getgsaSubArea(landing$lat, landing$lon)
  
  return(landing)
}

alpha3tpalpha2 <- function(x){
  if (x=="NOR"){
    return("NO")
  }
  if (x=="DEU"){
    return("DE")
  }
  if (x=="DNK"){
    return("DK")
  }
  if (x=="EST"){
    return("EE")
  }
  if (x=="FRA"){
    return("FR")
  }
  if (x=="FRO"){
    return("FO")
  }
  if (x=="GBR"){
    return("GB")
  }
  if (x=="GRL"){
    return("GL")
  }
  if (x=="IRL"){
    return("IE")
  }
  if (x=="ISL"){
    return("IS")
  }
  if (x=="LTU"){
    return("LT")
  }
  if (x=="LVA"){
    return("LV")
  }
  if (x=="NLD"){
    return("NL")
  }
  if (x=="POL"){
    return("PL")
  }
  if (x=="RUS"){
    return("RU")
  }
  if (x=="SWE"){
    return("SE")
  }
  
  stop("Country code ", x, "not recoginzed.")
}
alpha3tpalpha2 <- Vectorize(alpha3tpalpha2)

ecoZone <- function(zone){
  eu <- c("XEU")
  coast <- c("NOR", "XSV", "XJM", "GRL", "FRO", "ISL","RUS")
  int <- c("XRR", "XNW", "XNS", "XBS", "XCA", "XNE", "XSI", "XSK")
  if (zone %in% coast){
    return("COAST")
  }
  if (zone %in% int){
    return("RFMO")
  }
  if (zone %in% eu){
    return("EU")
  }
  
  stop("Ecozone code", zone, "not recognized")
}
ecoZone <- Vectorize(ecoZone)

faoToAphia <- function(fao){
  if (fao=="MAC"){
    return(127023)
  }
  if (fao=="WHB"){
    return(126439)
  }
  
  stop("FAO species code", fao, "not recognized.")
}
faoToAphia <- Vectorize(faoToAphia)

usage <- function(anv){
  huc <- c(1)
  ind <- c(2,3)
  if (anv %in% huc){
    return("HuC")
  }
  if (anv %in% ind){
    return("Ind")
  }
}
usage <- Vectorize(usage)

vesselLengthCat <- function(m){
  categories <- rep("", length(m))
  categories[!is.na(m)] <- as.character(cut(m[!is.na(m)], breaks=c(0,8,10,12,15,18,24,40,max(c(m,40), na.rm = T)+1), labels = c("<8", "8-<10", "10-<12", "12-<15", "15-<18","18-<24","24-<40","40<"),right = F))
  categories[is.na(m)] <- "Unknown"
  return(categories)
}

annotate_metier6 <- function(landings, logbooks, metiertableLogbooks, metiertableLandings, target="SPF"){
  stopifnot(all(!is.na(logbooks$RC)))
  rcs <- unique(landings$`Radiokallesignal (seddel)`[!is.na(landings$`Radiokallesignal (seddel)`)])
  
  logbooks$MASKEVIDDE[is.na(logbooks$MASKEVIDDE)] <- 0
  #landings with corresponding logbooks
  logbooks$target <- target
  logland <- landings[!is.na(landings$`Radiokallesignal (seddel)`) & (landings$`Radiokallesignal (seddel)` %in% rcs),]
  logbooks <- RstoxFDA::assignMetier(logbooks, metiertableLogbooks, "REDSKAP_FAO", "target", "MASKEVIDDE", metierColName = "CLmetier6")
  logbooks$gearid <- paste(logbooks$RC, logbooks$REDSKAP_NS)
  logbooks <- logbooks[!duplicated(logbooks$gearid),c("gearid", "CLmetier6")]
  logland$gearid <- paste(logland$`Radiokallesignal (seddel)`, logland$`Redskap (kode)`)
  logland <- merge(logland, logbooks, by="gearid")
  logland$gearid <- NULL
  
  #landings without corresponding logbooks
  landland <- landings[is.na(landings$`Radiokallesignal (seddel)`) | !(landings$`Radiokallesignal (seddel)` %in% rcs),]
  landland$target <- target
  landland <- RstoxFDA::assignMetier(landland, metiertableLandings, "Redskap (kode)", "target", metierColName = "CLmetier6")

  landland$target <- NULL

  return(rbind(landland, logland))
}

NOK=.091
meanValues <- data.table::data.table(faocode=c("MAC", "WHB"), meanValue=c(15.77*NOK,2.51*NOK))

compileCL <- function(landings, logbooks, meanValues, temporalResolution="Month"){
  stopifnot(all(!is.na(logbooks$RC)))
  stopifnot(c("CLarea", "CLstatisticalRectangle", "CLgsaSubarea") %in% names(landings))
  
  if (temporalResolution=="Quarter"){
    month <- rep("", nrow(landings))
  }
  else if (temporalResolution=="Month"){
    month <- substr(landings$`Siste fangstdato`,6,7)
  }
  else{
    stop("Temporal resolution", temporalResolution, "not recognized")
  }
  
  CL <- data.table::data.table(CLrecordType=rep("CL", nrow(landings)), 
                               CLdataTypeOfScientificWeight = rep("Official", nrow(landings)), 
                               CLdataSourceOfScientificWeight=rep("Saln", nrow(landings)),
                               CLsamplingScheme=rep("", nrow(landings)),
                               CLdataSourceLandingsValue=rep("Avgp", nrow(landings)),
                               CLlandingCountry=alpha3tpalpha2(landings$`Mottakernasjonalitet (kode)`),
                               CLvesselFlagCountry=alpha3tpalpha2(landings$`Fartøynasjonalitet (kode)`),
                               CLyear=substr(landings$`Siste fangstdato`,1,4),
                               CLquarter=substr(quarters(landings$`Siste fangstdato`, F),2,2),
                               CLmonth=as.integer(month),
                               CLarea=landings$CLarea,
                               CLstatisticalRectangle=landings$CLstatisticalRectangle,
                               CLgsaSubarea=landings$CLgsaSubarea,
                               CLjurisdictionArea=rep("", nrow(landings)),
                               CLexclusiveEconomicZoneIndicator=ecoZone(landings$`Sone (kode)`),
                               CLspeciesCode=faoToAphia(landings$`Art FAO (kode)`),
                               CLspeciesFaoCode=landings$`Art FAO (kode)`,
                               CLlandingCategory=usage(landings$`Anvendelse hovedgruppe (kode)`),
                               CLcatchCategory="Lan",
                               CLcommercialSizeCategoryScale="",
                               CLcommercialSizeCategory="",
                               CLnationalFishingActivity="",
                               CLmetier6=landings$CLmetier6,
                               CLincidentalByCatchMitigationDevice="Unknown",
                               CLlandingLocation=landings$CLlandingLocation,
                               CLvesselLengthCategory=vesselLengthCat(landings$`Største lengde`),
                               CLfishingTechnique=rep("", nrow(landings)),
                               CLdeepSeaRegulation=rep("", nrow(landings))
                               )
  
  aggVars <- list()
  for (n in names(CL)){
    aggVars[[n]] <- CL[[n]]
  }

  #aggregate weight  
  weightCL <- data.table::as.data.table(aggregate(list(CLofficialWeight=landings$Rundvekt), by=aggVars, FUN=function(x){max(1, round(sum(x, na.rm=T)))}))
  weightCL$CLscientificWeight=weightCL$CLofficialWeight
  weightCL$CLexplainDifference="NoDiff"
  
  
  meanPrice <- meanValues$meanValue[match(weightCL$CLspeciesFaoCode, meanValues$faocode)]
  stopifnot(all(!is.na(meanPrice)))
  weightCL$CLtotalOfficialLandingsValue <- round(weightCL$CLofficialWeight*meanPrice)
  
  #aggregate vessels
  vesselIds <- landings$`Radiokallesignal (seddel)`
  vesselIds[is.na(vesselIds)] <- landings$`Registreringsmerke (seddel)`[is.na(vesselIds)]
  
  vesselCountCL <- data.table::as.data.table(aggregate(list(CLnumberOfUniqueVessels=vesselIds), by=aggVars, FUN=function(x){length(unique(x))}))
  
  CL <- merge(weightCL, vesselCountCL, by=names(aggVars))
  
  CL$CLscientificWeightRSE <- ""
  CL$CLvalueRSE <- ""
  CL$CLscientificWeightQualitativeBias <- ""
  
  warning("Assuming not jurisdictionArea. Not filling in size categories (heterogenously defined). Not filling fishing technique (probably could). Not filling deep sea reg. Adapt CLscientificWeight and CLexplainDifference to cod and redfish")
  
  return(CL)
}

#' Not available in compiled logbooks, need to investigate different data source.
#' This function makes a guess based on date of last catch for a trip and the landing date.
#' 
#' The definition of the terms are for RBDES v.18. given in the report from the 2nd workshop of traversel variables
#' https://op.europa.eu/en/publication-detail/-/publication/8c5583fa-c360-11e6-a6db-01aa75ed71a1
#' 
#' days at sea = fishing days + days without fishing
#' 
#' fishing days is provided in census.
#' days without fishing will be assumed to be twice the number of the days without fishing at the end of the trip.
#' This is identifiable in data if landing date is assumed to be the end of the trip, which is the case for most entries.
#' It does however introduce large deviations for the few cases when the landing date does not reflect the end of the trip.
#' 
#' The dataset contains outliers in this respect that are probably due to erronous recording, or due to 
#' parts of catch not beeing landed at the end of the trip.
#' 
#' For the purposes of this "esimtation" these are excluded baes on the maxDaysAtSea parameter. 
#' For these the function will let the days without fishing be estimated as the mean of the other ones.
#' 
guessDaysAtSea <- function(fishingDays, lastCatchDate, landingDate, maxDaysAtSea=fishingDays*5){
  daysAfterLastCatch <- as.numeric(difftime(landingDate, lastCatchDate, units = "days"))
  daysWithoutFishing <- daysAfterLastCatch*2
  total <- daysWithoutFishing + fishingDays
  tooLong <- total > maxDaysAtSea
  
  if (all(tooLong)){
    stop("All landing dates were to late after catchdate.")
  }
  
  total[tooLong] <- fishingDays[tooLong] + mean(daysWithoutFishing[!tooLong])
  return(total)
}

#' annotate landing infor on logbooks
annotateLandingInfo <- function(logbooks, landings){
  stopifnot(all(!is.na(logbooks$RC)))
  rcs <- unique(logbooks$RC)
  logland <- landings[!is.na(landings$`Radiokallesignal (seddel)`) & (landings$`Radiokallesignal (seddel)` %in% rcs),]
  logland$tripid <- paste(logland$`Radiokallesignal (seddel)`, logland$`Siste fangstdato`)
  logland$trailingDays <- as.numeric(difftime(as.POSIXct(logland$Landingsdato, format="%d.%m.%Y"), logland$`Siste fangstdato`, units="days"))
  
  #choose the first landing from the trip to represent the trip
  logland <- logland[order(logland$trailingDays, decreasing = F),]
  logland <- logland[!duplicated(logland$tripid),]
  
  tripinfo <- logland[,c("tripid", "Landingskommune", "Landingsdato", "Siste fangstdato", "Radiokallesignal (seddel)")]
  
  logbooks$tripid <- NA
  tripinfo <- tripinfo[order(tripinfo$`Siste fangstdato`, decreasing = T),]
  for (v in unique(logbooks$RC)){
    print(v)
    vtrip <- tripinfo[tripinfo$`Radiokallesignal (seddel)`==v,]
    vesselSel <- logbooks$RC==v
    for (i in (1:nrow(vtrip))){
      logbooks$tripid[vesselSel & logbooks$STARTTIDSPUNKT<=vtrip$`Siste fangstdato`[i]] <- vtrip$tripid[i]
    }
  }
  
  return(merge(logbooks, tripinfo, by="tripid"))
}


annotateTrips <- function(annotatedLogbooks){
  warning("Distributes fraction days by haul for all gears. Should be effort variable.")
  annotatedLogbooks$haulid <- paste(annotatedLogbooks$RC, annotatedLogbooks$STARTTIDSPUNKT, sep="/")
  triptotals <- aggregate(list(totalHauls=annotatedLogbooks$haulid), by=list(tripid=annotatedLogbooks$tripid), FUN=function(x){length(unique(x))})
  
  annotatedLogbooks <- merge(annotatedLogbooks, triptotals, by="tripid", all.x=T)
  annotatedLogbooks$CEnumberOfFractionTrips <- 1/annotatedLogbooks$totalHauls
  
  return(annotatedLogbooks)
    
}



compileCE <- function(logb, temporalResolution="Month"){
  
  if (any(is.na(logb$MOTORKRAFT))){
    warning("Setting engine power mean for vessels with missing engine power")
    logb$MOTORKRAFT[is.na(logb$MOTORKRAFT)] <- mean(logb$MOTORKRAFT[!is.na(logb$MOTORKRAFT)])
  }
  if (any(is.na(logb$BRUTTOTONNASJE))){
    warning("Setting engine gross tonnage to mean for vessels with missing gross tonnage")
    logb$BRUTTOTONNASJE[is.na(logb$BRUTTOTONNASJE)] <- mean(logb$BRUTTOTONNASJE[!is.na(logb$BRUTTOTONNASJE)])    
  }

  
  if (temporalResolution=="Quarter"){
    month <- rep("", nrow(logb))
  }
  else if (temporalResolution=="Month"){
    month <- substr(logb$STARTTIDSPUNKT,6,7)
  }
  else{
    stop("Temporal resolution", temporalResolution, "not recognized")
  }
  stopifnot("CLmetier6" %in% names(logb))
  stopifnot("CEnumberOfFractionTrips" %in% names(logb))
  stopifnot("Landingskommune" %in% names(logb))
  CE <- data.table::data.table(CErecordType=rep("CE", nrow(logb)),
                              CEdataTypeForScientificEffort=rep("Official", nrow(logb)),
                              CEdataSourceForScientificEffort=rep("Combcd", nrow(logb)),
                              CEnationalProgramBehScientificEffort=rep("", nrow(logb)),
                              CEvesselFlagCountry=rep("NO", nrow(logb)),
                              CEyear=logb$FANGSTÅR,
                              CEquarter=substr(quarters(logb$STARTTIDSPUNKT, F),2,2),
                              CEmonth=as.integer(month),
                              CEArea=RDBESexchange::getDCRareaLvl3(logb$START_LT, logb$START_LG),
                              CEStatisticalRectangle=RDBESexchange::getDCRareaLvl5(logb$START_LT, logb$START_LG),
                              CEgsaSubarea=RDBESexchange::getgsaSubArea(logb$START_LT, logb$START_LG),
                              CEjurisdictionArea=rep("",nrow(logb)),
                              CEexclusiveEconomicZoneIndicator=ecoZone(logb$SONE),
                              CEnationalFishingActivity=rep("",nrow(logb)),
                              CEmetier6=logb$CLmetier6,
                              CEincidentalByCatchMitigationDevice="Unknown",
                              CElandingLocation=logb$CLlandingLocation,
                              CEvesselLengthCategory=vesselLengthCat(logb$STØRSTE_LENGDE),
                              CEfishingTechnique="",
                              CEdeepSeaRegulation=""
                                
        )
  
  aggVars <- list()
  for (n in names(CE)){
    aggVars[[n]] <- CE[[n]]
  }
  
  #
  # set trips
  #
  
  #get fractional trips
  CEbuild <- aggregate(list(CEnumberOfFractionTrips=logb$CEnumberOfFractionTrips), by=aggVars, FUN=sum)
  CEbuild$CEnumberOfFractionTrips <- RDBESexchange:::fdecimal(CEbuild$CEnumberOfFractionTrips, 2)
    
  #get dominant trips
  hc <- CE
  hc$tripid <- logb$tripid
  hc$haulid <- logb$haulid
  haulCount <- aggregate(list(haulsInCell=hc$haulid), by=append(aggVars, list(tripid=hc$tripid)), FUN=function(x){length(unique(x))}) 
  haulCount <- haulCount[order(haulCount$haulsInCell, decreasing = T),]
  haulCount <- haulCount[!duplicated(haulCount$tripid),] #keep only one cell pr tripid
  haulCount$CEnumberOfDominantTrips <-1
  
  aggVarsHC <- list()
  for (cn in names(CE)){
    aggVarsHC[[cn]] <- haulCount[[cn]]
  }
  
  domTripCE <- aggregate(list(CEnumberOfDominantTrips=haulCount$CEnumberOfDominantTrips), by=aggVarsHC, FUN=sum)
  CEbuild <- merge(CEbuild, domTripCE, by=names(aggVars), all.x=T)
  CEbuild$CEnumberOfDominantTrips[is.na(CEbuild$CEnumberOfDominantTrips)] <- 0

  #get days at sea
  logb$fishingDates <- paste(logb$RC, substr(logb$STARTTIDSPUNKT,1,10))
  fishingDaysPrTrip <- aggregate(list(fishingDaysOnTrip=logb$fishingDates), by=list(tripid=logb$tripid), FUN=function(x){length(unique(x))})
  trips <- logb[!duplicated(logb$tripid),]
  trips <- merge(trips, fishingDaysPrTrip)
  trips$totalDaysOnTrip <- guessDaysAtSea(trips$fishingDaysOnTrip, lastCatchDate = trips$`Siste fangstdato`, landingDate = as.POSIXct(trips$Landingsdato, format="%d.%m.%Y"))
  logb <- merge(logb, trips[,c("tripid", "totalDaysOnTrip", "fishingDaysOnTrip")], by="tripid", all.x=T)
  daysAtSeaCE <- aggregate(list(CEofficialDaysAtSea=logb$CEnumberOfFractionTrips*logb$totalDaysOnTrip), by=aggVars, FUN=sum)
  CEbuild <- merge(CEbuild, daysAtSeaCE, by=names(aggVars))
  CEbuild$CEofficialDaysAtSea <- RDBESexchange:::fdecimal(CEbuild$CEofficialDaysAtSea, 2)
  CEbuild$CEScientificDaysAtSea <- CEbuild$CEofficialDaysAtSea
  
  #get fishing days
  fishingDaysCE <- aggregate(list(CEofficialFishingDays=logb$fishingDates), by=aggVars, FUN=function(x){length(unique(x))})  
  CEbuild <- merge(CEbuild, fishingDaysCE, by=names(aggVars))
  CEbuild$CEofficialFishingDays <- RDBESexchange:::fdecimal(CEbuild$CEofficialFishingDays, 2)
  CEbuild$CEscientificFishingDays <- CEbuild$CEofficialFishingDays

  CEbuild$CEofficialNumberOfHaulsOrSets <- ""
  CEbuild$CEScientificNumberOfHaulsOrSets <- ""
  CEbuild$CEofficialVesselFishingHour <- ""
  CEbuild$CEscientificVesselFishingHour <- ""
  CEbuild$CEofficialSoakingMeterHour <- ""
  CEbuild$CEscientificSoakingMeterHour <- ""
  
  #get kw days at sea, convert from hp by /1,36
  kwDaysCE <- aggregate(list(CEofficialkWDaysAtSea=logb$MOTORKRAFT*logb$CEnumberOfFractionTrips*logb$totalDaysOnTrip/1.36), by=aggVars, FUN=sum)
  CEbuild <- merge(CEbuild, kwDaysCE, by=names(aggVars))
  CEbuild$CEofficialkWDaysAtSea <- round(CEbuild$CEofficialkWDaysAtSea)
  CEbuild$CEscientifickWDaysAtSea <- CEbuild$CEofficialkWDaysAtSea
  
  #get kw fishing days, convert from hp by /1,36
  kwDaysCE <- aggregate(list(CEofficialkWFishingDays=logb$MOTORKRAFT*logb$CEnumberOfFractionTrips*logb$fishingDaysOnTrip/1.36), by=aggVars, FUN=sum)
  CEbuild <- merge(CEbuild, kwDaysCE, by=names(aggVars))
  CEbuild$CEofficialkWFishingDays <- round(CEbuild$CEofficialkWFishingDays)
  CEbuild$CEscientifickWFishingDays <- CEbuild$CEofficialkWFishingDays
  
  CEbuild$CEOfficialkWFishingHours <- ""
  CEbuild$CEscientifickWFishingHours <- ""
  
  #get GT days at sea
  kwDaysCE <- aggregate(list(CEgTDaysAtSea=logb$BRUTTOTONNASJE*logb$CEnumberOfFractionTrips*logb$totalDaysOnTrip), by=aggVars, FUN=sum)
  CEbuild <- merge(CEbuild, kwDaysCE, by=names(aggVars))
  CEbuild$CEgTDaysAtSea <- round(CEbuild$CEgTDaysAtSea)
  
  #get GT fishing days
  kwDaysCE <- aggregate(list(CEgTFishingDays=logb$BRUTTOTONNASJE*logb$CEnumberOfFractionTrips*logb$fishingDaysOnTrip), by=aggVars, FUN=sum)
  CEbuild <- merge(CEbuild, kwDaysCE, by=names(aggVars))
  CEbuild$CEgTFishingDays <- round(CEbuild$CEgTFishingDays)
  
  CEbuild$CEgTFishingHours <- ""
  
  #get unique vessels
  uniqueVessels <- aggregate(list(CEnumberOfUniqueVessels=logb$RC), by=aggVars, FUN=function(x){length(unique(x))})
  CEbuild <- merge(CEbuild, uniqueVessels, by=names(aggVars))
  
  CEbuild$CEscientificFishingDaysRSE <- ""
  CEbuild$CEscientificFishingDaysQualitativeBias <- ""
  
  warning("Assuming no CEjurisdictionArea. Assuming no CEdeepSeaRegulation.")
  warning("Days at sea is computed very approximately")
  
  return(CEbuild)
}

#
# prep CL
#

landingsDCannot <- annotateAreas(landingsDC, posImp)
landingsDCannot <- annotateLocodes(landingsDCannot, locodesKommune)
landingsDCannot <- annotate_metier6(landingsDCannot, logbooksDC, logmetier, landmetier)
cl <- compileCL(landingsDCannot, logbooksDC, meanValues)
readr::write_csv(cl, "output/MAC_WHB_HCL.csv", col_names = F)



#
# prep CE
#

logbooksAllAnnot <- annotate_assemblage(logbooks, assemblage)
logbooksAllAnnot$MASKEVIDDE[is.na(logbooksAllAnnot$MASKEVIDDE)] <- 0
logbooksAllAnnot <- assignMetier(logbooksAllAnnot, logmetier, "REDSKAP_FAO", "assemblage", "MASKEVIDDE", metierColName = "CLmetier6")
#restrict to only the metiers that landings are provided for
logbooksLandedMetiers <- logbooksAllAnnot[logbooksAllAnnot$CLmetier6 %in% unique(cl$CLmetier6),]
logbooksLandedMetiers <- annotateLandingInfo(logbooksLandedMetiers, landings)
logbooksLandedMetiers <- annotateLocodes(logbooksLandedMetiers, locodesKommune)
logbooksLandedMetiers <- annotateTrips(logbooksLandedMetiers)
ce <- compileCE(logbooksLandedMetiers)
readr::write_csv(ce, "output/effort_MAC_WHB_metiers_HCE.csv", col_names = F)
