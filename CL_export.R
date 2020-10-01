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
  int <- c("XRR", "XNW", "XNS", "XBS", "XCA")
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
                               CLmetier6="MIS_MIS_0_0_0",
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


compileCE <- function(logb, temporalResolution="Month"){
  
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
  CE <- data.table::data.table(CErecordType=rep("CE", nrow(logb)),
                              CEdataTypeForScientificEffort=rep("Official", nrow(logb)),
                              CEdataSourceForScientificEffort=rep("Logbook", nrow(logb)),
                              CEnationalProgramBehScientificEffort=rep("", nrow(logb)),
                              CEvesselFlagCountry=rep("NO", nrow(logb)),
                              CEyear=logb$FANGSTÅR,
                              CEquarter=substr(quarters(logb$STARTTIDSPUNKT, F),2,2),
                              CEmonth=as.integer(month),
                              CEArea=RDBESexchange::getDCRareaLvl3(logb$START_LT, logb$START_LG),
                              CEStatisticalRectangle=RDBESexchange::getDCRareaLvlt(logb$START_LT, logb$START_LG),
                              CEgsaSubarea=RDBESexchange::getgsaSubArea(logb$START_LT, logb$START_LG),
                              CEjurisdictionArea=rep("",nrow(logb)),
                              CEexclusiveEconomicZoneIndicator=ecoZone(logb$SONE),
                              CEnationalFishingActivity=rep("",nrow(logb)),
                              CEmetier6=logb$CLmetier6,
                              CEincidentalByCatchMitigationDevice="Unknown",
                              CElandingLocation="*HS-*HS",
                              CEvesselLengthCategory=vesselLengthCat(logb$STØRSTE_LENGDE),
                              CEfishingTechnique="",
                              CEdeepSeaRegulation=""
                                
        )
  stop("Implement the rest")
  warning("Assuming no CEjurisdictionArea. Assuming no CEdeepSeaRegulation.")
  browser()
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



