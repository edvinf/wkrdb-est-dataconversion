library(Rstox)
library(RstoxData)
library(readr)
library(ggplot2)

source("metierannotation.R")
source("data_conversion_RstoxData.R")

#' Adds metainformation to lottery paramter table, in so these can be reported for unsampled selections
addLotteryMetaInf <- function(lotpar, log){
  logb <- log[log$RC %in% lotpar$ITU_Call_Sign,]
  logb <- logb[,c("RC", "REGM", "STARTTIDSPUNKT", "START_LT", "START_LG")]
  logb$datekey <- substring(logb$STARTTIDSPUNKT, 1, 10)
  logb <- merge(lotpar, logb, by.x=c("ITU_Call_Sign", "datekey"), by.y=c("RC", "datekey"))
  return(logb)
}

#' Add necessary columns for RDBES export
#' @param assemblage assembalgem to be applied to all stations
addCols <- function(data, assemblage){
  data$assemblage <- assemblage
  data$metier5 <- sapply(data$gear, function(x){getMetierLvl5(x, assemblage)})
  data$metier6 <- sapply(data$gear, function(x){getMetierLvl6(x, assemblage)})
  data$FAOgear <- sapply(data$gear, getGear)
  return(data)
}

loadVessels <- function(platformcodes, dates, refresh=F){
  
  if (length(platformcodes) != length(dates)){
    stop("platform codes and dates must correspond to each other")
  }
  
  if (refresh){
    vessels<- Rstox::getNMDinfo("v")  
  }
  
  if (file.exists("vesslDmp.rds")){
    vessels <- readRDS("vesslDmp.rds")
  }
  else{
    stop("Could not find vessel list. Run with option 'refresh")
  }
  
  selection <- vessels[vessels$platformNumber %in% as.integer(platformcodes),]
  for (i in 1:length(platformcodes)){
    code <- as.integer(platformcodes[i])
    date <- dates[i]
    selection <- selection[selection$platformNumber != code | selection$validFrom <= date,]
    selection <- selection[selection$platformNumber != code | selection$validTo >= date,]
  }
  selection <- selection[order(selection$validTo, decreasing = T),]
  
  if (any(duplicated(platformcodes))){
    stop("duplcated platformcodes")
  }
  
  return(selection)
  
}

loadLotteryData <- function(file, onlyHif=T){
  lotteryp <- read_delim(file, delim="\t", col_types = "iccccccdc")
  if (onlyHif){
    lotteryp <- lotteryp[lotteryp$TM=="HIF",]
  }
  lotteryp$DA <- as.POSIXct(as.Date(lotteryp$DA, "%Y%m%d"))
  return(lotteryp)
}

extractFlatBiotic <- function(mission, samples){
  data <- merge(mission, samples$fishstation, by=names(samples$fishstation)[names(samples$fishstation) %in% names(mission)])
  data <- merge(data, samples$catchsample, by=names(samples$catchsample)[names(samples$catchsample) %in% names(data)])
  data <- merge(data, samples$individual, by=names(samples$individual)[names(samples$individual) %in% names(data)])
  nind <- nrow(data)
  data <- merge(data, samples$agedetermination, all.x=T, by=names(samples$agedetermination)[names(samples$agedetermination) %in% names(data)])
  nage <- nrow(data)
  stopifnot(nind == nage)
  return(data)
}

# returns lottery draws matched with serialnumber (NA for nonresponse)
matchLotteryParameters <- function(samp, lotteryparams, lotteryname){
  stations <- samp[!duplicated(samp$serialnumber), c("serialnumber", "catchplatform", "stationstartdate")]
  vessels <- loadVessels(stations$catchplatform, stations$stationstartdate)
  stations$catchplatform <- as.integer(stations$catchplatform)
  stations <- merge(stations, vessels[,c("platformNumber", "ITU_Call_Sign")], by.x="catchplatform", by.y="platformNumber", all.x=T)
  
  
  if (any(is.na(lotteryparams$lotteri))){
    warning("NAs for column 'lotteri'")
  }
  lp <- lotteryparams[!is.na(lotteryparams$lotteri) & lotteryparams$lotteri == lotteryname,]
  lp$popTotal <- nrow(lp)
  lp <- lp[lp$Svar == "641",]
  
  if (any(duplicated(lp[,c("RC", "DA")]))){
    stop("Date and RC does not identify hauls")
  }
  
  lp$datekey <- substr(lp$DA,1,10)
  stations$datekey <- substr(stations$stationstartdate,1,10)
  
  mat <- merge(stations, lp, by.x=c("ITU_Call_Sign", "datekey"), by.y=c("RC","datekey"), all.y=T)
  mat$unitname <- paste(mat$RC, mat$DA, mat$TI, sep="_")
  return(mat[,c("ITU_Call_Sign", "datekey", "serialnumber", "i.prob", "unitname", "popTotal")])
}

logbooks <- RstoxData::readErsFile("~/logbooks/FDIR_HI_ERS_2019_PR_2020-03-04.psv")

commercial_samples <- "~/bioticsets/v3/commSampling2019_2020_03_18.xml"
samples <- RstoxData::readXmlFile(commercial_samples)
lotteryParFile <- "lotteryParams/HImedIncProb.txt"
lotteryParameters <- loadLotteryData(lotteryParFile)
 

lotteryMissions <- samples$mission[samples$mission$missiontype == "19",]
pelSamMissions <- samples$mission[samples$mission$missiontype == "1",]

whbPelSamMissions <- pelSamMissions[pelSamMissions$missionnumber == 50,]
macPelSamMissions <- pelSamMissions[pelSamMissions$missionnumber == 30,]

lotterydata <- extractFlatBiotic(lotteryMissions, samples)


#
# WHB
#
exportMonoTargetListLanded(filename = "output/specieslistWHB_HSL.csv", listname = "whbLandedSampling", year= 2019, speciesCode = 126439)
#whb pel sam
whbPelSamData <- extractFlatBiotic(whbPelSamMissions, samples)
whbPelSamData <- addCols(whbPelSamData, assemblage="SPF")
whbPelSamData$catchweight[whbPelSamData$catchweight>2e+6] <- 2e+6 #truncate very big catches to fit RDBES integer representation
exportPelSam("output/pelSamWHB_H13.csv", whbPelSamData, 2019, schemename = "National Routine", framedesc = "WHBps", speciesListName="whbLandedSampling")

#whb lottery
whbLotteryData <- lotterydata[lotterydata$commonname == "kolmule",]
whbLotteryData <- addCols(whbLotteryData, assemblage="SPF")
whbLotteryParameters <- matchLotteryParameters(whbLotteryData, lotteryParameters, "Kolmule2019")
whbLotteryParameters <- addLotteryMetaInf(whbLotteryParameters, logbooks)
exportLottery("output/lotteryWHB_H13.csv", whbLotteryData, samplingProb = whbLotteryParameters, 2019, schemename = "National Routine", framedesc = "WHB", speciesListName="whbLandedSampling")


#
# MAC
#
#mac pel sam
exportMonoTargetListLanded(filename = "output/specieslistMAC_HSL.csv", listname = "macLandedSampling", year= 2019, speciesCode = 127023)
macPelSamData <- extractFlatBiotic(macPelSamMissions, samples)
macPelSamData <- addCols(macPelSamData, assemblage="SPF")
macPelSamData$catchweight[macPelSamData$catchweight>2e+6] <- 2e+6  #truncate very big catches to fit RDBES integer representation
exportPelSam("output/pelSamMAC_H13.csv", macPelSamData, 2019, schemename = "National Routine", framedesc = "MACps", speciesListName="macLandedSampling")
