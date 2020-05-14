library(Rstox)
library(RstoxData)
source("data_conversion.R")

#approximate approach, for geneating test-sets
compileVessels <- function(landings, platforms, outfile="vessels.rds"){
  
  #merge by call sign
  
  vessellist <- Rstox::getNMDinfo("platform")
  vessellist <- vessellist[vessellist$platformNumber %in% as.integer(platforms),]
  vessellist <- vessellist[order(vessellist$ITU_Call_Sign),]
  vessellist <- vessellist[!duplicated(vessellist$platformNumber),]
  
  landings <- landings[!is.na(landings$`Radiokallesignal (seddel)`),]  
  landings <- landings[!duplicated(landings$`Radiokallesignal (seddel)`),]
  
  vessellist <- merge(vessellist, landings, by.x="ITU_Call_Sign", by.y="Radiokallesignal (seddel)", all.x=T)
  saveRDS(vessellist[,c("platformNumber","nationCode", "Største lengde", "Motorkraft", "Bruttotonnasje 1969", "Bruttotonnasje annen")], file=outfile)

}

exportVessels <- function(filename, platforms, vesselfile="vessels.rds"){
  vessels <- readRDS(vesselfile)
    
  if (!all(platforms %in% vessels$platformNumber)){
    missing <- unique(platforms[!(platforms %in% vessels$platformNumber)])
    stop(paste("Not all platforms in vessel file. Missing:", paste(missing, collapse=",")))
  }
  
  stream <- file(filename, open="w")
  
  vessels$tonnage <- vessels$`Bruttotonnasje annen`
  vessels$tonnageUnit <- as.character(NA)
  vessels$tonnageUnit[!is.na(vessels$tonnage)] <- "GRT"
  vessels$tonnage[is.na(vessels$`Bruttotonnasje annen`)] <- vessels$`Bruttotonnasje 1969`[is.na(vessels$`Bruttotonnasje annen`)]
  vessels$tonnageUnit[!is.na(vessels$`Bruttotonnasje 1969`)] <- "GRT"
  
  for (i in 1:nrow(vessels)){
    writeline(stream, c("VD",encodeVessel(vessels$platformNumber[i]), NA, convert_nation(vessels$nationCode[i]), vessels$`Største lengde`[i], getLengthCategory(vessels$`Største lengde`[i]), format(convertPower(vessels$Motorkraft[i]), digits = 1), vessels$tonnage[i], vessels$tonnageUnit[i]))
  }
  
  close(stream)
}