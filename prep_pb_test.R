library(Rstox)
library(readr)
library(ggplot2)

#eca

#sample data
pb2018q1 <- Rstox::readXMLfiles(files = list(biotic=c("data/pb2018/biotic_cruiseNumber_1_Falkungen.xml")))

#v<-getNMDinfo("v")
#v <- v[v$platformNumber %in% her2018$ReadBioticXML_BioticData_fishstation.txt$catchplatform,]
#v$REGM <- gsub(pattern = "-", "", v$Norwegian_Fisheries_Register)

table(pb2018q1$ReadBioticXML_BioticData_catchsample.txt$catchpartnumber)

#checks on sample data
all(!is.na(pb2018q1$ReadBioticXML_BioticData_catchsample.txt$catchweight))
#
# export data
# 

# Hierarchy 6: trip
# construct: 
source("metierannotation.R")
source("data_conversion.R")
source("vesselList.R")
exportVessels("vessellist.csv", pb2018q1$ReadBioticXML_BioticData_fishstation.txt$catchplatform)
exportPbSL2018("specieslist_portsampling.csv")
exportPortsamplingRDBES("portsampling_H6.csv", pb2018q1, exportPbSS, 2018, "TESTFILE - port sampling No 64 Lat", "Fresh fish landings", generateTargetAssemblageSpecified("DEF"))

