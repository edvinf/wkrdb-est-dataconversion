library(Rstox)

pb2018_1 <- Rstox::readXMLfiles(files = list(biotic=c("data/pb2018/biotic_cruiseNumber_1_Falkungen.xml")))
pb2018_2 <- Rstox::readXMLfiles(files = list(biotic=c("data/pb2018/biotic_cruiseNumber_11-2018-2_Falkungen.xml")))
pb2018_3 <- Rstox::readXMLfiles(files = list(biotic=c("data/pb2018/biotic_cruiseNumber_11-2018-3_Falkungen.xml")))
pb2018_4 <- Rstox::readXMLfiles(files = list(biotic=c("data/pb2018/biotic_cruiseNumber_11-2018-4_Falkungen.xml")))

pb2018 <- list()
pb2018$ReadBioticXML_BioticData_mission.txt <- rbind(pb2018_1$ReadBioticXML_BioticData_mission.txt, 
                                                     pb2018_2$ReadBioticXML_BioticData_mission.txt,
                                                     pb2018_3$ReadBioticXML_BioticData_mission.txt,
                                                     pb2018_4$ReadBioticXML_BioticData_mission.txt)
pb2018$ReadBioticXML_BioticData_fishstation.txt <- rbind(pb2018_1$ReadBioticXML_BioticData_fishstation.txt, 
                                                     pb2018_2$ReadBioticXML_BioticData_fishstation.txt,
                                                     pb2018_3$ReadBioticXML_BioticData_fishstation.txt,
                                                     pb2018_4$ReadBioticXML_BioticData_fishstation.txt)
pb2018$ReadBioticXML_BioticData_catchsample.txt <- rbind(pb2018_1$ReadBioticXML_BioticData_catchsample.txt, 
                                                         pb2018_2$ReadBioticXML_BioticData_catchsample.txt,
                                                         pb2018_3$ReadBioticXML_BioticData_catchsample.txt,
                                                         pb2018_4$ReadBioticXML_BioticData_catchsample.txt)
pb2018$ReadBioticXML_BioticData_individual.txt <- rbind(pb2018_1$ReadBioticXML_BioticData_individual.txt, 
                                                         pb2018_2$ReadBioticXML_BioticData_individual.txt,
                                                         pb2018_3$ReadBioticXML_BioticData_individual.txt,
                                                         pb2018_4$ReadBioticXML_BioticData_individual.txt)


# remove one registration of intersex cod, so that we don't have to deal with code conversion
pb2018$ReadBioticXML_BioticData_individual.txt <- pb2018$ReadBioticXML_BioticData_individual.txt[is.na(pb2018$ReadBioticXML_BioticData_individual.txt$sex) | pb2018$ReadBioticXML_BioticData_individual.txt$sex!= 3,]

selffished <- pb2018$ReadBioticXML_BioticData_fishstation.txt[pb2018$ReadBioticXML_BioticData_fishstation.txt$catchplatform == pb2018$ReadBioticXML_BioticData_fishstation.txt$platform,]

#Remove self-fished jig samples
pb2018$ReadBioticXML_BioticData_fishstation.txt <- pb2018$ReadBioticXML_BioticData_fishstation.txt[!(pb2018$ReadBioticXML_BioticData_fishstation.txt$serialnumber %in% selffished$serialnumber),]
pb2018$ReadBioticXML_BioticData_catchsample.txt <- pb2018$ReadBioticXML_BioticData_catchsample.txt[!(pb2018$ReadBioticXML_BioticData_catchsample.txt$serialnumber %in% selffished$serialnumber),]
pb2018$ReadBioticXML_BioticData_individual.txt <- pb2018$ReadBioticXML_BioticData_individual.txt[!(pb2018$ReadBioticXML_BioticData_individual.txt$serialnumber %in% selffished$serialnumber),]

warning("Put in proper target assemblage")
warning("Finish species list")
warning("buyer id")
warning("Empty strata not handled. pb gear and quarter ? Introduce if strata totals are introduced")
warning("Fix vessel encoding. All are unique after conversion.")
warning("Fix encoding when no measurments are taken (lowH D)")
warning("Put in unsampeld catches as unsampled SA.")

source("portsampling_conversion.R")
exportPortSamplingRDBES("portsampling_H5.csv", pb2018, exportCommercialWhiteFishSS, 2018, "Port sampling No 64 Lat", "Norwegian coastal fleet, No 64 Lat, landing fresh / unconserved whitefish", generateTargetAssemblageSpecified("DEF"))
