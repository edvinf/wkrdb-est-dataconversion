library(Rstox)
library(readr)
library(ggplot2)
#estimate total, scaling up by trip

#estimate proportion, scale up by landings

#eca

#sample data
her2018 <- Rstox::readXMLfiles(files = list(biotic=c("data/herringlot_2018.xml")))

#v<-getNMDinfo("v")
#v <- v[v$platformNumber %in% her2018$ReadBioticXML_BioticData_fishstation.txt$catchplatform,]
#v$REGM <- gsub(pattern = "-", "", v$Norwegian_Fisheries_Register)

#landings
l<-locale()
l$decimal_mark <- "."
l$encoding<-"latin1"
landings <- read_delim("data/sild.psv", delim = "|", locale = l)

table(her2018$ReadBioticXML_BioticData_catchsample.txt$catchpartnumber)
her2018$ReadBioticXML_BioticData_catchsample.txt[her2018$ReadBioticXML_BioticData_catchsample.txt$catchpartnumber>1,c("serialnumber", "catchweight", "catchpartnumber")]
#one isntance of several catch parts sampled. Given without totalweight, assume replicate samples of catchpartnumber 1, excluding these
her2018$ReadBioticXML_BioticData_catchsample.txt<-her2018$ReadBioticXML_BioticData_catchsample.txt[her2018$ReadBioticXML_BioticData_catchsample.txt$catchpartnumber==1,]

table(her2018$ReadBioticXML_BioticData_fishstation.txt$gear)
# three instances of bottom trawl (3100). Landings report pelagic trawl for these.
her2018$ReadBioticXML_BioticData_fishstation.txt[her2018$ReadBioticXML_BioticData_fishstation.txt$serialnumber %in% c(39001, 39002, 39035), "gear"] <- 3500
table(her2018$ReadBioticXML_BioticData_fishstation.txt$gear)

#checks on sample data
all(!is.na(her2018$ReadBioticXML_BioticData_catchsample.txt$catchweight))
all(!is.na(her2018$ReadBioticXML_BioticData_catchsample.txt$lengthsampleweight))
all(her2018$ReadBioticXML_BioticData_catchsample.txt$lengthsampleweight<her2018$ReadBioticXML_BioticData_catchsample.txt$catchweight)
all(her2018$ReadBioticXML_BioticData_catchsample.txt$lengthsampleweight>0)

st20 <- her2018$ReadBioticXML_BioticData_catchsample.txt[her2018$ReadBioticXML_BioticData_catchsample.txt$sampletype==20,]
st20ind <- merge(st20, her2018$ReadBioticXML_BioticData_individual.txt)
#all samples type 20 are missing some ages
merge(aggregate(list(ages.na=st20ind$age), by=list(serialn=st20ind$serialnumber, cid=st20ind$catchsampleid), FUN=function(x){sum(is.na(x))}),
      aggregate(list(ages=st20ind$age), by=list(serialn=st20ind$serialnumber, cid=st20ind$catchsampleid), FUN=function(x){sum(!is.na(x))}))

st20aged <- st20ind[!is.na(st20ind$age),]
st20notaged <- st20ind[is.na(st20ind$age),]

st23 <- her2018$ReadBioticXML_BioticData_catchsample.txt[her2018$ReadBioticXML_BioticData_catchsample.txt$sampletype==23,]
st23ind <- merge(st23, her2018$ReadBioticXML_BioticData_individual.txt)
merge(aggregate(list(ages.na=st23ind$age), by=list(serialn=st23ind$serialnumber, cid=st23ind$catchsampleid), FUN=function(x){sum(is.na(x))}),
      aggregate(list(ages=st23ind$age), by=list(serialn=st23ind$serialnumber, cid=st23ind$catchsampleid), FUN=function(x){sum(!is.na(x))}))

# 0 in some length groups for both sample type 20 and 23 indicate that they are not length stratified
# number of aged samples frequently larger than 6 for both sampletypes
# assuming unstratified SRS of fish in haul for both sample types.

#
# Turns out that sampletype 20 is correct. Missing values are due to readability issues. Encode in SA ?
#

indwcatch <- merge(her2018$ReadBioticXML_BioticData_catchsample.txt, her2018$ReadBioticXML_BioticData_individual.txt)
agesprlg <- aggregate(list(agesampes=indwcatch$age), by=list(serialnumber=indwcatch$serialnumber, catchsampleid=indwcatch$catchsampleid, sampletype=indwcatch$length, sampletype=indwcatch$sampletype), FUN=function(x){sum(!is.na(x))})
agesprlg <- agesprlg[order(as.integer(agesprlg$sampletype)),]

#
# check gear composition and consider post-stratifying on gear for pilot
#

#
# export data
# 

#estimate probabilities from registered weights (actual probabilities was set on reported weight during haul and set quoate for the fishery and a total sampling capacity of 200? catches)

herringSelectionProb2018 <- aggregate(list(catch_kg=her2018$ReadBioticXML_BioticData_catchsample.txt$catchweight), by=list(platform=her2018$ReadBioticXML_BioticData_catchsample.txt$platform, startyear=her2018$ReadBioticXML_BioticData_catchsample.txt$startyear, missiontype=her2018$ReadBioticXML_BioticData_catchsample.txt$missiontype, missionnumber=her2018$ReadBioticXML_BioticData_catchsample.txt$missionnumber, serialnumber=her2018$ReadBioticXML_BioticData_catchsample.txt$serialnumber), FUN=sum)
#herringSelectionProb2018$selectionprobability<-herringSelectionProb2018$catch_kg/sum(landings$Rundvekt)
# using quota for 2018
herringSelectionProb2018$selectionprobability<-herringSelectionProb2018$catch_kg/(304500*1000)
if (nrow(herringSelectionProb2018)!=nrow(her2018$ReadBioticXML_BioticData_fishstation.txt)){
  stop("Error in merging selection probabilities")
}

#
# hack stationnumbers to make them unique
#

her2018$ReadBioticXML_BioticData_fishstation.txt$station <- 1:nrow(her2018$ReadBioticXML_BioticData_fishstation.txt)

# Hierarchy 13: haul
# construct: 
source("metierannotation.R")
source("data_conversion.R")
exportLotteryRDBES("herringlottery_H13.csv", her2018, herringSelectionProb2018, exportHerringSS, 2018, "Pilot of Lottery-sampling herring", "Norwegian fleet > 15 m", generateTargetAssemblageSpecified("SPF"))
warning("FOclarifyHaulN")
warning("FOstopdate")
warning("FOrectangle")
warning("BVstratification")
warning("BVunitScaleList")
warning("BVunitValue")