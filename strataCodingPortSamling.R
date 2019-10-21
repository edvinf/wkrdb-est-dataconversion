#maps station gearcodes to strata for port sampling
pb <- list()
pb$gearstrata <- data.table(gear=character(), strata=character())
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("3100"), strata=c("Trawl")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("3400"), strata=c("Trawl")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("3600"), strata=c("D.Seine")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("3600"), strata=c("D.Seine")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("3700"), strata=c("Seine")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4000"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4110"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4113"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4114"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4115"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4120"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4121"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4123"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4124"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4125"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4126"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4127"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4128"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4140"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4141"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4142"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4143"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4144"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("4145"), strata=c("Gillnet")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5100"), strata=c("Longline")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5101"), strata=c("Longline")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5110"), strata=c("Longline")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5111"), strata=c("Longline")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5210"), strata=c("Jig")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5211"), strata=c("Jig")))
pb$gearstrata <- rbind(pb$gearstrata, data.table(gear=c("5212"), strata=c("Jig")))

#' get all strata codes for gear in portsampling
#' @return character() vector with strata codes
getPbGearStrata <- function(){
  return(unique(pb$gearstrata$strata))
}

#' Get all gears in a gear stratum in portsampling
#' @param stratum
#' @return character() vector with gear codes defined by NMD reference (used in NMD biotic)
getPbStratumGear <- function(stratum){
  if (!(stratum %in% pb$gearstrata$strata)){
    stop(paste("Stratum", stratum, "not defined"))
  }
  else{
    return(pb$gearstrata[pb$gearstrata$strata == stratum,][["gear"]])
  }
}

#' Get stratum code for gear in portsampling
#' @param gearcode defined by NMD reference (used in NMD biotic)
#' @return character() stratum code
getPbGearStraum <- function(gearcode){
  if (!(gearcode %in% pb$gearstrata$gear)){
    stop(paste("Gear code", gearcode, "not in any strata"))
  }
  else{
    return(pb$gearstrata[pb$gearstrata$gear == gearcode,][["strata"]])
  }
}