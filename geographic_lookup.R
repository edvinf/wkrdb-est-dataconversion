source("code_lists.R")

#
# load polygon files
#

fdirfilepath <- "~/shapefiles/fdir/NMD_annotated/FDIR_Statistikk_lokasjoner"
icesareafilepath <- "~/shapefiles/ICES_StatRec_mapto_ICES_Areas"

if (file.exists(fdirfilepath)){
  fdir <- rgdal::readOGR(fdirfilepath,"FDIR_Statistikk_lokasjoner", stringsAsFactors = F)  
} else{
  fdir <- NULL
}

if (file.exists(icesareafilepath)){
  icesArea <- rgdal::readOGR(icesareafilepath,"StatRec_map_Areas_Full_20170124", stringsAsFactors = F)  
} else{
  icesArea <- NULL
}

#
# Annotate based on position
#

#' Looks up Economic zone for a position
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getEcoZone <- function(lat, lon){warning("GetEcoZone not implemented");return(NA)}

#Mandatory
#' Looks up DCR area level 3 for a position
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getDCRareaLvl3 <- function(lat, lon){
  if (is.null(icesArea)){
    warning("ICES polygon file not available")
    return(NA)  
  } else{
    point <- SpatialPoints(data.frame(longitude=lon, latitude=lat), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
    point <- spTransform(point, CRS(proj4string(icesArea)))
    area <- paste("27", sp::over(point, icesArea)$Area_27[1], sep=".")
    return(area)
  }
  
}


#' Looks up DCR area level 5 for a position
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getDCRareaLvl5 <- function(lat, lon){
  if (is.null(icesArea)){
    warning("ICES polygon file not available")
    return(NA)  
  } else{
    point <- SpatialPoints(data.frame(longitude=lon, latitude=lat), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
    point <- spTransform(point, CRS(proj4string(icesArea)))
    rect <- sp::over(point, icesArea)$ICESNAME
    return(rect)
  }
}

#' Looks up area code defined by The Norwegian Directorate of Fisheries
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getFDIRarea <- function(lat, lon){
  if (is.null(fdir)){
    warning("FDIR polygon file not available")
    return(NA)  
  } else{
    point <- SpatialPoints(data.frame(longitude=lon, latitude=lat), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
    point <- spTransform(point, CRS(proj4string(fdir)))
    lok <- sp::over(point, fdir)$HAVOMR[1]
    return(lok)
  }
}

#' Looks up location code defined by The Norwegian Directorate of Fisheries
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getFDIRlocation <- function(lat, lon){
  if (is.null(fdir)){
    warning("FDIR polygon file not available")
    return(NA)  
  } else{
    point <- SpatialPoints(data.frame(longitude=lon, latitude=lat), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
    point <- spTransform(point, CRS(proj4string(fdir)))
    lok <- sp::over(point, fdir)$Lokasjon[1]
    return(lok)
  }
  
  }