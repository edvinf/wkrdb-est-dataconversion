source("code_lists.R")

#
# load polygon files
#

fdirfilepath <- "~/shapefiles/fdir/NMD_annotated/FDIR_Statistikk_lokasjoner"

if (file.exists(fdirfilepath)){
  fdir <- rgdal::readOGR(fdirfilepath,"FDIR_Statistikk_lokasjoner", stringsAsFactors = F)  
} else{
  fdir <- NULL
}


#' Looks up Economic zone for a position
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getEcoZone <- function(lat, lon){warning("GetEcoZone not implemented");return(NA)}

#Mandatory
#' Looks up DCR area level 3 for a position
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getDCRareaLvl3 <- function(lat, lon){warning("getDCRareaLvl3 not implemented");return(NA)}


#' Looks up DCR area level 5 for a position
#' @param lat latitude WGS84
#' @param lon longitue WGS84
getDCRareaLvl5 <- function(lat, lon){warning("getDCRareaLvl5 not implemented");return(NA)}

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