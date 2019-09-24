
getMetierLvl5 <- function(){warning("metierannotation not implemented");return(NA)}
getMetierLvl6 <- function(){warning("metierannotation not implemented");return(NA)}
getGear <- function(){warning("metierannotation not implemented");return(NA)}
getMeshSize <- function(){warning("metierannotation not implemented");return(NA)}
getSelDev <- function(){warning("metierannotation not implemented");return(NA)}
getSelDevMeshSize <- function(){warning("metierannotation not implemented");return(NA)}

#
# Target assemblage contract
# Functions for assigning target assemblage must accept one parameter 'nmdbiotic' which is IMR biotic format as returned by Rstox parsing routines. They must return a single value being a valid target assemblage
#

#' Generates a target assembalge functions that return a specified assemblage, no mather what its arguments are
generateTargetAssemblageSpecified <- function(assemblage){return(function(nmdbiotic){return(assemblage)})}