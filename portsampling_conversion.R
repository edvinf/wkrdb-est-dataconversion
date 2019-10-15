source("data_conversion.R")

#' Exports IMR port sampling to RDBES Hierarchy 5 with lower hiearchy C
#' @description 
#'  Output files are compatible with RDBES and can be imported via RDBES web-interface as part of data submission
#' @param outfile filename for writing out
#' @param nmdbiotic IMR biotic data format as formated by RStox parsing functions.
#' @param specieselection function for export species list and species list details. Must accept a single argument 'stream' defined as for this function. Will be called at appropriate places to write SL and SS lines. E.g: exportHerringSS
#' @param year the year of the sampling
#' @param samplingschemename Name of the sampling program
#' @param samplingframedesc Description of the sampling frame
#' @param targetAssemblageFunction Function described by the target function assemblage contract in metierannotatin.R
exportPortSamplingRDBES <- function(outfile, nmdbiotic, speciesselection, year, samplingschemename, samplingframedesc, targetAssemblageFunction){
  stream <- file(outfile, open="w")
  exportPortSamplingDE(stream, samplingschemename, samplingframedesc, year)
  exportPortSamplingSD(stream)
  exportPortSamplingOS(stream, nmdbiotic, codelist$RS_LowerHierarchy$BVonly, speciesselection, targetAssemblageFunction)
  close(stream)
}