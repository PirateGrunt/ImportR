#' @title ImportTable
#' @export 
#' 
#' @description
#' Returns the name of the import path and file associated with the destination table
#' 
#' @param channel A valid RODBC channel
#' @param DestinationTable Name of the table to test
#' 
#' @return Nothing
#' 
ImportTable = function(channel, DestinationTable, header, ClearTableFirst)
{
  fileSource = GetSourceFilename(channel, DestinationTable)
  if (length(fileSource) == 0) {
    warning(paste0("No filesource was found for ", DestinationTable))
    return (NA)
  }
  
  importMap = GetMapName(channel, DestinationTable)
  if (length(importMap) == 0) {
    warning(paste0("No import map was found for ", DestinationTable))
    return (NA)
  }
  
  dfCSV = ReadCSV(channel, DestinationTable, fileSource, header)
  if(nrow(dfCSV) == 0){
    warning(paste0("No records returned for ", fileSource))
    return (NA)
  }
  
  RunProc (channel, DestinationTable, TRUE)
  
  SaveDF(channel, dfCSV, DestinationTable, ClearTableFirst)
  
  RunProc (channel, DestinationTable, FALSE)
}