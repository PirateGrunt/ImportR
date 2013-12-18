#' @title GetImportSet
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' This will return a data frame with meta information about import files and target tables. The fields returned 
#' are as follows:
#' 
#' SourceFilepath - Folder containing the data to import
#' SourceFilename - The name of the file containing data to import
#' DestinationTable - The table into which the data will be imported
#' PreImportProc - SQL routine to run before importing. This is typically used to delete computed columns
#' PostImportProc - SQL routine to run after importing. This is typically used to restore computed columns
#' ImportSet - The name of the import set. Imports are collected into particular sets of common information which 
#' should all be imported simultaneously. Quarterly claim evaluations are one such example
#' ImportOrder - The order in which files of an import set should be imported
#' MapName - The name of the import map to use. The MapName is described in the table tblImportFieldMap
#' 
#' @param channel A valid RODBC channel
#' @param ImportSet Character string of the Import Set to be returned
#' 
#' @return Data frame with Import Set information
#' 
GetImportSet = function(channel, ImportSet)
{
  df = sqlQuery(channel, paste0("SELECT * FROM tblImportFile WHERE ImportSet = '", ImportSet, "' ORDER BY ImportOrder"))
  
  df
}

#' @title GetImportMap
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' This will return a data frame with
#' MapName
#' SourceFieldName
#' RType
#' TargetFieldName
#' DBType
#' Import
#' Position
#' 
#' @param channel A valid RODBC channel
#' @param MapName Character string with the map to return
#' 
#' @return Data frame with an import map
#' 
GetImportMap = function(channel, DestinationTable)
{
  mapName = GetMapName(channel, DestinationTable)
  df = sqlQuery(channel, paste0("SELECT * FROM tblImportFieldMap WHERE MapName = '", mapName, "' ORDER BY Position"))
  
  df
}

#' @title IsTableEmpty
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' This will test whether a table is empty.
#' 
#' 
#' @param channel A valid RODBC channel
#' @param TableName Name of the table to test
#' 
#' @return Boolean indicating whether the table contains any records
#' 
IsTableEmpty = function(channel, TableName)
{
  
  df = sqlQuery(channel, paste0("SELECT TOP 1 * FROM ", TableName))
  
  return (nrow(df) == 0)
}

#' @title ClearTable
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' This will delete all records in a table.
#' 
#' @param channel A valid RODBC channel
#' @param TableName Name of the table to test
#' 
#' @return String indicating the result of clearing the table
#' 
ClearTable = function(channel, TableName)
{
  if (!IsTableEmpty(channel, TableName))
  {
    results = sqlQuery(channel, paste0("DELETE FROM ", TableName))
  } else {
    results = "The table was already empty."
  }
  
  results
}

#' @title GetMapName
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' Returns the name of the import map associated with the destination table
#' 
#' @param channel A valid RODBC channel
#' @param DestinationTable Name of the table to test
#' 
#' @return String indicating the result of clearing the table
#' 
GetMapName = function(channel, DestinationTable)
{
  df = sqlQuery(channel, paste0("SELECT MapName 
                                FROM tblImportFile 
                                WHERE DestinationTable = '", DestinationTable, "'"))
  
  if (class(df) != "data.frame" || nrow(df) ==0)  {
    warning("No records returned from tblImportFile")
    return (character(0))
  }
  
  if (is.na(df$MapName)) {return (df$MapName)}
  
  return (as.character(df$MapName))
}

#' @title GetSourceFilename
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' Returns the name of the import path and file associated with the destination table
#' 
#' @param channel A valid RODBC channel
#' @param DestinationTable Name of the table to test
#' 
#' @return Filename as a string
#' 
GetSourceFilename = function(channel, DestinationTable)
{
  df = sqlQuery(channel, paste0("SELECT SourceFilepath, SourceFilename 
                                FROM tblImportFile
                                WHERE DestinationTable = '", DestinationTable, "'"))
  
  if (class(df) != "data.frame" || nrow(df) == 0)  {
    warning("No records returned from tblImportFile")
    return (character(0))
  }
  
  filename = paste0(df$SourceFilepath, df$SourceFilename)
  
  return (filename)
}