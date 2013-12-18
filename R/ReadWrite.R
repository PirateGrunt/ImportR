#' @title ReadCSV
#' @export 
#' 
#' @importFrom lubridate mdy
#' 
#' @description
#' Returns the name of the import path and file associated with the destination table
#' 
#' @param channel A valid RODBC channel
#' @param MapName
#' @param Filename
#' @param header
#' 
#' @return Filename as a string
#' 
ReadCSV = function(channel, MapName, Filename, header)
{
  dfImportFieldMap = GetImportMap(channel, MapName)
  dfImportFieldMap$SourceFieldName = as.character(dfImportFieldMap$SourceFieldName)
  
  dfCSV = read.csv(Filename
                , stringsAsFactors = FALSE
                , col.names = dfImportFieldMap$SourceFieldName
                , row.names = NULL
                , header = header)
  
  KeepColNames = dfImportFieldMap$SourceFieldName[dfImportFieldMap$Import == 1]
  dfCSV = dfCSV[, colnames(dfCSV) %in% KeepColNames]
  
  dfImportFieldMap = subset(dfImportFieldMap, Import == 1)
  
  dateCols = (dfImportFieldMap$RType == "Date")
  
  dfCSV[dateCols] = lapply(dfCSV[dateCols], function(x) {
    mojo = as.character(x)
    mojo = mdy(mojo, quiet=TRUE)
  })
  
  colnames(dfCSV) = dfImportFieldMap$TargetFieldName
  
  return (dfCSV)
}

#' @title SaveDF
#' @export 
#' 
#' @importFrom RODBC sqlColumns sqlSave
#' 
#' @description
#' This will save a 
#' 
#' @param channel A valid RODBC channel
#' @param MapName
#' @param Filename
#' @param header
#' 
#' @return Filename as a string
#' 
SaveDF = function(channel, dfCSV, DestinationTable, ClearTableFirst = FALSE)
{
    
  dfDBMeta = sqlColumns(channel, DestinationTable)
  
  ColumnsMissingInCSV = ! dfDBMeta$COLUMN_NAME %in% colnames(dfCSV)
  if (sum(ColumnsMissingInCSV) > 0){
    print ("There are columns present in the database, but not your CSV.")
    print (dfDBMeta$COLUMN_NAME[ColumnsMissingInCSV])
    return (NA)
  }
  
  ColumnsMissingInDB = !(colnames(dfCSV) %in% dfDBMeta$COLUMN_NAME)
  if (sum(ColumnsMissingInDB) > 0){
    print ("There are columns present in the CSV, but not your database.")
    print (colnames(dfCSV)[ColumnsMissingInDB])
    return (NA)
  }
  
  if (ClearTableFirst){ 
    ClearTable(channel, DestinationTable)}
  
  sqlSave(channel, dfCSV, DestinationTable, rownames=FALSE, append=TRUE)
}

