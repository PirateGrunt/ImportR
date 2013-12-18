#' @title RunProc
#' @export 
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @description
#' Runs a stored SQL procedure
#' 
#' @param channel A valid RODBC channel
#' @param DestinationTable Name of the table with the associated procedure
#' @param PreImport Is this a pre or post import procedure
#' 
#' @return Result of the operation
#' 
RunProc = function(channel, DestinationTable, PreImport)
{
  whichProc = ifelse(PreImport, "PreImportProc", "PostImportProc")
  procName = sqlQuery(channel, paste0("SELECT ", whichProc 
                                      , " FROM tblImportFile"
                                      , " WHERE DestinationTable = '", DestinationTable, "'"))
  
  if (length(procName) != 1){
    warning ("No record found for your table in function RunProc.")
    return (NA)
  }
  
  procName = as.character(procName[1,1])
  if (length(procName) == 0){
    warning (paste0("Procname is blank for DestinationTable ", DestinationTable))
  }
  
  result = sqlQuery(channel, paste0("EXEC ", procName))
  
}
