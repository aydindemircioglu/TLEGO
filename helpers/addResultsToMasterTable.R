

#' addResultsToMasterTable
#'
#' add or merge given results to the master table.
#'
#' @note		if the results contain multiple entries for a point (C, g, solver, dataset and seconds
#'					determine a point), then they will be updated consequently, i.e. the last of such
#'					multiple entries survive.
#'
addResultsToMasterTable = function (results = NULL, masterTable = NULL, mergeKeys = c("solver", "dataset", "C", "g", "seconds")) {
  library(checkmate)
  testDataFrame(results, "data.frame")
  
  if (is.null(masterTable)) {
  	masterTable = results	
	return (masterTable)
  }
  testDataFrame(masterTable, "data.frame", min.rows = 1)
  
  # if results has no rows, we are done.
  if (nrow(results) < 1) {
    return (masterTable)
  }
  
  # test if masterTable has all the things we need
  testSubset(mergeKeys, colnames(masterTable))
  
  # we need the same for the results table, else we cannot match the rows
  testSubset(mergeKeys, colnames(results))
  
  # before we do anything, we make sure that the columns do fit
  # if we have extra columns in results, we add those to the mastertable
  extraCols = setdiff (colnames(results), colnames(masterTable))
  
  for (e in extraCols) {
    masterTable[[e]] = NA
    message ("Adding new row ", e, "\n")
  }
  
  # now we can splice all rows
  # two possiblities: a row from results does not exist at all,
  # or it exists and needs updating. for consistency there must be
  # no more than one row in masterTable matching the results.
  for (i in 1:nrow(results)) {
    curRow = results[i, ]
    # this probably can be done better :/
    rowsToUpdate = 1:nrow(masterTable)
    for (k in mergeKeys) {
      rowsToUpdate = which (masterTable[rowsToUpdate, k] == curRow[[k]])
    }
 #   print (rowsToUpdate)
#    print ("N")
    if (length(rowsToUpdate) == 0) {
      # need to add the row 
      # we could add straight ahead, problem is only that probably the row contains
      # data our table does not have. for this we should use merge
      
      newRow = masterTable[1,]
      newRow[1,] = NA
      
      # remove columns that we want to overwrite. as we already added all columns
      # from the result table to our table, this should actually be all columns of results 
      # minus the C,g ...
      overwriteCols = setdiff (intersect (colnames(masterTable), colnames(results)) , mergeKeys)
      for (r in overwriteCols) {
        newRow[[r]] = NULL
      }
      newRow = merge (newRow, curRow, by = mergeKeys, all = TRUE)

      # add to masterTable
      masterTable = rbind (masterTable, newRow[1,])
    } else if (length(rowsToUpdate) > 1) {
      # consistency problem. there should never be two results for one point
      # as C, g, solver, dataset, seconds uniquely determines one result
      stop ("More than one row found in masterTable to update. This should not happen, it is not consistent.")
    } else {
      # exactly one row exists, so update
      # remove columns that we want to overwrite. as we already added all columns
      # from the result table to our table, this should actually be all columns of results 
      # minus the C,g ...
      updateRow = masterTable[rowsToUpdate,]
      overwriteCols = setdiff (intersect (colnames(masterTable), colnames(results)) , mergeKeys)
#      print ("A")
 #     print (curRow)
      for (r in colnames(curRow)) {
        masterTable[rowsToUpdate,r] = curRow[1,r]
      }
  #    print ("B")
    }
  }
  return (masterTable)
}




