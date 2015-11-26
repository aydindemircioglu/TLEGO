#!/usr/bin/Rscript --vanilla
source ("./helpers/lsdir.R")
source ("./helpers/getResultsFromRegistry.R")

mergeResults = function (resultsPath, regExp = "", verbose = FALSE) {

	if (verbose == FALSE)
		verbose = 0
	else verbose = 1
	
	folders = lsdir(resultsPath, all = FALSE, recursive = FALSE)
 
	count = 0 
	mergedTable = list()

	for (folder in folders) {
		# throw out the wXa-hack.
		if  ( (grepl (regExp, folder) == FALSE) )
		{
			if (verbose > 0)
				cat ("RegExp: Skipping folder", folder, "\n")
			next
		}    
		if (verbose > 0)
			cat ("Processing", folder, "\n")

		# load registry for continuing
		subTable = getResultsFromRegistry (file.path(resultsPath, folder))
		if (is.null(mergedTable) == TRUE)
			mergedTable = subTable
		else
			mergedTable = rbind (mergedTable, subTable)
	}
	return(mergedTable)
}
    
