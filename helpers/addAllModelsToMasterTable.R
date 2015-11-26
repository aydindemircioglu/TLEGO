#!/usr/bin/Rscript --vanilla


#' add all computed models to mastertable.
#' 
#' traverse through the given mastertable. for all rows, find all corresponding models 
#' and return a mastertable with all of them added
#' IT WILL NOT DETECT IF A JOB WAS NOT STARTED AT ALL (as then usually there
#' will be no models/ID folder.
#' use one of the other checkers to see, if such things exist (e.g. determineMissingCases.R)
#'
addAllModelsToMasterTable = function (masterTable, resultsPath, verbose = FALSE) {
	
	loadThings <- function ()
	{
		library(BBmisc)
		source ("./helpers/lsdir.R")
	}
	suppressMessages(loadThings())

	newModelsTable = NULL
	
	# need to 'cache'
	subTable = NULL
		
	# traverse master table
	for (p in 1:nrow(masterTable)) {
		
		cr = masterTable[p,]
		
		# first check if the model is existent at all, i.e. if the modelpath is correct. if not, we quit
		# to force the user to make sure all model paths are correct.
		models = list.files (cr$modelPath)
		for (model in models) {
			seconds = suppressWarnings(as.numeric(tail(strsplit(model, '_')[[1]], n = 1)))
			if (is.na(seconds) == FALSE) {
				# we have a nice intermediate model, modify a copy of cr and rbind it
				nr = masterTable[p,]
				
				# correct entries
				nr$trainTime = seconds
				nr$modelFilename = model

				# stupid, but all men must die.
				if (is.null (nrow(subTable ))) {
					n = nrow(newModelsTable)
					if (is.null(n) == TRUE)
						n = 0
						
					nr$id = nrow(masterTable) + n + 1
					subTable = nr
				} else {
					n = nrow(newModelsTable)
					if (is.null(n) == TRUE)
						n = 0

					nr$id = nrow(masterTable) + n + nrow(subTable) + 1
					subTable = rbind (subTable , nr)
				}
			} 
		}
		
		if (nrow(subTable) > 1000) {
			if (is.null (newModelsTable) == TRUE) {
				newModelsTable = subTable
			} else {
				newModelsTable = rbind (newModelsTable, subTable)
			}
			subTable = NULL
			cat (".")
		}
	}
	
	cat ("X\n")
	ret = rbind (masterTable, newModelsTable)
	return (ret)
}

