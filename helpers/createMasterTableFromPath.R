
	library(BatchExperiments)

	source("helpers/lsdir.R")
	source("experiments/getGridSize.R")
	source("helpers/sourceDir.R")
	source("helpers/getResultsFromRegistry.R")
	source("helpers/addResultsToMasterTable.R")
	source("helpers/unfactorize.R")
	
createMasterTable = function (resultsPath, regExp = "", mergeKeys = c("C", "g", "static.dataset", "static.solver"), verbose = TRUE) {

	masterTable = NULL

	# first all registries with the initial data
		folders = lsdir(resultsPath, all = FALSE, recursive = FALSE)
		registryPathList = c()
		for (folder in folders) {
			if  ( (grepl (regExp, folder) == TRUE)  ) {
				# read and process registry
				if (verbose == TRUE)
					cat ("Retrieving from folder ", folder, "\n")
				results = getResultsFromRegistry (registryPath = file.path(resultsPath, folder))
				results = unfactorize (results)
				cat ("  Registry has", nrow(results), "entries\n")
				masterTable = addResultsToMasterTable (results, masterTable, mergeKeys = mergeKeys)
				cat ("  MasterTable has", nrow(masterTable), "entries\n")
			}   
		}
	return(masterTable)
}
