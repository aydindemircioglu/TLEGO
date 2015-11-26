
getResultsFromRegistry = function (registryPath = NULL, njobs = 5) {
	library(checkmate)
	assertDirectory( registryPath, access = "rw")
	reducedDataFile = file.path(registryPath, "reduced.data")

	# check if it is reduced already
	if (testFile (reducedDataFile) == TRUE) {
		# load the results instead of reducing
		load (reducedDataFile)
	} else {
		# reduce and save results
		registry = loadRegistry(registryPath)
		results = reduceResultsExperimentsParallel(registry, njobs = njobs, fun = function(job, res){source("./helpers/LinearizeNestedList.R"); return( LinearizeNestedList (res, LinearizeDataFrames=TRUE)) })
		save (results, file = reducedDataFile)
	}	
	
	return (results)
}

