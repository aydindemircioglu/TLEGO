
source("./helpers/optValuesPoint.R")

optValuesRows = function (registry = NULL, mtable = NULL) {
	if (is.null(registry) || is.null(mtable)) {
		stop ("Registry must not be NULL")
	}
	
	if (is.null(registry) || is.null(mtable)) {
		warning ("Table to add is NULL, will do nothing")
		return (registry)
	}

	# for every row we start an experiment
	for (r in 1:nrow(mtable)) {
		solver = mtable[r,]$solver
		dataset = mtable[r,]$dataset
		repl = mtable[r,]$repl
		
		messagef("Adding point with data:")
		messagef("		%s as solver", solver)
		messagef("		%s as dataset", dataset)
		messagef("		%d as split", repl)

		modelPath = mtable[r,]$modelPath
		# stupid :/ overwrite modelPath
		
		modelFile = mtable[r,]$modelFilename
		modelFile = paste(modelPath, "/", modelFile, sep = "")
		
		registry = optValuesPoint (registry = registry, 
			dataset = dataset, 
			solver = solver, 
			repl = repl, 
			C = mtable[r,]$C,
			modelFile= modelFile,
			mRow = mtable[r,],
			splitsPath = "../splits")
	}

	return (registry)
}
