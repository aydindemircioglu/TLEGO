 

trainRows = function (registry = NULL, mtable = NULL, verbose = FALSE) {
    source("./helpers/trainPoint.R")
    
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
		
        if (verbose == TRUE) {
            messagef("Adding point with data:")
            messagef("		%s as solver", solver)
            messagef("		%s as dataset", dataset)
            messagef("		%d as split", repl)
        }
        
		modelPath = mtable[r,]$modelPath
		if (is.null (modelPath) == TRUE) {
			modelPath = file.path ("results", solver, dataset)
		}
		
		if (verbose == TRUE) {
            cat ("Set model file path to ")
        }
		modelFile = mtable[r,]$modelFilename
		if (is.null (modelFile) == TRUE) {
			library(digest)
			modelFile = paste (digest::digest (unlist(match.call()), algo="sha256"), solver, "final", "model", sep = ".")		
		}
		modelFile = paste(modelPath, "/", modelFile, sep = "")
		if (verbose == TRUE) {
            cat (modelFile, "\n")
        }
		
		registry = do.call (trainPoint, 
            append (list (registry = registry, 
			#dataset = dataset, 
			#solver = solver, 
			#repl = repl, 
			#modelFile= modelFile,
			mRow = mtable[r,],
			splitsPath = "../splits"),
			as.list (mtable[r,]) ) )
	}

	return (registry)
}
