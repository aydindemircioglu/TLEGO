

validateRows = function (registry = NULL, mtable = NULL, 
        verbose = TRUE,
        useTestData = FALSE, 
        takeSnapshotModel = FALSE ) 
{
    source("./helpers/validatePoint.R")
    source("./helpers/lsdir.R")

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
		# stupid :/ overwrite modelPath
		
		modelFilename = mtable[r,]$modelFilename
		modelFile = paste(modelPath, "/", modelFilename, sep = "")
		
		# if there are other models, we take the first of those-- stupid fix, TODO: make it better
		snapshotModelFilename  = NULL
		if (takeSnapshotModel == TRUE) {
            #print (modelFilename)
            fileList = lsdir(modelPath, listFiles = TRUE) # lido will kill me for this.
            idx = grep (paste0(modelFilename, "_"), fileList)
            if (length(idx) > 0) {
                snapshotModelFilename = fileList[idx][1]
                cat ("Will also test with model residing in ", snapshotModelFilename, "\n")
            } else {
                cat ("No snapshot model was found.\n")
            #    print (fileList)
            }
		}
		
		registry = validatePoint (registry = registry, 
			dataset = dataset, 
			solver = solver, 
			repl = repl, 
			test = useTestData,
			modelFile= modelFile,
			mRow = mtable[r,],
			splitsPath = "./splits")

        if (is.null (snapshotModelFilename) == FALSE) {
            mRow = mtable[r,]
            snapshotModelFile = paste(modelPath, "/", snapshotModelFilename, sep = "")
            mRow$modelFilename = snapshotModelFile

            registry = validatePoint (registry = registry, 
                dataset = dataset, 
                solver = solver, 
                repl = repl, 
                test = useTestData,
                modelFile= snapshotModelFile,
                mRow = mRow,
                splitsPath = "./splits")
        }
			
	}

	return (registry)
}
