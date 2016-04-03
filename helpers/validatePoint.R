
source ("./helpers/fValidateTLSVM.R")


.dynamicFunction = function(job, static, ...){
	list(job = job)
}


validatePoint = function (registry = NULL, dataset = NULL, solver = NULL, #C = NULL, g = NULL, 
        repl = NULL, splitsPath = "./splits", modelFile = NULL, test = FALSE, 
        verbose = TRUE,
        experimentID = NULL, ...)
{

	if (is.null (experimentID) == TRUE) {
        experimentID = registry$id
    }
    
    if (is.null(repl) == TRUE) {
        stop ("Replication/Split not chosen!")
    }
    
	if (verbose == TRUE) {
        cat ("Experiment ID is ", experimentID, "\n")
    }
    
	if ((is.null(modelFile) == TRUE))
		stop ("Not given any model file!")

	if ((is.null(registry) == TRUE))
		stop ("Not given any registry!")

	if ((is.null(dataset) == TRUE) | (is.null(solver) == TRUE))
		stop ("Not given any solver or data set")

	# 	create a job
	valid.file = paste(splitsPath, "/", dataset, "_", repl, "/validData.dat", sep = "")
	if (test == TRUE) {
		valid.file = paste(splitsPath, "/", dataset, "_", repl, "/testData.dat", sep = "")
	}
		
	# add our problem
   		IDstring = paste(c(dataset, solver, modelFile, splitsPath, repl, ...))
		probID = paste ("id", digest::digest (IDstring, algo="sha256"), sep = "_")
        static = append( list(	valid = valid.file, 
							solver = solver,
							dataset = dataset,
							modelFile = modelFile,
							registryPath = registry$file.dir), list(...))

# 
#         # check if we have this experiment already -- TODO: unravel this stupid go-for-id and now go-for-resources duplication removal things :/
#         for (j in getJobIds(registry)) {
#             r = loadResult (registry, id = j, missing.ok = TRUE)
#             if (is.null(r)) {
#                 next
#             }
# 
#             ultra = FALSE
#             if (ultra == TRUE) {
#                 cat (r$static$solver, " vs ", solver, "\n")  
#                 cat (r$static$dataset , " vs ",  dataset, "\n")
#                 cat (r$static$valid , " vs ",  valid.file, "\n")
#                 cat (basename(as.character(r$static$mRow$modelFilename))  , " vs ",  basename (modelFile), "\n")
#             }
#             
#             QUICKFIX = FALSE # fix for wrong modelfilenamefor snapshots
#             if (QUICKFIX == TRUE) {
#                 if ((r$static$solver == solver)  & (r$static$dataset == dataset) & (r$static$valid == valid.file) ) {
#                     cat ("PROBLEM ", j, " is KNOWN!\n")
#                     return (registry)
#                 }
#             } else {
#                 if ((r$static$solver == solver)  & (r$static$dataset == dataset) & (r$static$valid == valid.file) & 
#                     (basename(as.character(r$static$mRow$modelFilename)) == basename (modelFile))) {
#                     cat ("PROBLEM ", j, " is KNOWN!\n")
#                     return (registry)
#                 }
#             }
#         }
                                    
		quitProblem = FALSE
		result = tryCatch({
			addProblem(registry, 
				id = probID,
				dynamic = .dynamicFunction,
    			overwrite = FALSE,
				static = static
			)
		}, warning = function(w) {
            cat ("\n\nThere was a warning!\n", w, "\n")
        }, error = function(e) {
            # we have this experiment, so we do not do anythin
            stop ("\n\nThere was an error!\n", e, "\n")
            quitProblem = TRUE
        }, finally = {
        })
		
		if (quitProblem == TRUE) {
            cat ("Quitting.\n")
            return (registry)
        }
		
		cat ("## Adding experiment with ", solver, " on ", dataset, " (split ", repl, ")\n")
        cat ("## expID = ", experimentID, "\n")
        cat ("## registry.expID = ", registry$id, "\n")
        cat ("## probID = ", probID, "\n")
		data.design = makeDesign (probID)

			
	# define algorithm		 
		addAlgorithm(registry, 
            id = experimentID,
            fun = fValidateTLSVM, 
			overwrite = TRUE)
		algo.design = makeDesign (experimentID)

	# finally..
		addExperiments (registry, 
			algo.designs = algo.design, 
			prob.design = data.design, 
			skip.defined = TRUE)

	return (registry)
}

