
source ("./helpers/fExecuteTLSVMSolver.R")


.dynamicFunction = function(job, static, ...){
	list(job = job)
}


trainPoint = function (registry = NULL, dataset = NULL, solver = NULL, verbose = FALSE, #C = NULL, g = NULL,  
	wallTime = NULL, repl = 1, subsamplingRate = 1.0, splitsPath = "../splits", experimentID = NULL, ...) {

	if (is.null (experimentID) == TRUE) {
        experimentID = registry$id
    }
    
	if (verbose == TRUE) {
        cat ("Experiment ID is ", experimentID, "\n")
    }
	
	if ((is.null(registry) == TRUE))
		stop ("Not given any registry!")

# 	# check for C and g
# 	if ((is.null(C) == TRUE) | (is.null(g) == TRUE))
# 		stop ("Not given any C or g")

	if ((is.null(dataset) == TRUE) | (is.null(solver) == TRUE))
		stop ("Not given any solver or data set")

		
	if ((is.null(wallTime) == TRUE) )
		stop ("Not given any walltime")

	# 	create a job
		train.file = paste(splitsPath, "/", dataset, "_", repl, "/trainData.dat", sep = "")
		test.file = paste(splitsPath, "/", dataset, "_", repl, "/validData.dat", sep = "")

	# add our problem
        IDstring = paste(c(dataset, solver, wallTime, ...))
		probID = paste ("id", digest::digest (IDstring, algo="sha256"), sep = "_")
        static = append( list(	train = train.file, 
                        test = test.file, 
                        solver = solver,
                        wallTime = wallTime,
                        dataset = dataset,
                        subsamplingRate = subsamplingRate,
                        registryPath = registry$file.dir), list(...))

                        
                        ## THIS IS ALSO WRONG-- if no result is there the one should be reused? something like this?
                        
#                         
#         # check if we have this experiment already -- TODO: unravel this stupid go-for-id and now go-for-resources duplication removal things :/
#         for (j in getJobIds(registry)) {
#             r = loadResult (registry, id = j, missing.ok = TRUE)
#             if (is.null(r)) {
#                 next
#             }
#             if ((r$static$solver == solver) & (r$static$wallTime == wallTime) & (r$static$dataset == dataset) & (r$static$train == train.file)) {
#                 cat ("PROBLEM ", j, " is KNOWN!\n")
#                 return (registry)
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
            return (registry)
        }
		
		cat ("## Adding experiment with ", solver, " on ", dataset, " (split ", repl, ")\n")
		
		data.design = makeDesign (probID)

			
	# define algorithm		 
		addAlgorithm(registry, 
			id = experimentID,
			fun = fExecuteTLSVMSolver, 
			overwrite = TRUE)
		algo.design = makeDesign (experiment)

	# finally..
		addExperiments (registry, 
			algo.designs = algo.design, 
			prob.design = data.design, 
			skip.defined = TRUE)

	return (registry)
}
