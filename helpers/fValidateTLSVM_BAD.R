	
	fValidateTLSVM = function(static, dynamic, 
		extraParameter = '',
		...) 
	{
		library(SVMBridge)
		
		# find package for solver (need to search very 'broad' because of CVM/BVM)
		wSolver = paste(static$solver, "walltime", sep = "_")
		addSVMPackage (method = wSolver)
		findSVMWrapper (method = wSolver, searchPath = "./software", verbose = TRUE)
		findSVMSoftware (method = wSolver, searchPath = "./software", verbose = TRUE)

		messagef("Working on %s", static$valid)

		predictionsFile = NULL
		computePredictions = TRUE
		if (computePredictions == TRUE) {
			job = dynamic$job
			predictionsBasePath = paste (static$registryPath, "/predictions/", job$id, sep = "")
			dir.create(predictionsBasePath , recursive = TRUE)
			
			# creating a new md5 hash for the filename is suboptimal,
			# using the original name makes it easier later.
			useOldName = TRUE
			if (useOldName == FALSE) {
				filename = paste (digest::digest (job$algo.pars, algo="sha256"), static$solver, "predictions", sep = ".")

				# get seconds
				seconds = suppressWarnings(as.numeric(tail(strsplit(static$modelFile, '_')[[1]], n = 1)))
				if (is.na(seconds) == TRUE) {
						messagef("Found final model without seconds")
				} else { 
						messagef("Found model at %g seconds.", seconds)
						filename = paste (filename, seconds, sep = "_")
				}
			} else {
				filename = basename(static$modelFile)
			}

			predictionsFile = paste(predictionsBasePath, "/", filename, sep = "")
		}
		
		# funny thing, if a wrapper does not handle the specific parameter,
		# it will simply ignore it.

		
		# we want to generate here all the needed statistics
		testObj =  testSVM(
			method = wSolver,
			testDataFile = static$valid,
			modelFile = as.character(static$modelFile),
			predictionsFile = as.character(predictionsFile),
			verbose = TRUE
		)  
		
		# return
		res = list (result = testObj,
			static = static,
			predictionsFile = predictionsFile)

		return(res)
	}
		
