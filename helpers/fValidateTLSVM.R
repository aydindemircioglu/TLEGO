	
	fValidateTLSVM = function(static, dynamic, 
		extraParameter = '',
		...) 
	{
		library(SVMBridge)

		pathForMethod = function (method = NA) {
			# stupid check
			path = method
			if (method == "CVM") 
				path = "libCVM"
			if (method == "BVM") 
				path = "libCVM"
			if (method == "BSGD") 
				path = "BudgetedSVM"
			return (path)
		}

		binaryForMethod  = function (method = NA, bin = NA) {
			# stupid check
			if ((method == "LASVM") && (bin == "test"))
				bin = "la_test"
			if ((method == "LASVM") && (bin == "train"))
				bin = "la_svm"
			if ((method == "LIBSVM") && (bin == "test"))
				bin = "svm-predict"
			if ((method == "LIBSVM") && (bin == "train"))
				bin = "svm-train"
			if ((method == "BSGD") && (bin == "test"))
				bin = "budgetedsvm-predict"
			if ((method == "BSGD") && (bin == "train"))
				bin = "budgetedsvm-train"
			if ((method == "SVMperf") && (bin == "test"))
				bin = "svm_perf_classify"
			if ((method == "SVMperf") && (bin == "train"))
				bin = "svm_perf_learn"
			if ((method == "CVM") && (bin == "test"))
				bin = "svm-predict"
			if ((method == "CVM") && (bin == "train"))
				bin = "svm-train"
			if ((method == "BVM") && (bin == "test"))
				bin = "svm-predict"
			if ((method == "BVM") && (bin == "train"))
				bin = "svm-train"
				
			return (bin)
		}

# TODO: FIXME: OMG.
		softwarePath = "./software/"

		wSolver = paste(static$solver, "walltime", sep = "_")
		wrapperName = paste0 (wSolver, "_wrapper.R")
		wrapperPath = file.path (softwarePath, pathForMethod(static$solver), wrapperName) 
		#print (wrapperPath)

		testBinaryPath = file.path(softwarePath, pathForMethod (static$solver), "bin", binaryForMethod (static$solver, bin = "test"))
		trainBinaryPath = file.path (softwarePath, pathForMethod (static$solver), "bin", binaryForMethod (static$solver, bin = "train"))
				
		addSVMPackage (method = wSolver, trainBinaryPath = trainBinaryPath,
			testBinaryPath = testBinaryPath,
			wrapperPath  = wrapperPath ,
			verbose = TRUE)

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
		
