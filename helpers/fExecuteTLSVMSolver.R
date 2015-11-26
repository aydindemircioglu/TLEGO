# FIXME: subsampling!
fExecuteTLSVMSolver = function(static, dynamic, 
		extraParameter = '',
		...) 
	{
		library(devtools)
		load_all("../SVMBridge/")
		
		# find package for solver (need to search very 'broad' because of CVM/BVM)
		wSolver = paste(static$solver, "walltime", sep = "_")
		addSVMPackage (method = wSolver)
		findSVMWrapper (method = wSolver, searchPath = "./software", verbose = TRUE)
		findSVMSoftware (method = wSolver, searchPath = "./software", verbose = TRUE)

		messagef("Working on %s", static$train)

		# generate dynamic model name, not correct, FIXME
		job = dynamic$job
		modelBasePath = paste (static$registryPath, "/models/", job$id, sep = "")
		dir.create(modelBasePath, recursive = TRUE)

		filename = paste (digest::digest (job$algo.pars, algo="sha256"), static$solver, "model", sep = ".")
		modelFile = paste(modelBasePath, "/", filename, sep = "")

		# need to fake the parameters that need to be set, but not optimized.
		epsilon = 2^(-50)
		budget = 2048
		k = budget
		epochs = 16777215
		saveFactor = 0.0
		saveExponential = 2.0
		kernelCacheSize = 1024
		finishingStep = FALSE
		
		# overwrite what we got
 		if (!is.null (static$epsilon)) {
			epsilon = static$epsilon
		}
		if (!is.null (static$budget)) {
			budget = static$budget
		}
		if (!is.null (static$k)) {
			k = static$k
		}
		if (!is.null (static$epochs)) {
			epochs = static$epochs
		}
		if (!is.null (static$cost)) {
			cost = static$cost
		}
		if (!is.null (static$gamma)) {
			gamma = static$gamma
		}
		if (!is.null (static$saveFactor)) {
			saveFactor = static$saveFactor
		}
		if (!is.null (static$saveExponential)) {
			saveExponential = static$saveExponential
		}
		if (!is.null (static$kernelCacheSize)) {
			kernelCacheSize = static$kernelCacheSize
		}
		
		if (!is.null (static$finishingStep)) {
			finishingStep = static$finishingStep 
		}
 
		# funny thing, if a wrapper does not hanadle the specific parameter,
		# it will simply ignore it.
		trainObj =  trainSVM(
				method = wSolver,
				trainDataFile = static$train,
				cost = cost, 
				gamma = gamma, 
				epsilon = epsilon,
				budget = budget,
				k = k,
				epochs = epochs,
				kernelCacheSize = kernelCacheSize,
				saveFactor = saveFactor,
				saveExponential = saveExponential,
				wallTime = static$wallTime,
				modelFile = modelFile,
				extraParameter = "",
				verbose = TRUE
		)
		
		# return
		res = list (result = trainObj,
			static = static,
			wallTime = static$wallTime,
			modelFilename = filename, 
			modelPath = modelBasePath)

		return(res)
	}

