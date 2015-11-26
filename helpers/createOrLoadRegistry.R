	# either load the data already reduced, or if it does not exist, reduced,
	# or if nothing exists, create a registry with all the data
	#
	createOrLoadRegistry = function (registryPath, experiment = "validate", registrySeed = 632) { 
			
		library(checkmate)
		library(BatchExperiments)

		tryCatch  (
			loadRegistry (registryPath),
			error = function (e) {
				# hard way
				unlink (registryPath, recursive = TRUE)
				print(registryPath)	
				tmpOutput = makeExperimentRegistry(id = experiment, 
					file.dir = registryPath, 
					seed = registrySeed)
			}
		)
		
		registry = loadRegistry (registryPath)
		return(registry)
	}

