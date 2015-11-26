

#' given some specific registry path, load the registry there and
#' check if are any not-done jobs that have nonetheless models
#' if so, add the model to some orphans.data list below the registry
#'
getOrphansFromRegistry = function (registryPath = NULL) {
	library(checkmate)
	assertDirectory( registryPath, access = "rw")
	orphansDataFile = file.path(registryPath, "orphans.data")
	
	# check if it is reduced already
	if (testFile (orphansDataFile) == TRUE) {
		# load the results instead of reducing
		load (orphansDataFile)
	} else {
		# reduce and save results
		registry = loadRegistry(registryPath)
		ids = findNotDone (registry)
		cat ("Found ", length(ids), " orphans.\n")

		# now we iterate by hand over all these things and create a orphantable
		for (i in ids) {
			#cat ("Deorphaning job id ", i, "\n")
			modelPath = file.path(registryPath, "models", i)
			content = list.files (modelPath)
			
			if (length(content) ==  0) {
				# this job was not even started?
				# this then must be computed at the update step
			} else {
				# for every file we need to add an entry to our orphantable
				for (afile in content) {
					seconds = suppressWarnings(as.numeric(tail(strsplit(afile, '_')[[1]], n = 1)))
					if (is.na(seconds) == TRUE) {
						messagef("Found model final model %s", afile)
						messagef("THIS SHOULD NEVER HAVE HAD HAPPENED (finished jobs must have an entry in the registry)")
						messagef("Pouting now. Think about what you did. Really.")
						die()
					} else { 
						messagef("Found model %s at %g seconds.", afile, seconds)
						currentModel = paste(file.path (modelPath, afile), seconds, sep = "_")
					}
					
					# add to our table
					jobinfo = getJob (registry, i)
					# we do not have the solver :/ again stupid me.
					# registry__07_04_2015__00_29_19_BSGD_aXa_1
					tmpsplit = strsplit (modelPath, '_')[[1]]
					solver =  	tmpsplit [length (tmpsplit) - 2]
					orphanTable = rbind( orphanTable, data.frame(
							solver,
							jobinfo$prob.id, #prob.id is dataset
							jobinfo$algo.pars$C,
							jobinfo$algo.pars$g,
							seconds,
							modelPath,
							currentModel,
							#p$modelFilename,
							jobinfo$repl
							)
					)
				}
			}
		}
		
		# save results
		results = orphanTable
		save (results, file =orphansDataFile)
	}	
	
	return (results)
}
