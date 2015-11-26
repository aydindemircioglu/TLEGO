



#' check all results if are any not-done jobs that have nonetheless models
#'
countOrphans = function (resultsPath, regExp = "reg_exp", verbose = FALSE) {

	orphansTable = NULL
	nOrphans = 0

	# first all registries with the initial data
	folders = lsdir(resultsPath, all = FALSE, recursive = FALSE)
	registryPathList = c()
	for (folder in folders) {
		if  ( (grepl (regExp, folder) == TRUE)  ) {
			# read and process registry
			if (verbose == TRUE)
				cat ("Retrieving from folder ", folder, "\n")

			registry = loadRegistry(file.path(resultsPath, folder))
			ids = findNotDone (registry)

			# now we iterate by hand over all these things and create a orphantable
			for (i in ids) {
				#cat ("Deorphaning job id ", i, "\n")
				modelPath = file.path(registryPath, "models", i)
				content = list.files (modelPath)
				
				if (length(content) ==  0) {
					# this job was not even started?
					# this then must be computed at the update step
				} else {
					# this is now an true orphan. 
					nOrphan = nOrphan + 1
					
					jobinfo = getJob (registry, i)
					cat ("  Found orphan at ", modelPath, "for C=", jobinfo$algo.pars$C, "and g=", jobinfo$algo.pars$g, "\n")
				}
			}
		}   
	}
	
	return (nOrphans)
}
