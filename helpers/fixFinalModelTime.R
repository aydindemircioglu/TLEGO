

#' fix final model time
#'
#' idea (old): re-read the true training time of the final model and fix the dummy one (=walltime)
#' idea (old2): replace ALL training times to the true ones.
#' idea (new): the training time has been saved in the masterTable, as $trainTime
#' no thing needs to be done here.

fixFinalModelTime = function (masterTable) {	

	stop ("Are you sure you need this function?")

	# check if there is a row of what we want
	if (!"validationError" %in% colnames(masterTable)) {
		# we also save the time, so we check for that too
		if (!"validationTime" %in% colnames(masterTable)) {
			# now we are f...ked up.
			stop("Sorry, something gone wrong, we have no validation or validation time!")
		}
	}

	# prepare results
	
	# proceed row by row of loaded registry
	for (p in 1:nrow(masterTable)) {
		# now that we have the registry we search for all FINAL models
		if  ( (grepl (p$modelFilename, afile) == TRUE)  ) {
			seconds = suppressWarnings(as.numeric(tail(strsplit(afile, '_')[[1]], n = 1)))
			if (is.na(seconds) == TRUE) {
				messagef("Found model final model %s", afile)
				seconds = p$wallTime
				messagef("Walltime for model is %g seconds.", seconds)
				currentModel = file.path (p$modelPath, p$modelFilename)
			} else { 
				messagef("Found model %s at %g seconds.", afile, seconds)
				currentModel = paste(file.path (p$modelPath, p$modelFilename), seconds, sep = "_")
			}
		}
	}
}

