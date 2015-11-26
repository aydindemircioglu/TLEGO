
# just iterate over all modelfiles in table and check if it exists on disk
# TODO: override.path will be used to override the path 
checkModelFiles = function(parameters = NULL, pathmodifier = NULL) {

	missingFiles = NULL

	for (r in nrow(parameters)) {
		curFile = parameters[r,]$modelFilename
		if (file.exists(curFile) == FALSE) {
			missingFiles  = rbind (missingFiles, parameters[r, ])
		}
	}

	print (missingFiles)
	return (missingFiles)
}

