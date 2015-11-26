

#' make sure that the timings fit somehow to the model-name and fix if possible	
fixTimings = function (masterTable) {

	for (p in 1:nrow(masterTable)) {
		cr = masterTable[p,]
		# extract seconds 
		seconds = cr$seconds

		# extract seconds from modelname 
		trueseconds = suppressWarnings(as.numeric(tail(strsplit(cr$modelFilename, '_')[[1]], n = 1)))
		
		# can be NA if its the final model. then we cannot check.
		if (is.na(trueseconds) == FALSE) {
			
			if (seconds != trueseconds) {
				cat("Fixing:", seconds, "should be", trueseconds, "\n")
				masterTable[p,]$seconds = trueseconds
			}
		}
	}
	
	return (masterTable)
}
