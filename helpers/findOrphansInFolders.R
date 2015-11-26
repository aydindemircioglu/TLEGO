#!/usr/bin/Rscript --vanilla

#' find orphans only based on model files, not the master table
#' i.e. transverse over all folders (reg_exp_...) and look in each of
#' the model/folders. warn then if no model can be found at all
#' or if no final model exists.
#' 
findOrphansInFolders = function (resultsPath, verbose = FALSE) {
	
	loadThings <- function ()
	{
		library(BBmisc)
		source ("./helpers/lsdir.R")
	}
	suppressMessages(loadThings())

    
	# determine registries
    folders = lsdir (resultsPath, all = FALSE, recursive = FALSE)
    registryPathList = c()
	
	totalnotStarted = 0
	totalnoFinalModel = 0
	
    for (folder in folders) {
        if  ( (grepl ("reg_exp", folder) == TRUE)  ) {

			notStarted = 0
			noFinalModel = 0
			cat (folder, "\n")
	
			modelFolders = lsdir(file.path (resultsPath, folder, "models"), all = FALSE, recursive = FALSE)
			for (modelFolder in modelFolders) {

				content = list.files (file.path(resultsPath, folder, "models", modelFolder))
				if (length (content) ==  0) {
					# this job was not even started?
					cat (" --JOB", folder, "WAS NOT STARTED?\n")
					if (is.null(grep("lidong", (Sys.info()["nodename"]))) == FALSE) {
						# cat ("   submitting job.\n")	
						#submitJobs(registry, ids = c(i), resources = list(walltime = 8*3600, memory= 4*1024))
					}
					notStarted = notStarted + 1
				} else {
				
					finalModelExists = FALSE
					for (afile in content) {
						seconds = suppressWarnings(as.numeric(tail(strsplit(afile, '_')[[1]], n = 1)))
						if (is.na(seconds) == TRUE) {
							finalModelExists = TRUE
						}
					}
					
					if (finalModelExists == FALSE) {
						noFinalModel = noFinalModel + 1
					}
				}
			}
			
			if ( (notStarted > 0) || (noFinalModel > 0)) {
				cat ("## Not started:    ", notStarted, "\n")
				cat ("   No final model: ", noFinalModel, "\n")
			}
			totalnotStarted = totalnotStarted + notStarted
			totalnoFinalModel =  totalnoFinalModel + noFinalModel
		}
		
	}
	cat ("############### TOTAL \n")
	cat ("## Not started:    ", totalnotStarted, "\n")
	cat ("   No final model: ", totalnoFinalModel, "\n")
}
	

