#!/usr/bin/Rscript --vanilla

#' removes all registries that have open jobs.
#' needed as a by-product
#'
removeNotFinished = function (resultsPath, regRegExp = "", verbose = TRUE) {
	
	loadThings <- function () 	{
		source("./helpers/lsdir.R")
	}
	suppressMessages(loadThings())

	folders = lsdir (resultsPath, all = FALSE, recursive = FALSE)
	for (folder in folders) {
		if  ( (grepl (regRegExp, folder) == TRUE) )
		{
			cat("Processing", folder)
			removeNotFinishedPath (file.path (resultsPath, folder), verbose = verbose)
		}
	}
}


# eats a specific registry!
removeNotFinishedPath = function (registryPath, verbose = TRUE) {

# stupid R
	loadThings <- function ()
	{
		library(BBmisc)
		library(BatchJobs)
		library(BatchExperiments)
	}
	suppressMessages(loadThings())

        # load registry for continuing
        registry = loadRegistry(registryPath, work.dir = ".")
	
	# submit the jobs
	runIDs = findNotDone(registry)

	if (verbose == TRUE)
		cat ("Path",registryPath, "has",length(runIDs),"unfinished jobs.\n")	
	if (length(runIDs) > 0) {
		# ok this one is somehow broken, remove it
		if (verbose == TRUE)
			cat ("--> Removing",registryPath,"\n")
		unlink (registryPath, recursive = TRUE)
	}
}


