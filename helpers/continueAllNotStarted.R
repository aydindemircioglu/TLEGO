#!/usr/bin/Rscript --vanilla

continueAllNotStarted = function (resultsPath, regRegExp = "", walltime = 1*3600, verbose = FALSE) {
	
	loadThings <- function () 	{
		source("./helpers/lsdir.R")
		source("./helpers/continueNotStarted.R")
	}
	suppressMessages(loadThings())

	folders = lsdir (resultsPath, all = FALSE, recursive = FALSE)
	for (folder in folders) {
		if  ( (grepl (regRegExp, folder) == TRUE) )
		{
			cat("Processing", folder)
			continueNotStarted (file.path (resultsPath, folder), walltime)
		}
	}
}

