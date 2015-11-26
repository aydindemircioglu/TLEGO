#!/usr/bin/Rscript --vanilla
source ("./helpers/lsdir.R")

countNotFinishedExperiments = function (resultsPath, regExp = "", verbose = FALSE) {

	if (verbose == FALSE)
		verbose = 0
	else verbose = 1
	
	folders = lsdir(resultsPath, all = FALSE, recursive = FALSE)
 
	count = 0 
	rowForTable = list()

	mym = ""
	for (folder in folders) {
		# throw out the wXa-hack.
		if  ( (grepl (regExp, folder) == FALSE) )
		{
			if (verbose > 0)
				mym = paste (mym, "RegExp: Skipping folder", folder, "\n", sep = " ")
			next
		}    
		if (verbose > 0)
			mym = paste(mym, "Processing", folder, "\n", sep = " ")

		# load registry for continuing
		registry = BatchJobs::loadRegistry(file.path(resultsPath, folder))#, work.dir = "."))
		
		o = (BatchJobs::findNotDone(registry))
		count = count + length(o)
		if (length(o) > 0)
		{
			mym = paste(mym, "###",  folder, "has", length(o), "unfinished jobs.\n", sep = " ")
			if (verbose > 0)
				print(o)#	mym = paste (mym, "     ", o)
			
		}
	}
	cat (mym)
	cat ("Total number of unfinished jobs:", count,"\n")
	return(count)
}
    
	
