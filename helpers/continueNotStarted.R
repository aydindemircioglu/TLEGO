#!/usr/bin/Rscript --vanilla

# eats a specific registry!
continueNotStarted = function (registryPath, walltime = 1*3600, verbose = FALSE) {

# stupid R
	loadThings <- function ()
	{
		library(BBmisc)
		library(ParamHelpers)
		library(BatchJobs)
		library(BatchExperiments)
		library(parallelMap)

		source("./helpers/lsdir.R")
	}
	suppressMessages(loadThings())

	# load registry for continuing
	registry = loadRegistry(registryPath, work.dir = ".")
		
	# submit the jobs
	print (findNotDone(registry))
	runIDs = findNotDone(registry)
	
	if (length(runIDs) > 1) {
		submitJobs(registry, runIDs, resources = list(walltime = walltime, memory= 4*1024))
	}
}


# minibatch:
# 	
# 
# 		# submit the jobs
# 			cIds = findNotDone (registry)
# 
# 	print(cIds)
# 	miniBatchSize = 128
# 	for (i in seq(1, length(cIds) %/% miniBatchSize)) {
# 		ids = cIds[((i-1)*miniBatchSize+1):(i*miniBatchSize)]
# 		cat(ids, "\n")
# 		submitJobs(registry, ids = ids, resources = list(walltime = 8*3600, memory= 4*1024))
# 		print("Waiting.")
#                 waitForJobs (registry)
# 	}
# 
# 	ids = cIds[(i*miniBatchSize+1):(length(cIds))]
# 	cat(ids)			
# 	submitJobs(registry, ids = ids, resources = list(walltime = 8*3600, memory= 4*1024))
# 	waitForJobs (registry)
#     	
# 	
