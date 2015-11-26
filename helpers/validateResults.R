source("helpers/createOrLoadRegistry.R")
source("helpers/validateRows.R")

#' NOTE: neither split not replication is considered
#'
#' create the validation experiments
#' for each dataset/solver combination one registry.
#'
validateResults =  function (masterTable, resultsPath = "./results/experiment1", solver = NULL, dataset = NULL, wallTime = 1 * 3600) 
{
	if (is.null(solver)) {	
		# loop over all solver, dataset combinations
		solver = sort(unique(masterTable$solver))
	}

	if (is.null (dataset)) {
		dataset = sort(unique(masterTable$dataset))
	}

	for (d in dataset) {
		for (s in solver) {
		
			# create a new registry for this problem
			registryPath = file.path (resultsPath, paste ("reg_val", s, d, "1", "1", sep = "_"))
			registry = createOrLoadRegistry (registryPath,  registrySeed = 632, experiment = "validate") 

			# collect the subtable
			subTable = subset(masterTable, solver == s & dataset == d)

			# now add these to the new registry
			registry = validateRows (registry, subTable, useTestData = FALSE)

			# send jobs, do not wait
#			submitJobs(registry, resources = list (walltime = wallTime, memory= 4 * 1024))
		}
	}
}
