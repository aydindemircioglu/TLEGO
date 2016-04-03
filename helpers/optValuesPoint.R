
source ("./helpers/fOptValues.R")


.dynamicFunction = function(job, static, ...){
	list(job = job)
}


optValuesPoint = function (registry = NULL, dataset = NULL, solver = NULL, 
	repl = 1, splitsPath = "./splits", modelFile = NULL, experiment = "optValues", ...) {

	if ((is.null(modelFile) == TRUE))
		stop ("Not given any model file!")

	if ((is.null(registry) == TRUE))
		stop ("Not given any registry!")

	if ((is.null(dataset) == TRUE) | (is.null(solver) == TRUE))
		stop ("Not given any solver or data set")

	# 	create a job
	train.file = paste(splitsPath, "/", dataset, "_", repl, "/trainData.dat", sep = "")

	
	# add our problem
		probID = paste ("id", digest::digest (paste(c(as.list(environment()))), algo="sha256"), sep = "_")
		addProblem(registry, 
			id = probID,
			dynamic = .dynamicFunction,
			static = append( list(	train = train.file, 
							solver = solver,
							dataset = dataset,
							modelFile = modelFile,
							registryPath = registry$file.dir), list(...))
		)
		data.design = makeDesign (probID)

			
	# define algorithm		 
		addAlgorithm(registry, 
			id = registry$id,
			fun = fOptValues, 
			overwrite = TRUE)
		algo.design = makeDesign (experiment)

	# finally..
		addExperiments (registry, 
			algo.designs = algo.design, 
			prob.design = data.design, 
			skip.defined = TRUE)

	return (registry)
}

