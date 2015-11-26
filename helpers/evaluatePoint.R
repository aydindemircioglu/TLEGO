
source ("./helpers/fExecuteTLSVMSolver.R")


.dynamicFunction = function(job, static, ...){
	list(job = job)
}


evaluatePoint = function (registry = NULL, dataset = NULL, solver = NULL, #C = NULL, g = NULL, 
	wallTime = NULL, repl = 1, subsamplingRate = 1.0, splitsPath = "../splits", experimentID = "myexp", ...) {

	if ((is.null(registry) == TRUE))
		stop ("Not given any registry!")

# 	# check for C and g
# 	if ((is.null(C) == TRUE) | (is.null(g) == TRUE))
# 		stop ("Not given any C or g")

	if ((is.null(dataset) == TRUE) | (is.null(solver) == TRUE))
		stop ("Not given any solver or data set")

	if ((is.null(wallTime) == TRUE) )
		stop ("Not given any walltime")

	# 	create a job
		repl = 1 
		train.file = paste(splitsPath, "/", dataset, "_", repl, "/trainData.dat", sep = "")
		test.file = paste(splitsPath, "/", dataset, "_", repl, "/validData.dat", sep = "")

	# add our problem
		probID = paste ("id", digest::digest (paste(c(as.list(environment()))), algo="sha256"), sep = "_")
		addProblem(registry, 
			id = probID,
			dynamic = .dynamicFunction,
#			overwrite = TRUE,
			static = append( list(	train = train.file, 
							test = test.file, 
							solver = solver,
							wallTime = wallTime,
							dataset = dataset,
							subsamplingRate = subsamplingRate,
							registryPath = registry$file.dir), list(...))
		)
		data.design = makeDesign (probID)

			
	# define algorithm		 
		addAlgorithm(registry, 
			id = experimentID,
			fun = fExecuteTLSVMSolver, 
			overwrite = TRUE)
		algo.design = makeDesign (experiment)

	# finally..
		addExperiments (registry, 
			algo.designs = algo.design, 
			prob.design = data.design, 
			skip.defined = TRUE)

	return (registry)
}
