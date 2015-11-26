
source("./helpers/evaluatePoint.R")

# addToExperiment = function (registry = NULL, mtable = NULL) {
# 	if (is.null(registry) || is.null(mtable)) {
# 		stop ("Registry must not be NULL")
# 	}
# 	
# 	if (is.null(registry) || is.null(mtable)) {
# 		warning ("Table to add is NULL, will do nothing")
# 		return (registry)
# 	}
# 
# 	# for every row we start an experiment
# 	for (r in mtable) {
# 		solver = parameters[r,]$solver
# 		dataset = parameters[r,]$dataset
# 		C = parameters[r,]$C
# 		g = parameters[r,]$g
# 		wallTime = parameters[r,]$wallTime
# 		split = parameters[r,]$split
# 		subsamplingRate = parameters[r,]$subsamplingRate
# 		messagef("Adding point with data:")
# 		messagef("		%s as solver", solver)
# 		messagef("		%s as dataset", dataset)
# 		messagef("		%d as wallTime", wallTime)
# 		messagef("		%d as split", split)
# 		messagef("		%g as subsampling rate", subsamplingRate)
# 		
# 		registry  = evaluatePoint (registry = registry, dataset = dataset, solver = solver, C = C, g = g, wallTime = wallTime,
# 			repl = split, subsamplingRate = subsamplingRate, splitsPath = "../splits") {
# 	}
# 	
# 	return (registry)
# }
