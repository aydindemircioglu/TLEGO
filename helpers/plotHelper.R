
    
	getSolverList = function() {
		return (c("BSGD", "LASVM", "LLSVM", "BVM", "CVM", "LIBSVM", "Pegasos", "SVMperf", "PARETO"))
	}
	
	getColorList = function() {
		return (c("orange","violet", "black", "turquoise", "green", "red", "yellow", "blue", "brown"))
	}
		
	getColor = function (solver) {
		solverList = getSolverList ()
		solverColor = getColorList ()
		color = solverColor[which (solverList %in% solver)]
		return (color)
	}
		
	getFactor = function (solver) {
		solverList = getSolverList ()
		color = which (solverList %in% solver)
		return (color)
	}

	
	addLineSegment <- function (lineSegments, x, y, xend, yend, color, linetype) {
		lineSegments = rbind (lineSegments, c (x, y, xend, yend, color, linetype))
		lineSegments[, c(1:4)] <- sapply(lineSegments[, c(1:4)], as.numeric)
		lineSegments[, c(5)] <- sapply(lineSegments[, c(5)], as.character)
		lineSegments[, c(6)] <- sapply(lineSegments[, c(6)], as.character)
		colnames(lineSegments) = c("x", "y", "xend", "yend", "color", "linetype")
		return(lineSegments)
	}

