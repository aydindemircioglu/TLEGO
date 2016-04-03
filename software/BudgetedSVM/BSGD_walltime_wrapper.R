#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		BSGD_walltime_wrapper.R
# 
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#
 
	createTrainingArguments.BSGD_walltime = function (x,
		trainDataFile = "",
        modelFile = "",
                                            
		kernelCacheSize = 1024,
		cost = -1,
		gamma = -1,
		epochs = 1,
		budget = 128,
        
		# walltime specific
		wallTime = -1,
		modelPath = -1,
		saveFactor = -1,
		saveExponential = -1,

		extraParameter = "",
        ...) 
	{

		#argg <- c(as.list(environment()), list(...))
		#print (argg)
		
		n = R.utils::countLines(trainDataFile)

			
		# ---  take care of primal/wall time, will not be added if its turned off. 
		if (wallTime == -1) {
			wallTimeParameter = ""
		} else {
			wallTimeParameter =  sprintf("-l %d", floor(wallTime))
		}
		
		if (modelPath == -1) {
			modelPathParameter = ""
		} else {
			modelPathParameter =  paste("-x", modelPath)
		}
		
		if (saveFactor == -1) {
			saveFactorParameter = ""
		} else {
			saveFactorParameter = sprintf("-f %.3f", saveFactor)
		}

		if (saveExponential == -1) {
			saveExponentialParameter = ""
		} else {
			saveExponentialParameter = sprintf("-a %.3f", saveExponential)
		}


    # arguments for training
    args = c(
        "-A 4",
        "-r 0",
        wallTimeParameter,
        sprintf("-B %.16f", budget ),
        sprintf("-L %.16f", (1.0 / (n * cost))),
        sprintf("-e %.16f", epochs ),
        sprintf("-g %.16f", 2 * gamma),
        saveFactorParameter,
        saveExponentialParameter,
        modelPathParameter,
        extraParameter,
        trainDataFile,
        modelFile
    )

    return (args)
}
	
	
	createTestArguments.BSGD_walltime = function (x,
     	testDataFile = "",
		modelFile = "", 
		predictionsFile = "",
		...) 
	{
		args = c(
			"-v 1",
			testDataFile,
			modelFile,
			predictionsFile
		)
    
		return (args)
	}


	
	extractTrainingInfo.BSGD_walltime = function (x, output) {
		pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
		error = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	
	extractTestInfo.BSGD_walltime = function (x, output) {
		pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
		error = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	readSparseFormatBSGD <- function (con, submodel)
	{
		# these will contain the coefficients and the  svs.
		supportvectors <- matrix()
		coefficients <- matrix()
		weights <- matrix()
		
		# read file one by one
		currentIndex = 0
		while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
			
			# remove comment if necesary
			oneLine = stringr::str_split_fixed(oneLine, pattern = '#', n = 2)[1]
			
			# split line by " "
			svec = vector(length = 1)
			parts = strsplit (oneLine, " ")
			
			# where the support vector data starts in the row
			fvpos = 1
			coeff = vector(length = 1)
			w = vector (length = 1)

			# read part for part until it is something positive
			for (i in seq(1, length(parts[[1]]))) {
				fparts <- strsplit (parts[[1]][i], ":")
				if (!is.na(fparts[[1]][1])) {
					ind = as.numeric(fparts[[1]][1])
					value = as.numeric(fparts[[1]][2])
					
					# check if we have part of some feature vector
					if (ind > 0) {
					# yes, so quit the whole loop
					fvpos = i
					break
					}
					
					# if not, we can save it in the coeff
					coeff[-ind] = value
				}
				else {
					stop ("Should never happen. Really.")
				}
			}
			
			# grep feature vectors one by one
			for (i in fvpos:length(parts[[1]])) {
				# split by :
				fparts <- strsplit (parts[[1]][i], ":")
				
				# if we have anything, add it to our vector
				if (!is.na(fparts[[1]][1])) {
					ind = as.numeric(fparts[[1]][1])
					value = as.numeric(fparts[[1]][2])
					svec[ind] <- value
				}
			}
			
			# make sure our vector has no NAs
			#print (svec)
			svec[is.na(svec)] <- 0
			
			# stack matrices
			supportvectors <- plyr::rbind.fill.matrix(supportvectors, t(svec))
			coefficients <- plyr::rbind.fill.matrix(coefficients, t(coeff))
			weights <- plyr::rbind.fill.matrix(weights, t(w))
		} 
		
		# crop first NA list (why does it even exist?..)
		supportvectors = supportvectors[-1, ]
		coefficients = coefficients[-1, ]
		weights = weights[-1, ]
		
		# remove possible NA values that occur if the very last
		# entry of a sparse vector is omitted because of sparsity
		supportvectors[is.na(supportvectors)] <- 0
		coefficients[is.na(coefficients)] <- 0 
		weights[is.na(weights)] <- 0 

		return (list("X" = supportvectors, "a" = coefficients, "w" = weights))
	}


	
	
	readModel.BSGD_walltime = function (x, modelFile = './model', verbose = FALSE) {
		# open connection
		con  <- file(modelFile, open = "r")
		
		invertLabels = FALSE
		while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
			if (grepl("MODEL", oneLine) == TRUE) 
				break;
		
			# gamma value
			if (grepl("KERNEL_GAMMA_PARAM", oneLine) == TRUE) 
			{
				pattern <- "KERNEL_GAMMA_PARAM: (.*)"
				gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}  
		
			# bias
			if (grepl("BIAS_TERM", oneLine) == TRUE) 
			{
				pattern <- "BIAS_TERM: (.*)"
				bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}
		
			# order of labels
			if (grepl("LABELS", oneLine) == TRUE) 
			{
				pattern <- "LABELS: (.*)"
				order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			
				if ((order != "1 -1") && (order != "-1 1")) {
					stop ("Label ordering %s is unknown!", order)
				}
			
				if (order == "1 -1") {
					invertLabels = FALSE
				}

				if (order == "-1 1") {
					invertLabels = TRUE
				}
			}  
		}
		if (verbose == TRUE)  
			cat(" Finished reading header.")

	
		# read and interprete data 
		# basically all data is sparse data format, but the data around this differs
		svmatrix = readSparseFormatBSGD(con)

	
		# add header information
		svmatrix$gamma = gamma
		svmatrix$bias = bias
		svmatrix$modelname = "BSGD"
		
		
		# do we need to invert the labels? in this case we invert the coefficients
		if (invertLabels == TRUE) {
			if (verbose == TRUE)  
				cat(" Inverting Labels.")

			# invert alphas
			svmatrix$a = -svmatrix$a

			# this is also needed.. 
			svmatrix$bias = -bias
		}

		# close connection
		close(con)

		# return
		return (svmatrix)		
	}


	
	writeModel.BSGD_walltime = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeModel.BSGD (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
 


 	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	readPredictions.BSGD_walltime = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}

	
	
	findSoftware.BSGD_walltime = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    BSGD Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^budgetedsvm-train$"
		trainBinaryOutputPattern = c('saveExponential : set exponential', 'budget maintenance in BSGD')

		binaryPath = SVMBridge:::findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^budgetedsvm-predict$"
		testBinaryOutputPattern = 'training and test file are loaded in chunks so'

		binaryPath = SVMBridge:::findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}

	
	
	print.BSGD_walltime = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
	
	
