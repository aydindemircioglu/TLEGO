#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		LIBSVM_walltime_wrapper.R
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
 
	
	
	createTrainingArguments.LIBSVM_walltime = function (x,
		trainDataFile = "",
        modelFile = "",

        kernelCacheSize = 1024,
		cost = -1,
		useBias = FALSE,
        gamma = -1,
        epsilon = 0.001, 
        degree = -1,
        coef0 = -1,
        nu = -1,
        shrinking = -1,
        probabilityEstimates = -1,
        weight = -1,
        n = -1,
        quietMode = FALSE,

		# walltime specific
		wallTime = -1,
		modelPath = -1,
		saveFactor = -1,
		saveExponential = -1,

		extraParameter = "",
        ...) 
	{
		
		# ---  take care of primal/wall time, will not be added if its turned off. 
		if (wallTime == -1) {
			wallTimeParameter = ""
		} else {
			wallTimeParameter =  sprintf("-L %d", floor(wallTime))
		}

		if (modelPath == -1) {
			modelPathParameter = ""
		} else {
			modelPathParameter =  paste("-X", modelPath)
		}

		if (saveFactor == -1) {
			saveFactorParameter = ""
		} else {
			saveFactorParameter = sprintf("-F %.3f", saveFactor)
		}

		if (saveExponential == -1) {
			saveExponentialParameter = ""
		} else {
			saveExponentialParameter = sprintf("-A %.3f", saveExponential)
		}
		
		
		args = c(
			"-t 2",
			sprintf("-m %d", kernelCacheSize), # in MB 
			sprintf("-c %.16f", cost), 
			sprintf("-g %.16f", gamma),        # gamma
			sprintf("-e %.16f", epsilon),      # epsilon tolerance
			wallTimeParameter,
			modelPathParameter,
			saveFactorParameter,
			saveExponentialParameter,
			extraParameter,
        	trainDataFile,
			modelFile
		)

		return (args)
	}

	
	
	createTestArguments.LIBSVM_walltime = function (x,
		testDataFile = "",
		modelFile = "", 
		predictionsFile = "",
		...) 
	{
		args = c(
			testDataFile,
			modelFile,
			predictionsFile
		)
    
		return (args)
	}


	
	extractTrainingInfo.LIBSVM_walltime = function (x, output) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	
	extractTestInfo.LIBSVM_walltime = function (x, output) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	
	readModel.LIBSVM_walltime = function (x, modelFile = './model', verbose = FALSE) {
		ret = readLIBSVMModel (modelFile = modelFile, verbose = verbose)
		return (ret)
	}



	writeModel.LIBSVM_walltime = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeLIBSVMModel (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
 


 	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	readPredictions.LIBSVM_walltime = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}

	
	
	findSoftware.LIBSVM_walltime = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    LIBSVM Object: Executing search for software for %s", x$method)
		}
		
		# i am not that sure 
		trainBinaryPattern = "^svm-train$"
		trainBinaryOutputPattern = c('saveExponential : set exponential',
			'.q : quiet mode .no outputs')

		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^svm-predict$"
		testBinaryOutputPattern = 'for one-class SVM only 0 is supported'

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}

	
	
	print.LIBSVM_walltime = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
	
	
