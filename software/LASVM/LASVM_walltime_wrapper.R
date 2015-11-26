#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#	.LASVM_walltime_wrapper.R
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
 


	
	createTrainingArguments.LASVM_walltime = function (x,
		trainDataFile = "",
        modelFile = "",
        kernelCacheSize = 1024,
        finishingStep = TRUE,
		cost = 1, 
        useBias = FALSE,
        gamma = 1,
        epochs = 1,
        epsilon = 0.001, 

		saveFactor = -1,
		saveExponential = -1,
		modelPath = -1,
		wallTime = -1,
        
        extraParameter = "",
		...) 
	{
		# count training examples
		N = R.utils::countLines(trainDataFile)

		biasParameter = "-b 0"
		if (useBias == TRUE) {
			biasParameter = "-b 1"
		}
		
		finishingStepParameter = "-o 1"
		if (finishingStep == FALSE) {
			finishingStepParameter = "-o 0"
		}
		
		
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
			sprintf("-m %d", kernelCacheSize), # in MB 
			biasParameter,
			wallTimeParameter,
			sprintf("-g %.16f", gamma),
			sprintf("-c %.16f", cost), 
			sprintf("-e %.16f", epsilon),
			sprintf("-p %.16f", epochs),
			saveFactorParameter,
			saveExponentialParameter,
			finishingStepParameter,
			modelPathParameter,
			extraParameter,
			trainDataFile,
			modelFile
		)

		return (args)
	}



	createTestArguments.LASVM_walltime = function (x,
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


	
	extractTrainingInfo.LASVM_walltime = function (x, output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	#NEW
	extractTestInfo.LASVM_walltime = function (x, output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	# TODO: need to make double sure readModel.LIBSVM exists.
	readModel.LASVM_walltime = function (x, modelFile = './model', verbose = FALSE) {
		ret = readLIBSVMModel(modelFile = modelFile, verbose = verbose)
		return (ret)
	}



	# TODO: need to make double sure readModel.LIBSVM exists.
	writeModel.LASVM_walltime = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeLIBSVMModel (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
 


 	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	readPredictions.LASVM_walltime = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readLIBSVMPredictions (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}


	

	findSoftware.LASVM_walltime = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    LASVM Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^la_svm$"
		trainBinaryOutputPattern = '-A saveExponential : set exponential of time interval in seconds to save intermediate'
		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^la_test$"
		testBinaryOutputPattern = 'Usage: la_test .options. test_set_file model_file output_file'

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}


	
	print.LASVM_walltime_walltime = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
