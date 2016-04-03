#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#	.CVM_walltime_wrapper.R
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
 


	
	createTrainingArguments.CVM_walltime = function (x,
		trainDataFile = "",
        modelFile = "",
        kernelCacheSize = 1024,
		cost = 1, 
        gamma = 1,
        epsilon = 0.001, 

		saveFactor = -1,
		saveExponential = -1,
		modelPath = -1,
		wallTime = -1,
        
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
			"-s 6",                         # CVM = 6, BVM = 9
			"-t 2",
			sprintf("-m %d", kernelCacheSize), # in MB 
			sprintf("-g %.16f", gamma),
			sprintf("-c %.16f", cost), 
			sprintf("-e %.16f", epsilon),
			wallTimeParameter,
			saveFactorParameter,
			saveExponentialParameter,
			modelPathParameter,
			extraParameter,
			trainDataFile,
			modelFile
		)

		return (args)
	}



	createTestArguments.CVM_walltime = function (x,
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


	
	extractTrainingInfo.CVM_walltime = function (x, output) {
		pattern <- "Accuracy = (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	#NEW
	extractTestInfo.CVM_walltime = function (x, output) {
		pattern <- "Accuracy = (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	readModel.CVM_walltime = function (x, modelFile = './model', verbose = FALSE) {
		ret = readLIBSVMModel (modelFile = modelFile, verbose = verbose)
		return (ret)
	}



	writeModel.CVM_walltime = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
 


 	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	readPredictions.CVM_walltime = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}


	

	findSoftware.CVM_walltime = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    CVM_walltime Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^svm-train$"
		trainBinaryOutputPattern = c(
			'-A saveExponential : set exponential of time interval in seconds to save intermediate',
			'CVM.BVM, default eps')
			
		binaryPath = SVMBridge:::findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)


		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^svm-predict$"
		testBinaryOutputPattern = 'Usage: bvm-predict .options. test_file model_file output_file'

		binaryPath = SVMBridge:::findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}


	
	print.CVM_walltime = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
