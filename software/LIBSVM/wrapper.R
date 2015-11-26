#!/usr/bin/Rscript  --vanilla 

source ("software/helpers/universalWrapper.R")

library(BBmisc)




evalLIBSVM = function(...)  {   
    obj = universalWrapper (
        modelName = "LIBSVM",
        trainingParameterCallBack = LIBSVMTrainingParameterCallBack,
        testParameterCallBack = LIBSVMTestParameterCallBack,
        extractInformationCallBack  = LIBSVMExtractInformationCallBack,
        trainBinary = LIBSVMTrainBinary(),
        testBinary = LIBSVMTestBinary (),
        bindir = LIBSVMBinDir(),
        ...
    )
    return (obj)
}



LIBSVMTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            saveFactor = -1,
                                            saveExponential = -1,
                                            extraParameter = "",
                                            wallTime = 8*60,
                                            kernelCacheSize = 1024,
                                            cost = 1, 
                                            gamma = 1, 
                                            epsilon = 0.001, 
                                            modelPath = -1,
                                             ...) {

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
        "-s 0",                         # c classification
        "-t 2",
        sprintf("-m %d", kernelCacheSize), # in MB 
        wallTimeParameter,
        sprintf("-c %.16f", cost),         # rbf kernel
        sprintf("-g %.16f", gamma),        # gamma
        sprintf("-e %.16f", epsilon),      # epsilon tolerance
        saveFactorParameter,
        saveExponentialParameter,
        modelPathParameter,
        extraParameter,
        trainfile,
        modelFile
    )
    print(args)
    return (args)
}



LIBSVMTestParameterCallBack = function (testfile = "",
                                        modelFile = "", ...) {
    args = c(
        testfile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



LIBSVMExtractInformationCallBack = function (output) {

    # compute error
    pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100

    return(err)
}



LIBSVMTrainBinary <- function() {
    return ("svm-train")
}


LIBSVMTestBinary <- function() {
    return ("svm-predict")
}


LIBSVMBinDir <- function() {
    return ("software/LIBSVM/bin/")
}



# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
    testFile = getData ("spektren")
    trainFile = getData ("spektren")
  
    err = evalLIBSVM(trainFile, testFile, 
        cost = 444.355010,
        gamma = 0.232882, 
        epsilon = 0.001,
        wallTime = 60,
#        subsamplingRate = 0.17,
		kernelCacheSize = 6000,
        verbose = TRUE,
        modelFile = "/tmp/libsvm.model",
        computePrimal = FALSE,
        executeTest = FALSE
    )  



  
    err = evalLIBSVM(trainFile, testFile, cost = 1, gamma = 1, epsilon = 0.01, 
        wallTime = -1,
#        subsamplingRate = 0.17,
        verbose = TRUE,
        modelFile = "/tmp/libsvm.model",
        computePrimal = FALSE,
        executeTest = FALSE
    )  

    messagef("Error: %s", err$err) 
    messagef("Dual: %s", err$dual) 
    messagef("Primal: %s", err$primal)
}
