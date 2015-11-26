	
	fOptValues = function(static, dynamic, 
		extraParameter = '',
		...) 
	{
		library(SVMBridge)

		pathForMethod = function (method = NA) {
			# stupid check
			path = method
			if (method == "CVM") 
				path = "libCVM"
			if (method == "BVM") 
				path = "libCVM"
			if (method == "BSGD") 
				path = "BudgetedSVM"
			return (path)
		}

		binaryForMethod  = function (method = NA, bin = NA) {
			# stupid check
			if ((method == "LASVM") && (bin == "test"))
				bin = "la_test"
			if ((method == "LASVM") && (bin == "train"))
				bin = "la_svm"
			if ((method == "LIBSVM") && (bin == "test"))
				bin = "svm-predict"
			if ((method == "LIBSVM") && (bin == "train"))
				bin = "svm-train"
			if ((method == "BSGD") && (bin == "test"))
				bin = "budgetedsvm-predict"
			if ((method == "BSGD") && (bin == "train"))
				bin = "budgetedsvm-train"
			if ((method == "SVMperf") && (bin == "test"))
				bin = "svm_perf_classify"
			if ((method == "SVMperf") && (bin == "train"))
				bin = "svm_perf_learn"
			if ((method == "CVM") && (bin == "test"))
				bin = "svm-predict"
			if ((method == "CVM") && (bin == "train"))
				bin = "svm-train"
			if ((method == "BVM") && (bin == "test"))
				bin = "svm-predict"
			if ((method == "BVM") && (bin == "train"))
				bin = "svm-train"
				
			return (bin)
		}

		softwarePath = "/home/xboy01/nobackup/timelimited/software/"

		# make sure we always load LIBSVM
		wrapperName = paste0 ("LIBSVM", "_wrapper.R")
		wrapperPath = file.path (softwarePath, "wrapper") 
		addSVMPackage (method = static$solver, trainBinaryPath = NULL,
			testBinaryPath = NULL,
			wrapperPath  = wrapperPath ,
			verbose = TRUE)

		wrapperName = paste0 (static$solver, "_wrapper.R")
		wrapperPath = file.path (softwarePath, "wrapper") 
		addSVMPackage (method = static$solver, trainBinaryPath = NULL,
			testBinaryPath = NULL,
			wrapperPath  = wrapperPath ,
			verbose = TRUE)


		cat("Working on ", static$train, "\n");

		model = readModelFromFile (static$modelFile)
        data = readSparseData (static$train)
        print ("A")
        optValues = optimizationValues (X = as.matrix(data$X), Y = as.matrix(data$Y), model = model, C = static$C, verbose = TRUE)
        print ("A")
        print (optValues)

		# return
		res = list (result = optValues, static = static)
		return(res)
	}
		

# 		
# library(SVMBridge)
# fOptValues(static = list(solver="LIBSVM",modelFile="./tmp/final.LIBSVM_walltime.model_16", C = 2.0, train = "../splits/mnist_1/validData.dat"), dynamic = "B")
