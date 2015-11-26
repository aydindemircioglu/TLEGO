    
TLEGO = function (solver, dataset, wallTime, registry.path,
					repl = 1,
					software.path = "./software", 
                    data.splits.path = "./splits",     
                    model.base.path = "./results/models", 
                    parameter.set = NULL,
					clearRegistry = TRUE,
					iterations = 10,
					initial.design = 20,
					points.per.iteration = 20,
					cpus = 2,
                    verbose = FALSE, 
                    ...) 
{    
	if (is.null (parameter.set) == TRUE)
		stop ("Parameter set must be provided.")

	if (verbose == TRUE) {
        cat ("Searching on ", dataset, " with solver ", solver, " and walltime ", wallTime, "\n")
        cat ("Saving registry below ", registry.path, "\n")
    }

    
    library(mlrMBO)
    library(parallelMap)
	library(ParamHelpers)


	mode = "multicore"
    clusterWalltime = 1 * 60 * 60

    
    ## function to be called
    makeTargetFunction = function(solver, dataset, repl, train.file, valid.file, parameter.set, 
		model.base.path = NULL,
		saveFactor = 0.0,
		saveExponential = -1,
		wallTime = wallTime, 
		verbose = TRUE) 
	{		
        f = function(x) {
            library (SVMBridge)

            # if we do not specify this, we turn OFF saving intermediate models 
            if (saveExponential == -1) {
                saveExponential = wallTime * 2 + 10
            }
            
            if (verbose == TRUE) {
                cat ("Working on ", train.file, " with walltime ", wallTime, " and validation/test file ", valid.file, "\n")
            }
            
            # helpers
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

            wSolver = paste(solver, "walltime", sep = "_")
            wrapperName = paste0 (wSolver, "_wrapper.R")
            wrapperPath = file.path (software.path , pathForMethod(solver), wrapperName) 
            if (verbose == TRUE) {
                cat ("Wrapper path is ", wrapperPath, "\n")
            }

            testBinaryPath = file.path(software.path , pathForMethod (solver), "bin", binaryForMethod (solver, bin = "test"))
            trainBinaryPath = file.path (software.path , pathForMethod (solver), "bin", binaryForMethod (solver, bin = "train"))
                            
            addSVMPackage (method = wSolver, trainBinaryPath = trainBinaryPath,
                testBinaryPath = testBinaryPath,
                wrapperPath  = wrapperPath ,
                verbose = TRUE)
 
            outputAllSVMSoftwarePackages()
            if (verbose == TRUE) {
                cat ("SVM Instance information: \n")
                print (getSVMInstance (wSolver))
            }

            ## MODELS:
            # create filename. we put those still under ./dataset/solver/ to avoid too much clutter
            filename = paste (digest::digest (x, algo="sha256"), solver, "model", sep = ".")
            model.path = file.path (model.base.path,  solver, paste(dataset, "_", repl, sep = ""))
            dir.create (model.path, recursive = TRUE)
            model.file = paste(model.path, "/", filename, sep = "")

            # set further parameter. TODO: make this better.
            budget = 2048
            k = 512
            epochs = 1
            
            if (solver == "BSGD")
                x = append(x, list (epochs = epochs, budget = budget))
            if (solver == "LASVM")
                x = append(x, list (epochs = epochs, finishingStep = FALSE))
            if (solver == "BSGD")
                x = append(x, list (k = k))

			# for our experiment we skip epsilon-- it should be left at its default
			#x = append (x, list(epsilon = epsilon))

            # train the SVM
            trainObj =  do.call( trainSVM, 
                c(list (
                    method = wSolver,
                    timeOut = 2 * wallTime, # hard timeout
                    trainDataFile = train.file,
                    kernelCacheSize = 1024,
                    saveFactor = saveFactor,
                    saveExponential = saveExponential,
                    wallTime = wallTime,
                    modelFile = model.file,
                    extraParameter = "",
                    verbose = TRUE),
                x)
            )

            # must test to know where to go
            testObj = testSVM (method = wSolver,
                    testDataFile = valid.file,
                    modelFile = model.file,
                    extraParameter = "",
                    verbose = TRUE
                    #...
            )

            # extra infos -- only scalars for now :(
            org.extras = list (
                    resultTrain = trainObj,
                    resultTest = testObj,
                    paramStr = x,
                    wallTime = wallTime,
                    modelFilename = filename, 
                    modelPath = model.base.path)
            extras = list (
                    train.time = trainObj$trainTime,
                    test.time = testObj$testTime
            )
            
            # return
            retvalue = c(testObj$testError)
            attributes(retvalue) = list (extras = extras)

            # save return object?
            saveReturnObject = TRUE
            if (saveReturnObject == TRUE) {
                retObj.filename = paste (digest::digest (x, algo="sha256"), solver, "retObj", sep = ".")
                retObj.file = paste(model.path, "/", retObj.filename, sep = "")
                save (retvalue, file = retObj.file)
            }
            
            return(retvalue)
        }
        return(f)
    }

    
    ## determine data paths
    train.file = file.path (data.splits.path, paste (dataset, "_", repl, "/trainData.dat", sep = ""))
    valid.file = file.path (data.splits.path, paste (dataset, "_", repl, "/validData.dat", sep = ""))

    
    ## determine walltime if necessary
    if (wallTime == -1) {
        nLines = R.utils::countLines (train.file)[1]
        wallTime = floor(2^{log10(nLines)})  
        cat ("INFO: Setting walltime to ", wallTime, " seconds.")
    }

    
    ## create function
    f = makeTargetFunction(solver = solver, dataset = dataset, repl = repl, 
		train.file = train.file, 
		valid.file = valid.file, 
		parameter.set = parameter.set,
		wallTime = wallTime, 
		model.base.path = model.base.path,
		verbose = TRUE)
	
	
	## just make sure the storage exists, but delete contents
    if (clearRegistry == TRUE) {
		unlink(registry.path, recursive = TRUE)
		if (!file.exists (registry.path))
            dir.create(registry.path, recursive = TRUE)
	}
	
	# imputation function: will fail for now, good so
    imputeFun = function(x, y, opt.path) {
		cat ("Imputing called, this means something is broken, as walltime should be applied and walltime < clustertime (please check).")
        return (0.6) 
    }


    ## cache/results
    cache.filename = "cache.RData"
    cache.file = file.path (registry.path, cache.filename)
    
    
	## create control object
    learner = makeLearner("regr.km", 
        predict.type = "se")
    				
	control = makeMBOControl(number.of.targets = 1L, 
        on.learner.error = "stop",
        show.learner.output = TRUE,
        iters = iterations,
        propose.points = points.per.iteration, 
        init.design.points = initial.design, 
        y.name = c("error"),
        save.on.disk.at = 1:iterations + 1, 
        save.file.path = cache.file,
        impute = imputeFun, 
        store.model.at = integer(0))
        
    control = setMBOControlInfill(control, 
        crit = "ei", 
        crit.lcb.lambda = 0.1,
        opt = "focussearch",
        opt.restarts = 2L, 
        opt.focussearch.maxit = 3L, 
        opt.focussearch.points = 5000L,
        filter.proposed.points.tol = 0.1,
        filter.proposed.points = TRUE)
    
    control = setMBOControlMultiPoint (control, 
        method = "cl")
	
	
    ## start the job 
    parallelStart(mode = mode, cpus = cpus, logging = TRUE, storagedir = registry.path)
    resMBO = mbo(f, parameter.set, learner = learner, control = control)        
    print ("######################################################################################################")
    print (resMBO)
    print ("######################################################################################################")
    parallelStop()
    
    # return, but actully its more like dummy.
    return (resMBO)
}

