#!/usr/bin/Rscript 

    library(BBmisc)
    library(BatchExperiments)
    library(ParamHelpers)
    library(PMCMR) # needed for friedman tests
    library(SVMBridge)
    library(mlrMBO)

    
    source ("./TLEGO.R")
    source ("./helpers/createOrLoadRegistry.R")
    source ("./helpers/trainRows.R")
    source ("./helpers/validateRows.R")
    source("./helpers/unfactorize.R")
    source("./helpers/addRow.R")


    
### 0. parameters

    # EGO settings: grid to search
    cost = makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x)
    gamma =  makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x)
    parameter.set = makeParamSet (cost, gamma)
    

    # EGO settings
    iterations = 2
    initial.design = 20
    points.per.iteration = 10
    cpus = 10

    
    # all solver and data sets 
    datasets = c("arthrosis", "aXa", "cod-rna", "covtype", "ijcnn1", "mnist", "poker", "protein", "shuttle", "spektren", "vehicle", "wXa")
    solvers = c("BSGD", "LASVM", "LIBSVM", "CVM", "BVM", "SVMperf")
    repl = 1
    
    
    # determine wall time heuristically
    wallTime = -1

    # paths    
    results.path = "./results"
    output.path = "./output"
    suppressWarnings (dir.create (output.path, recursive = TRUE))
    suppressWarnings (dir.create (results.path, recursive = TRUE))

    # output precision
    options (digits = 2)
    
    
### 1. execute EGO experiment

    cat ("\n\n### Executing TLEGO search.\n")

    EGOcache.filename = "EGOcache.RData"
    EGOcache.file = file.path (output.path, EGOcache.filename)

    if (file.exists (EGOcache.file) == FALSE) {
        # will hold all results
        EGOResultsTable = NULL

        # execute EGO for all dataset/solver combinations
        for (d in datasets) {
            for (s in solvers) {
            registry.path = file.path (output.path, paste (s, d, repl, sep = "_"))
                local.cache.file = file.path (registry.path, "cache.data")
                suppressWarnings (dir.create (registry.path, recursive = TRUE))
                if (file.exists (local.cache.file) == FALSE) {
                    cat ("Executing TLEGO...\n")
                    result = TLEGO (dataset = d, solver = s, repl = repl, 
                        parameter.set = parameter.set, 
                        model.base.path = file.path (output.path, "models"),
                        registry.path = registry.path, 
                        wallTime = wallTime,
                        iterations = iterations,
                        initial.design = initial.design,
                        points.per.iteration = points.per.iteration,
                        cpus = cpus)
                    # we save everything here, just to be sure
                    save.image (file = local.cache.file)
                } else {
                    load (local.cache.file)
                }
                
                # create part of the master table
                result = cbind (getOptPathX(result$opt.path), getOptPathY(result$opt.path), getOptPathExecTimes(result$opt.path))
                colnames (result) = c ("cost", "gamma", "EGOerror", "EGOtime")
                result$solver = s
                result$dataset = d
                
                # add all results to mastertable
                EGOResultsTable  = addRow (EGOResultsTable , result)		
            }
        }
        
        # cache results
        save (EGOResultsTable, file = EGOcache.file)
    } else {
        cat ("Loading EGO results from cache..\n")
        load (EGOcache.file)
    }

    # really do check that the table is complete
    cat ("Checking for completeness..\n")
    for (d in datasets) {
        for (s in solvers) {
            subTable = subset (EGOResultsTable, solver == s & dataset == d)
            if (nrow(subTable) != (iterations*points.per.iteration + initial.design)*length(solvers)*length(datasets)) {
                warning (s, " on ", d, " has missing points!\n")
            }
        }
    }

    
    
### 2. find best=optimal points of the given egoResultsTable
    
    cat ("\n\n### Finding optimal points.\n")
    optimalPoints.filename = "optimalPointscache.RData"
    optimalPoints.file = file.path (output.path, optimalPoints.filename)

    # find the best point in our egoResultsTable
    optimalPointsTable = NULL
    if (file.exists (optimalPoints.file) == FALSE) {
        for (d in datasets) {
            for (s in solvers) {
                subTable = subset (EGOResultsTable, solver == s & dataset == d)
                bestPoint = subTable[subTable$EGOerror == min(subTable$EGOerror),][1,]
                optimalPointsTable = addRow (optimalPointsTable, bestPoint)
            }
        }
        
    
        # make sure for retraining we have the parameters correct -- need the 2^ operation as we do it by hand
        optimalPointsTable$cost = 2^optimalPointsTable$cost 
        optimalPointsTable$gamma = 2^optimalPointsTable$gamma
        optimalPointsTable$budget = 2048 # for BSGD
        optimalPointsTable$rank = 512 # SVMperf default
        optimalPointsTable$epochs = 1
        optimalPointsTable$saveFactor = 0
        optimalPointsTable$saveExponential = 8*60*60 # want to save the results after 8h latest. 
        optimalPointsTable$wallTime = 8*60*60 # 96h walltime..
        optimalPointsTable$repl = repl
        
        # se epsilon, TODO: make it automatic by passing -1
        optimalPointsTable$epsilon = 10^{-3}
        if ("BVM" %in% unique(optimalPointsTable$solver))
            optimalPointsTable[optimalPointsTable$solver == "BVM",]$epsilon = 10^{-6}
        if ("CVM" %in% unique(optimalPointsTable$solver))
            optimalPointsTable[optimalPointsTable$solver == "CVM",]$epsilon = 10^{-6}
        if ("SVMperf" %in% unique(optimalPointsTable$solver))
            optimalPointsTable[optimalPointsTable$solver == "SVMperf",]$epsilon = 10^{-1}
        
        save (optimalPointsTable, file = optimalPoints.file)
    } else {
        cat ("Loading optimal points results from cache..\n")
        load (optimalPoints.file)
    }
    
    # really do check that the table is complete
    for (d in datasets) {
        for (s in solvers) {
            subTable = subset (optimalPointsTable, solver == s & dataset == d)
            if (nrow(subTable) != 1) {
                stop (s, " on ", d, " has no optimal point!\n")
            }
        }
    }
        
        
    
### 3. retrain all the best points, now without any walltime
    
    cat ("\n\n### Retraining on best points.\n")
    experiment = "retraining"

    retrainingPoints.filename = "retrainingcache.RData"
    retrainingPoints.file = file.path (output.path, retrainingPoints.filename )

    if (file.exists (retrainingPoints.file ) == FALSE) {
        
        registryPath = file.path (output.path, "retrainingRegistry")
        registry = createOrLoadRegistry (registryPath, experiment = experiment)

        # compute
        registry = trainRows (registry, mtable = optimalPointsTable)
        submitJobs(registry, findNotStarted(registry), resources = list(walltime = 80 * 3600, memory = 4 * 1024))
        waitForJobs (registry)

        # reduce it
        curResults = reduceResultsExperiments(registry, fun = function(job, res) 
                {  
                    mRow = res$static$mRow
                    names(mRow) = replace(names(mRow), names(mRow) == "error", "EGOerror")
                    names(mRow) = replace(names(mRow), names(mRow) == "time", "EGOtime")
                    mRow[,"repl"] = NULL

                    c( static = mRow,
                        finalTrainTime = res$result$trainTime,
                        modelPath = res$modelPath, 
                        modelFilename = res$modelFilename);                        
                }  )
                
        save.image (file = retrainingPoints.file )

        # we have the table now we need but the names have a static prefix
        names(curResults) = lapply(names(curResults), FUN=function(x) {y = sub("static.", "" ,x); return(y) })
        # there is no new error when just training
        #names(curResults) = replace(names(curResults), names(curResults) == "error", "validationError")
        names(curResults) = replace(names(curResults), names(curResults) == "time", "trainTime")
        finalResultsTable = curResults
        
        save.image (file = retrainingPoints.file )
    } else {
        cat ("Loading retraining results from cache..\n")
        load (file = retrainingPoints.file )
    }

    
    
### 4. validate the models we have

    # create a temporary registry 
    cat ("\n\n### Validating final models on test data\n\n")
    experiment = "finalTestResults"
    
    finalTest.filename = "finalTestcache.RData"
    finalTest.file = file.path (output.path, finalTest.filename )
    
    if (file.exists (finalTest.file ) == FALSE) {
        
        print (head(finalResultsTable))
        registryPath = file.path (output.path, "finalTestRegistry")
        registry = createOrLoadRegistry (registryPath, experiment = experiment)

        # compute
        registry = validateRows (registry, mtable = finalResultsTable, useTestData = TRUE, takeSnapshotModel = FALSE, verbose = TRUE)
        submitJobs(registry, findNotStarted(registry), resources = list(walltime = 80 * 3600, memory = 4 * 1024))
        waitForJobs (registry)

        # reduce it
        curResults = reduceResultsExperiments(registry, fun = function(job, res) 
            {   #
                mRow = res$static$mRow
                    names(mRow) = replace(names(mRow), names(mRow) == "error", "EGOerror")
                    names(mRow) = replace(names(mRow), names(mRow) == "time", "EGOtime")

                c( static = mRow,
                    testError = res$result$testError, 
                    testTime = res$result$testTime);    
            }  )
            
        save.image (file = finalTest.file)
    } else {
        cat ("Loading retrain validation results from cache..\n")
        load (file = finalTest.file)
    }

    # do some renaming
    names(curResults) = lapply(names(curResults), FUN=function(x) {y = sub("static.", "" ,x); return(y) })
    finalTestTable = curResults

    # as we had NAs in our runs because of bugs, we remove all those stupid ones, just to be sure
    finalTestTable = finalTestTable[which(!is.na(finalTestTable$testError)),]
    
    # make sure the modelfilename is just the filename
    finalTestTable$modelFilename = basename(as.character(finalTestTable$modelFilename))
    
    # drop double things
    drops <- c("algo", "prob", "id", "repl.1")
    finalTestTable  = finalTestTable [,!(names(finalTestTable ) %in% drops)]

    save.image (file = finalTest.file)

    
    
### 5.	create data frame with all relevant results

    cat ("\n\n### Results: Generating accuracy table.\n")
    load("parego/allResults.RData")

    # generate the results table for all datasets:
    # and compute simple statistics, which one is usuable with the two goals
    # a) is TL harming model selection?
    # b) which of the solvers are 'compatible' with TL?

    # repeat this for all data sets

    #generateErrorTable = function (subsampling) {


    overallAccuracy = list()
    for (d in datasets) {
        resultsTable = NULL
        rawResultsTable = NULL
        for (s in solvers) {
            # get results from parego test
            errorSolverBest = min (subset(results, solver == s & dataset == d )$error)
            errorOverallBest = min (subset(results, dataset == d )$error)
            
            # compure some statistics
            errorTLfinal = subset(finalTestTable, solver == s & dataset == d )$testError
            errorTLDiff = (errorOverallBest - errorTLfinal)
            errorTLSolverDiff = (errorSolverBest - errorTLfinal)
            
            # convert to strings
            errorSolverBestStr = paste0 (round (errorSolverBest*100, 1) )
            errorOverallBestStr = paste0 (round (errorOverallBest *100, 1) )
            errorTLfinalStr = paste0 (round (errorTLfinal *100, 1) )
            errorTLDiffStr = paste0 (round (errorTLDiff *100, 1) )
            errorTLSolverDiffStr = paste0 (round (errorTLSolverDiff *100, 1) )

            # combine everything to a table
            rawRow = c(s, errorSolverBest, errorOverallBest, errorTLfinal, errorTLDiff, errorTLSolverDiff)
            cRow = c(s, errorSolverBestStr, errorOverallBestStr, errorTLfinalStr, errorTLDiffStr, errorTLSolverDiffStr)
            resultsTable  = addRow (resultsTable , cRow)		
            rawResultsTable = addRow (rawResultsTable, rawRow)
        }   
    
        resultsTable = t(resultsTable)
        colnames (resultsTable) = resultsTable[1,]
        resultsTable = resultsTable[-1,]
        
        overallAccuracy [[d]] = rawResultsTable
    }

    
    
### 6. statistical tests

    cat ("### Statistical Tests.\n")

    # generate frame for friedman test
    fM = matrix(0, nrow = length(unique(finalTestTable$dataset)), ncol = length(unique(finalTestTable$solver)), byrow = TRUE)

    print (finalTestTable)
    datasets = unfactorize(as.character(unique(finalTestTable$dataset)))
    solvers = unfactorize(as.character(unique(finalTestTable$solver)))
    for (si in 1:length(solvers)) {
        fM[,si] = subset (finalTestTable, solver == solvers[si])$testError
    }
    colnames(fM) = solvers
    rownames(fM) = datasets

    # create control classifier ParEGO
    paregoCol = matrix(0, nrow = length(datasets), ncol = 1, byrow = TRUE)
    for (sd in 1:length(datasets)) {
        paregoCol[sd] = min (subset(results, dataset == datasets[sd] )$error)
    }

    fM = cbind (fM, paregoCol)
    colnames(fM)[length(colnames(fM))] = "ParEGO"
    esolvers = c (solvers, "ParEGO")
    
    cat ("Friedman Test:\n")
    w = friedman.test(fM)
    pValue = w$p.value
    cat  ("  pValue for Friemdann Test:" ,pValue, "\n")
    
    # demsar works with accuray
    X = 1-fM

    # compute ranks
    N = nrow(X)
    k = ncol (X)
    rankM = t(apply(X, 1, FUN = function(x){return (k-rank(x)+1)}))
    ranks = 1/N*colSums (rankM)  # sum of ranks for each algorithm
    cat ("Ranks of algorithms:\n")
    print (ranks)    
    
    # compute standard error
    SE = sqrt(k * (k+1)/(6*N))
    cat ("  Standard error:", SE, "\n")

    # now we check like on page 14.

    # first all algorithms worse than parego.
    # this is the group of bad ones
    ranks = sort(ranks)

    # if we use holms/hommel/hochberg tests, we get the same bad group
    # this is probably not correct written down (should be ok nonetheless)
    cat("Holms test\n")
    pL = c()
    for (i in 1:(length(ranks))) {
        z = ranks[i]- ranks[length(ranks)]
        z = z/SE
        p = 2*pnorm(-abs(z))
        pL = c(pL,p)
    }
    goodGrpH = names(pL[p.adjust(pL, method = "holm")<=0.05])
    badGrpH = names(pL[p.adjust(pL, method = "holm")>0.05])
    cat("  Good/Unconclusive performing group:", goodGrpH, "\n")
    cat("  Bad performing group:", badGrpH, "\n")


    
### 7.  results: timing tables

    cat ("\n\n### Results: Generating timing table.\n")

    # load data from ParEGO experiment
    load("parego/allResults.RData")

    overallTimingTable = list()
    for (d in datasets) {
        timingTable = NULL
        rawTimingTable = NULL
        for (s in solvers) {
            # first from parego
            time  = sum (subset(results, solver == s & dataset == d & subsampling == FALSE)$execTime) 

            timeStr = paste0 (round(time), "s (1x)")
            cRow = c(s, timeStr)#, errorStr)
            timingTable = addRow (timingTable, cRow)		
            
            ## TL-variant
            modelSelectionTime = sum (subset(EGOResultsTable, solver == s & dataset == d )$EGOtime) 
            finalModelTrainingTime =  sum (subset(finalTestTable, solver == s & dataset == d )$trainTime)

            timeTL = modelSelectionTime + finalModelTrainingTime 
            egovsfinalTimeFactor = modelSelectionTime/finalModelTrainingTime 
            
            factor = time/timeTL
            factorStr = paste0 ("(",round(factor,1), "x)")
            timeTLStr = paste0 (round(timeTL), "s ", factorStr)

            rawRow = c(s, timeTL, factor, egovsfinalTimeFactor, modelSelectionTime, finalModelTrainingTime)
            rawTimingTable = addRow (rawTimingTable , rawRow)

            s = paste0("TL-", as.character(s))
            cRow = c(s, timeTLStr) #, errorTLStr)
            timingTable = addRow (timingTable, cRow)		
        }
    
        colnames (timingTable) = c("solver", "overall time")#, "test error")
        overallTimingTable [[d]] = rawTimingTable 
    }

    allResults.filename = "allResults.RData"
    allResults.file = file.path (output.path, allResults.filename )
    save.image (file = allResults.file )

    
    
### 8. hip colorful plots

    cat ("\n\n### Results: Generating plots.\n")
    
    # rainbow colors
    myPalette  <- colorRampPalette(c(
        "#00d000", 
        "#d0d000",
        "#d00000", 
        "#d000d0",
        "#0000f0"))


    # subfunction to create accuracy plot
    createAccuracyPlot = function (datasets, outputFile, legend = FALSE, mar = NULL, legendX = -0.3, legendY = 0.47) {
        bp = list()
        q = 1
        for (d in datasets) {
            bp[[q]] = as.numeric(overallAccuracy[[d]][,4])
            q = q + 1
        }

        bp = as.data.frame(bp)
        colnames(bp) = datasets

        pdf (file.path(results.path, outputFile))
        
        # generate barplot
        if (is.null (mar) == TRUE) {
        par(mar=c(2.2, 5, 0.1, 0)) # bottom, left, top, right
        } else {
            par (mar = mar)
        }
        
        barplot (as.matrix(as.data.frame(bp)), main="", ylab="Absolute Error", 
            beside = TRUE, 
            col = myPalette(length(solvers)), 
            cex.names = 1.87, 
            cex.axis = 2.0,
            cex.lab = 2.0)
            
        # generate baselines
        for (d in datasets) {
            finalLine = c()
            for (cd in datasets) {
                if (cd == d) {
                    # create line
                    errorOverallBest = min (subset(results, dataset == d )$error)
                    for (ys in 1:(length(solvers)+1)) {
                        finalLine = c(finalLine, errorOverallBest)
                    }
                    finalLine = c(finalLine, NA)
                } else { 
                    # empty line
                    for (ys in 1:(length(solvers)+1)) {
                        finalLine = c(finalLine, NA)
                    }
                }
            }
            lines(finalLine, lwd = 4)
        }

        if (legend == TRUE) {
            legend(legendX, legendY, solvers, cex=1.9, fill=myPalette(length(solvers)), bty = "n")
        }
        dev.off()
    }
    

    
    # subfunction to create timing plot
    createTimingPlot = function (datasets, outputFile) {
        bpT = list()
        q = 1
        for (d in datasets) {
            bpT[[q]] = log10 (as.numeric(overallTimingTable[[d]][,3]))
            q = q + 1
        }

        bpT = as.data.frame(bpT)
        colnames(bpT) = datasets

        pdf (file.path(results.path, outputFile))
        
        # generate barplot
        par(mar=c(2.2, 5, 0.1, 0)) # bottom, left, top, right
        barplot (as.matrix(as.data.frame(bpT)), main="", ylab="Factor (log_10)", 
            beside = TRUE, 
            col = myPalette(length(solvers)), 
            cex.names = 1.875, 
            cex.axis = 2.0,
            cex.lab = 2.0)
        dev.off()
    }


    
    
    # generate all three plots, but first one=one used in the paper
    datasets = c("aXa", "cod-rna", "mnist", "poker")
    createTimingPlot (datasets, "plot_timing.pdf")

    datasets = c("arthrosis", "covtype", "spektren", "wXa")
    createTimingPlot (datasets, "plot_timing_2.pdf")

    datasets = c("protein", "ijcnn1", "shuttle", "vehicle")
    createTimingPlot (datasets, "plot_timing_3.pdf")

    
    
    # generate all three plots, but first one=one used in the paper
    datasets = c("aXa", "cod-rna", "mnist", "poker")
    createAccuracyPlot (datasets, "plot_accuracy.pdf", legend = TRUE)

    datasets = c("arthrosis", "covtype", "spektren", "wXa")
    createAccuracyPlot (datasets, "plot_accuracy_2.pdf", legend = TRUE, legendX = 17.5)

    datasets = c("protein", "ijcnn1", "shuttle", "vehicle")
    createAccuracyPlot (datasets, "plot_accuracy_3.pdf", legend = TRUE, legendX =10.0)

        
