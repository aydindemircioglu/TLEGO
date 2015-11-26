
library(BatchExperiments)


replications= 10
ratio1 = 1/2
ratio2 = 1/4
datasets = c("aXa", "cod-rna", "covtype", "ijcnn1", "mnist", "poker", 
                "protein", "shuttle", "vehicle", "wXa")

# path of datasets (from https://bitbucket.org/aydin_demircioglu/datasets)
base.path = "./datasets"
splits.path = "./splits"

               
r = makeExperimentRegistry("makeSplits", file.dir = as.character(sample(10000, 1)), seed = 1273)

addAlgorithm(r, "makeSplits",
    function(static, dynamic, repl, ratio1, ratio2, splits.path) {
        source("./helpers/makeTrainTestSplit.R")

        dfile = paste("datasets/", static, "/", static, ".combined.scaled", sep = "")    
        outdir = file.path (splits.path, paste(static, repl, sep = "_"))
        
        makeTrainTestSplit(dfile, outdir, ratio1, ratio2)
    }
)
design = makeDesign("makeSplits", exhaustive = list(ratio1 = ratio1, ratio2 = ratio2, repl = 1:replications, splits.path = splits.path))

lapply(datasets, function(i) addProblem(r, i, static = i, dynamic = NULL))
addExperiments(r, algo.designs = design, prob.designs = lapply(datasets, function(i) makeDesign(i)))
submitJobs(r, findExperiments(r), resources = list(walltime = 8*3600, memory = 2000))

           
