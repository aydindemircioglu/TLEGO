
# @param path
#   path of the data file
# @param outDir
#   Directory for the output files. Must not contain files named testData.dat
#   and trainData.dat
# @param ratio1
#   Ratio for the size of the training data must sum up to 1
# @param ratio2
#   Ratio for the size of the valid, valid data. ratio1 + ratio2 < 1 must hold
#   and 1 - ratio1 - ratio2 is the ratio of the test data
# @param trainInds
#   Indizes of the training observations. if NULL, these are sample
# @return
#   list with pathes for traindata and testdata

makeTrainTestSplit <- function (path, outDir, ratio1, ratio2, trainInds = NULL) {
  # some tests, if files (not) exists
    library(e1071)

    if(!file.exists(path))
        stop ("  The combined and scaled file" , path, " does not exist!") 
    if(file.exists(paste(outDir, "testData.dat", sep = "/")))
        stop ("  There is already a testfile in ", outDir) 
    if(file.exists(paste(outDir, "trainData.dat", sep = "/")))
        stop ("  There is already a trainfile in ", outDir) 
    if(file.exists(paste(outDir, "validData.dat", sep = "/")))
        stop ("  There is already a trainfile in ", outDir) 
  
    dataset = read.matrix.csr (path)
    n = nrow(dataset$x)
    ratio = c(ratio1, ratio2, 1 - ratio1 - ratio2)
    if(is.null(trainInds)) {
        sizes = floor(ratio * n)
        inds = sample(unlist(lapply(1:3, function(i) rep(i, sizes[i]))))
    }
  
    cat ("Creating directory ", outDir, "\n")
    dir.create (outDir, recursive = TRUE, showWarnings = FALSE)

    write.matrix.csr(x = dataset$x[inds == 1,], y = dataset$y[inds == 1],  file = paste(outDir, "trainData.dat", sep = "/"))
    write.matrix.csr(x = dataset$x[inds == 2,], y = dataset$y[inds == 2], file = paste(outDir, "validData.dat", sep = "/"))
    write.matrix.csr(x = dataset$x[inds == 3,], y = dataset$y[inds == 3], file = paste(outDir, "testData.dat", sep = "/"))
  
    return (list(train = paste(outDir, "trainData.dat", sep = "/"),
              valid  = paste(outDir, "validData.dat", sep = "/"),
              test  = paste(outDir, "testData.dat", sep = "/")
              ))
  
}

