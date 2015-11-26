#!/usr/bin/Rscript --vanilla

# FIRST consistency check:
# we need to make sure that all experiments we conducted are really in the
# master table. each gridpoint needs to have an result.
# e.g. what about if some experiment did crash directly upfront?
# only in this case no model file was written, and thus no row in the master table
# exists. if the solver somehow did run successfully, then we have at least the
# final model in the model path, so the mastertable will have at least one entry
# corresponding to this one.

# FIXME: if the solver finishes at time X, this must be accounted as X, obviously not
# as wall time. (did i do this? this is the fixme check). in rare cases the solver might
# finish even below a second. then the final time must be set to 1 probably.
# need to check for these cases too.


checkMasterTableConsistency = function (masterTable) # TODO: allow giving it a grid from outside 
{
	# grid helper function
	source("experiments/getGridSize.R")
	source("helpers/sourceDir.R")
	sourceDir ("helpers")
	
	
	# count the number of validation NAs, but without validation time (do not understand this)
	masterTable$validationTime = NULL
	naRows = masterTable[complete.cases(masterTable) == FALSE,]

	cat("Total rows:", nrow(masterTable), "\n")
	cat("Complete rows:", nrow(masterTable) - nrow(naRows), "\n")
	cat("Incomplete rows:", nrow(naRows), "\n")
	
	trainMissing = length(which(is.na(masterTable$validationError)))
	cat("Missing validation rows:", nrow(naRows), "\n")

	missingCases = determineMissingCases (masterTable)
	cat("Missing training rows:", nrow(missingCases), "\n")
	
	# fix the table too
	colnames( missingCases) = colnames(masterTable)
	masterTable = rbind (masterTable, missingCases)
	

	# check if all models really exist on disk
	cat ("#################################")
	missingFiles = checkModelFiles (masterTable)
	cat("Missing files: ", nrow(missingFiles))
	print (missingFiles)
	cat ("#################################")
}
