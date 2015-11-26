
# make all columns of a given table like that of the master table.

convertTable = function (X, Y) {

	# loop over all columns in X
	for (xC in colnames(X)) {
		# check existence
		cat ("Checking ", xC, "\n")
		if (xC %in% colnames(Y)) { 
			# convert to same type as X??
			if (class(X[[xC]]) == class(Y[[xC]]) ) {
				# same class, everything should be ok
			} else {
				if (class(X[[xC]]) == "factor") {
					Y[[xC]] = as.factor (Y[[xC]])
				} else if (class(X[[xC]]) == "integer") {
					Y[[xC]] = as.integer (Y[[xC]])
				} else if (class(X[[xC]]) == "numeric") {
					Y[[xC]] = as.numeric (Y[[xC]])
				} else if (class(X[[xC]]) == "character") {
					Y[[xC]] = as.character (Y[[xC]])
				} else {
					stop("Unsupported class", class(X[[xC]]) )
				}
			}
		} else { 
			# add to Y
			Y[[xC]] = NA
		}
	}
	
	# just for information
	for (yC in colnames(Y)) {
		if (yC %in% colnames(X) == FALSE) {
			cat ("\nColumn", yC, "was not transferred.\n")
		}
	}
	
	return (Y)
}


# 
# load("testmasterTable3.df")
# Y = parameters
# load("masterTable.df")
# X = parameters
# 
# print( str(X))
# print(str(Y))
# Z = convertTable(X, Y)
# print(str(Z))
