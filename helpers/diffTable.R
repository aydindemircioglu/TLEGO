
# show differences between tables

diffTable = function (X, Y) {
	
	while (nrow(Y) > 0) {
		# take first entry of Y
		currentYRow = Y[1,]
		
		# find its block in Y
		YBlockIds = which (Y$solver == currentYRow$solver & Y$dataset == currentYRow$dataset & Y$C == currentYRow$C & Y$g == currentYRow$g)
		 
		# sanity check
		if (length(YBlockIds) == 0) {
			stop ("Sorry, this is madness.")
		}
		
		# find the block in X
		XBlockIds = which (X$solver == currentYRow$solver & X$dataset == currentYRow$dataset & X$C == currentYRow$C & X$g == currentYRow$g)

		# updating is now removing all XBlockIds and adding YBlockIds.
		if (is.null(XBlockIds) == FALSE) {
			X = X[-XBlockIds, ]
		} else {
			print ("Does not exist")
			print (Y[YBlockIds,])
		}
		X = rbind (X, Y[YBlockIds, ])
		
		# remove from Y so that the list shrinks
		Y = Y[-YBlockIds, ] 
	}
	
	return (X)
}


# 
# load("testmasterTable3.df")
# Y = parameters
# load("masterTable.df")
# X = parameters
# 
# print( str(X))
# print(str(Y))
# Z = diffTable(X, Y)
# print(str(Z))
