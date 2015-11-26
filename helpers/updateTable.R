
# update table X by entries of table Y
# if point exists in X, it will be updated from Y
# if it does not exist, it will be added.
# this must and does so blockwise,
# i.e. a block is a (C,g,solver,dataset) tuple
# and all the seconds. All of these will be removed
# when being updated.

updateTable = function (X, Y) {
	
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
		cat ("Found ", length(XBlockIds), "rows to update.\n")
		
		# updating is now removing all XBlockIds and adding YBlockIds.
		if (length(XBlockIds) > 0) {
			X = X[-XBlockIds, ]
		}
		cat ("X has now ", nrow(X), "rows \n")
		X = rbind (X, Y[YBlockIds, ])
		
		# remove from Y so that the list shrinks
		Y = Y[-YBlockIds, ] 
	}
	
	return (X)
}


# 
# load("aXa_BSGD_Table_UNFAC.df")
# Y = parameters
# load("masterTable_VT.df")
# X = parameters
# 
# print( str(X))
# print(str(Y))
# Z = updateTable(X, Y)
# print(str(Z))
