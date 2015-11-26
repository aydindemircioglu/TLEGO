
determineMissingCases = function (masterTable = NULL) {
	# determine all solver and datasets from masterTable
	solver = sort(unique(masterTable$solver))
	dataset = sort(unique(masterTable$dataset))

	missingCases = NULL
	for (d in dataset) {
		for (s in solver) {
			# get grid size we want to check
			tL = getGridSize (d)
			dataPoints = NULL

			cat ("Checking", d, "on", s, "\n")
			
			crosscut = subset(masterTable, solver == s & dataset == d)
			for (tC in tL$C) {
				breezecut = subset(crosscut, C == 2^tC)
				for (tg in tL$g) {
					#print (c(s,d,tC,tg))
					t = subset(breezecut, g == 2^tg)
					if (nrow(t) < 1) {
						cat (".")
						entry = masterTable[1,]
						entry[1,] = NA
						entry$solver = s
						entry$dataset = d
						entry$C = 2^tC
						entry$g = 2^tg
						entry$split = 1
						missingCases = rbind(missingCases, entry)
					}
				}
			}
		}
	}
	
	print (head(missingCases))
	cat ("Have", nrow(missingCases), "rows\n")
	return (missingCases)
}