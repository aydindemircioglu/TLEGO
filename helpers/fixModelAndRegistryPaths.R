
#' replace old style registry naming to new style.
#' NOTE: NO SuBSAMPLINGRATE INCLUDED
#' 
fixModelAndRegistryPaths = function (masterTable) {

	# traverse master table
	for (p in 1:nrow(masterTable)) {

		# fix model path
		curPath = masterTable[p,]$modelPath
		newPath = gsub ("(?U)registry__.._.._....__.._.._.._(.*)/", "experiment1/reg_exp_\\1_1/", curPath)
		if (file.exists (newPath) == FALSE) {
			# next try! some models have _1_1 already :(
			newPath = gsub ("(?U)registry__.._.._....__.._.._.._(.*)/", "experiment1/reg_exp_\\1/", curPath)
			if (file.exists (newPath) == FALSE) {
				cat ("Model: Path", newPath, "does not exist.\n")
				stop ()
			}
		}
		masterTable[p,]$modelPath = newPath
		
		# fix registry path
		curPath = masterTable[p,]$registryPath
		newPath = gsub ("registry__.._.._....__.._.._.._(.*)", "experiment1/reg_exp_\\1_1", curPath)
		if (file.exists (newPath) == FALSE) {
			# next try! some models have _1_1 already :(
			newPath = gsub ("registry__.._.._....__.._.._.._(.*)", "experiment1/reg_exp_\\1", curPath)
			if (file.exists (newPath) == FALSE) {
				cat ("Registry: Path", newPath, "does not exist.\n")
				stop ()
			}
		}
		masterTable[p,]$registryPath = newPath
	}
	
	return (masterTable)
}

