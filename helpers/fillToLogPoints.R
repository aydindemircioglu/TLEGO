
# assume only one kind of data is given, i.e.
# points fixed solver, dataset, C and g. only time varies.
fillToLogPoints <- 	function (parameters, verbose = FALSE) {
	# create a new table
	newTable = parameters[0, ]
	

	# suppose we have one data point at some 2^i time, so everything is ok?
	# not really. in some cases we have NAs. that darn thing just jumped over
	# that time. so for now we just interpolate between the dots to the left
	# and right, if possible. because of this, we throw away all NAs.
	parameters = parameters[!is.na(parameters$validationError),]
	if (verbose) print(parameters)
	
	for (i in 1:15) {
		# is there a point at 2^i?
		curRow = subset(parameters, parameters$seconds == 2^i)
		if (verbose) cat ("At time", 2^i, "have", nrow(curRow), "rows\n")
		entry = NULL
		
		if (nrow(curRow) != 1) {
			# ok, we have too many or to few, check that
			if (nrow(curRow) == 0) {
				# no point
				# in that case we search for a left one
				if (verbose)  cat ("  -no rows found\n")
				leftPoints = subset (parameters, parameters$seconds < 2^i)
				leftPoints = leftPoints [with(leftPoints, order(-seconds)), ]
				if (nrow(leftPoints) > 0) {
					# we have at least one, take the nearest
					if (verbose)  cat ("  -left point found\n")
					leftPoint = leftPoints[1,]
					
					# look out for right Point
					rightPoints = subset (parameters, parameters$seconds > 2^i)
					rightPoints = rightPoints [with(rightPoints, order(seconds)), ]
					if (nrow(rightPoints) > 0) {
						rightPoint = rightPoints[1,]
						if (verbose)  cat ("  -right point found\n")
						# we have left and right, so we interpolate
						leftSeconds = leftPoint$seconds
						rightSeconds = rightPoint$seconds
						leftError = leftPoint$validationError
						rightError = rightPoint$validationError
						
						t = 2^i
						interError = (rightError - leftError)*(t - leftSeconds)/(rightSeconds - leftSeconds) + leftError
						if (verbose)  cat ("  -error computed:", interError, "\n")
						entry = leftPoint
						entry$seconds = t
						entry$validationError = interError
		#				entry$orgSeconds = NA
						# cat ("\nS:", leftSeconds, rightSeconds, "\nE:", leftError, rightError, "\nI:", interError)
					} else {
						# we do not have any right point, so we are at the end.
						# we can replicate the last point simply
						leftSeconds = leftPoint$seconds
						leftError = leftPoint$validationError
						entry = leftPoint
						entry$seconds = leftSeconds
						entry$validationError = leftError
	#					entry$orgSeconds = NA
					}
					
				} else {
					# we have none, so probably at the very start.
					# we skip this then, we do not want to extrapolate
				}
			} else {
				# too many rows
				# in that case expect the rows to be the SAME!
				if (nrow(unique(curRow)) != 1) {
					# ok, sometime it happens that the final model somehow was duplicated.
					# either by dump copying bug by myself, or just because the training was just
					# finished after writing the last prefinal model. this can happen not only 
					# on normal timeslots, contrarily it will necessarily happen if the algorithm
					# is beyond its walltime, e.g. if the walltime was 25200, but the algorithm
					# only finished its loop at 27761, then the 27761 model will be written
					# and then, without detour to the training algorithm, the loop will be exited
					# and the final model will be written. this will be (or should be) then the 
					# same as this last intermediate model. 
					
					# solution is to remove the prefinal model. this might theoretically hurt a bit,
					# but as the nearly-at-the-same-time solution is so close (or the same in case
					# of an walltime-timeout).
					# this remedies a little bit those models who takes years to write
					# the next model. and we throw away the stupid extra data from the bug.
					cat ("BEFORE\n")
					print (curRow)
					for (o in 1:nrow(curRow)) {
						seconds = suppressWarnings(as.numeric(tail(strsplit(curRow[o,]$modelFilename, '_')[[1]], n = 1)))
						if (is.na(seconds) == TRUE) {
							entry = rbind (entry, curRow[o,])
						} else { 
							# do nothing, it wont get added
						}
					}
					
					# now uniquify and expect only one model finally
					entry = unique(entry)
					if (nrow(entry) != 1) {
						print (entry)
						stop ("See. This is not unique!")
					}
					cat ("AFTER\n")
					print (entry)
				} else {
					# the rows are the very same, so just copt the first one
					entry = curRow [1,]
				}
			}
		} else {
			# we have one data point at this time, so everything is ok
			entry = curRow
		}
#		print(entry)
		newTable = rbind (newTable, entry)
	}
	
	return (newTable)
}
	
