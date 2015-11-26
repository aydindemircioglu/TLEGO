library(BBmisc)


system3 <- function (binPath, args, verbose = FALSE) 
{ 

  if (verbose == TRUE) {
    messagef("----- Arguments:")
    messagef("%s %s", binPath, paste(args, collapse=" "))
  }

  s = BBmisc::system3(binPath, args, stdout = TRUE)
  
  if (verbose == TRUE) {
    messagef("----- Output:")
    messagef("%s %s", binPath, paste(s$output, collapse="\n"))
    messagef("-------------")
  }
  
  return (s)
}
