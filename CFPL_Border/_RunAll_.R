##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-06

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())




R.files <- list.files("R", full.names = TRUE)
R.files <- R.files[grepl("\\.R$", x = R.files)]

f_run <- function(file) {
  print(file)
  source(file, chdir = TRUE)
  return(invisible())
}


lapply(R.files, f_run)

