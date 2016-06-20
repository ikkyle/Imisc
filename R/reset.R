# function to remove all objects from memory, run garbage collection, and clear the console
reset <- function(){
  rm(list=ls(all=TRUE), envir = .GlobalEnv)
  gc(FALSE)
  cat('\14')
}
