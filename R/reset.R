# function to remove all objects from memory, run garbage collection, and clear the console. 
# optionally remove all non-base packages too
reset <- function(prompt=TRUE, pkgs = FALSE){
  if (prompt) {
    p <- readline('Are you sure you want to reset? (y/n): ')
  } else {
    p <- 'y'
  }
  if (tolower(p) == 'y') {
    rm(list=ls(.GlobalEnv, all.names=TRUE), 
       inherits=TRUE)
    gc(FALSE)
    cat('\14') 
    if (pkgs) {
      pkg <- names(sessionInfo()$otherPkgs)
      if (!is.null(pkg)) {
        invisible(sapply(paste0('package:',pkg), 
                      detach, 
                      character.only = TRUE, 
                      unload=TRUE, 
                      force=TRUE))
      }
    }
  } else {
    cat('Reset cancelled')
  }
  sessionInfo()
}
