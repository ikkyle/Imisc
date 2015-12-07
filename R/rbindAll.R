rbindAll <- function(..., .from=TRUE){
  # arguments: ...   - data.frames you wish to rbind together 
  #            .from - if you want a variable that keeps track of which data.frame the data came from,
  #                    the function will create a variable called ".from" that holds the name of the data.frame
  #                    (it is called .from in case there is already a variable called from in the data)
  # return: a data.frame contaning all the variables and data from all the data.frames passed to the function
  #         Unlike base::rbind, data.frames are not required to have the same number of columns and the same variable names
  #         If variable names don't exist for a given data.frame, they will be created and set to NA before the rbind
  if(.from){
    fromData <- substitute(list(...))[-1]
    fromData <- sapply(fromData, deparse)
  } else {
    fromData <- list(NULL)
  }
  
  dotArgs <- list(...)
  var.names <- unique(unlist(sapply(dotArgs, names)))
  
  dotArgs <- mapply(function(dfs, f, vars){
    non.names <- vars[which(!vars %in% names(dfs))]
    dfs[non.names] <- NA
    dfs$.from <- f
    dfs
  }, dfs = dotArgs, f = fromData,
  MoreArgs = list(vars = var.names),
  SIMPLIFY = FALSE)
  
  dotArgs <- do.call(rbind, dotArgs)
  return(dotArgs)
}
