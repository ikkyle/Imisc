errorizeFxn <- function(FUN){
  FUN2 <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())

    tryCatch(do.call(FUN, args),
             error = function(e){
               saveRDS(object = list(error = as.character(e),
                                     time = Sys.time(),
                                     fxn = FUN, 
                                     arglst = args),
                       file = 'error.Rds')
               stop(sprintf('Wrote to ./error_%.f7.Rds on catching "%s"', 
                            Sys.time(), as.character(e)))
             }, 
             warning = function(w){
               saveRDS(object = list(warning = as.character(w),
                                     time = Sys.time(),
                                     fxn = FUN, 
                                     arglst = args),
                       file = 'warning.Rds')
               warning(sprintf('Wrote to ./warning_%.f.Rds on catching "%s"', 
                               Sys.time(), as.character(w)))
             })
  }
  formals(FUN2) <- formals(FUN)
  FUN2 
}
