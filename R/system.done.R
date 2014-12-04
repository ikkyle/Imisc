system.done <- function(expr, dev=1){
  ppt <- function(y) {
    if (!is.na(y[4L])) 
      y[1L] <- y[1L] + y[4L]
    if (!is.na(y[5L])) 
      y[2L] <- y[2L] + y[5L]
    y[1L:3L]
  }
  if (!exists("proc.time")) 
    return(rep(NA_real_, 5L))
  time <- proc.time()
  on.exit({cat("Timing stopped at:", ppt(proc.time() - time), 
              "\n")
           pbPost(type='note',
                  title='Error',
                  body=geterrmessage(), 
                  deviceind=dev)
           beep()})
  expr
  new.time <- proc.time()
  on.exit()
  
    pbPost(type='note',
           title='Done!',
           body=paste('expression took ', round((new.time - time)/ 60, 2), ' minutes'), 
           deviceind=dev)
  
  beep()
  
  structure(new.time - time, class = "proc_time")
}