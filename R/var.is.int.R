var.is.int <- function(x){
  x <- ifelse(is.na(x) | x == '', 1, x)
  if(all(!is.na(as.numeric(x)))){
    y <- all(as.numeric(x) %% 1 == 0, na.rm=TRUE)
  } else {
    y <- FALSE
  }
  return(y)
}
