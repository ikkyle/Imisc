var.is.num <- function(x){
  x <- ifelse(is.na(x) | x == '', 1, x)
  y <- all(!is.na(as.numeric(x)))
  return(y)
}