nvalid <- function(x){
  x <- unknownToNA(x, '')
  x <- cbind(x, rep(NA, nrow(data.frame(x))))
  n.valid <- rowSums(ifelse(!is.na(x), 1, 0), na.rm=TRUE)
  return(n.valid)}