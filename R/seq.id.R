seq.id <- function(id_columns){
  x <- ifelse(duplicated(cbind(id_columns)), 0, 1)
  n <- length(x)
  seq.total <- seq(from=1, to=n, by=1)
  index.pos <- c(seq.total[x==1], n+1)
  seq.list <- vector(mode='list', length=length(index.pos))
  seq.lengths <- vector(mode='list',length=length(index.pos))
  
  for(i in 1:(length(index.pos)-1)){
    seq.lengths[i] <- index.pos[i+1] - index.pos[i]
  }
  seq.lengths <- seq.lengths[1:length(index.pos)-1]
  
  for(i in 1:(length(index.pos)-1)){
    seq.list[i] <-  lapply(seq.lengths[i], function(x){
      seq(from=1, to=seq.lengths[[i]])
    })
  }                         
  seq.vector <- unlist(seq.list)
  return(seq.vector)
}