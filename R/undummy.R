undummy <- function(data, omit=NULL){
  # converts a set of dummy variables to a single character vector 
  # if a value is omitted in the dummies, set its label in the omit argument, otherwise, let omit=NULL
  data <- lapply(data, function(x){
    ifelse(x==1, 'x', '')
  })
  for (i in seq_along(data)){
    data[[i]] <- gsub('x', names(data)[i], data[[i]])
  }
  labs <- do.call(paste0, data)
  
  if(!is.null(omit)){
    labs[which(labs=='')] <- omit
  } else {
    labs[which(labs=='')] <- NA
  }
  
  return(labs)
}
