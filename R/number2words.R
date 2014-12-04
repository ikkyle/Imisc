makeDigits <- function(x){
  strsplit(as.character(x), "")[[1]]}
makeNumber <- function(x){
  as.numeric(paste(x, collapse=""))}

number2words <- function(x, up=FALSE){
  ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  teens <- c("ten", "eleven", "twelve","thirteen", "fourteen", "fifteen","sixteen", " seventeen", "eighteen","nineteen")
  names(ones) <- names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty","fifty", "sixty", "seventy", "eighty","ninety")
  names(tens) <- 2:9
  suffixes <- c("thousand,", "million,", "billion,", "trillion,")
  negative <- x < 0
  x <- abs(x)
  digits <- makeDigits(x)
  nDigits <- length(digits)
  result <- if (nDigits == 1) as.vector(ones[digits])
  else if (nDigits == 2)
    if (x <= 19) as.vector(teens[digits[2]])
  else trim(paste(tens[digits[1]], "-", ones[digits[2]], sep=""))
  else if (nDigits == 3) {
    tail <- makeNumber(digits[2:3])
    if (tail == 0) paste(ones[digits[1]], "hundred")
    else trim(paste(ones[digits[1]], "hundred", number2words(tail)))
  }
  else {
    nSuffix <- ((nDigits + 2) %/% 3) - 1
    if (nSuffix > length(suffixes) || nDigits > 15)
      stop(paste(x, "is too large!"))
    pick <- 1:(nDigits - 3*nSuffix)
    trim(paste(number2words(makeNumber(digits[pick])),
               suffixes[nSuffix], number2words(makeNumber(digits[-pick]))))
  }
  if(up==TRUE){result <- gsub('^.{1}', toupper(substr(result, start=0, stop=1)), result)} else {result}
  result <- gsub('-zero', '', result)
  if (negative) {paste("minus", result)} else {result}
  return(result)
}