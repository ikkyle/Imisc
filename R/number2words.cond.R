num2words.cond <- function(x,...){
  # determines if the number is an integer or not
  int <- ifelse(abs(x - round(x)) < .Machine$double.eps^0.5, 1, 0)
  y = ifelse(int==1 & x < 10, number2words(x,...), x)
  return(y)
}