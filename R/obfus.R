obfus <- function(ch, rv=TRUE) {
  k <- ifelse(rv, 41, 13)
  p0 <- function(...) paste(c(...), collapse = "")
  A <- c(letters, LETTERS, " '")
  I <- seq_len(k) 
  chartr(p0(A), p0(c(A[-I], A[I])), ch)
}