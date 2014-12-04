paste_ws <- function(...,sep="; ", collapse=NULL) {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <- do.call(paste,c(L,list(sep=sep, collapse=collapse)))
  ret <- gsub(paste0('^(',sep,'){1,}|(', sep, '){1,}$'), '', ret)
  ret <- gsub(paste0('(',sep,'){2,}'), sep, ret)
  is.na(ret) <- ret==""
  ret
}