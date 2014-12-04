##### unknownToNA #####
unknownToNA <- function (x, unknown, warning = FALSE, ...) {
  UseMethod("unknownToNA")
}

unknownToNA.data.frame <- function (x, unknown, warning = FALSE, ...) {
  x[] <- unknownToNA.list(x = x, unknown = unknown, warning = warning)
  x
}

unknownToNA.default <- function (x, unknown, warning = FALSE, ...) {
  if (warning) {
    if (any(is.na(x))) 
      warning("'x' already has NA")
  }
  is.na(x) <- isUnknown(x = x, unknown = unknown)
  x
}

unknownToNA.factor <- function (x, unknown, warning = FALSE, ...) {
  if (warning) {
    if (any(is.na(x))) 
      warning("'x' already has NA")
  }
  if (is.list(unknown)) 
    unknown <- unlist(unknown)
  levs <- levels(x)
  levs <- levs[!(levs %in% unknown)]
  factor(x, levels = levs)
}


unknownToNA.list <- function (x, unknown, warning = FALSE, ...) {
  unknown <- .unknownList(x = x, unknown = unknown)
  x <- mapply(FUN = "unknownToNA", x = x, unknown = unknown, 
              warning = warning, SIMPLIFY = FALSE)
  return(x)
}

##### isUnknown() #####

isUnknown <- function (x, unknown = NA, ...) {
  UseMethod("isUnknown")
}

isUnknown.data.frame <- function (x, unknown = NA, ...) {
  x[] <- isUnknown.list(x, unknown = unknown, ...)
  x
}

isUnknown.default <- function (x, unknown = NA, ...) {
  if (is.list(unknown)) 
    unknown <- unlist(unknown)
  ret <- x %in% unknown
  if (any(is.na(unknown))) 
    ret <- ret | is.na(x)
  ret
}

isUnknown.list <- function (x, unknown = NA, ...) {
  unknown <- .unknownList(x = x, unknown = unknown)
  x <- mapply(FUN = "isUnknown", x = x, unknown = unknown, 
              ..., SIMPLIFY = FALSE)
  x
}

isUnknown.matrix <- function (x, unknown = NA, ...) {
  apply(X = x, MARGIN = ifelse(ncol(x) > nrow(x), 1, 2), FUN = isUnknown, 
        unknown = unknown)
}

isUnknown.POSIXlt <- function (x, unknown = NA, ...){
  if (is.list(unknown) && !inherits(x = unknown, what = "POSIXlt")) {
    unknown <- lapply(unknown, FUN = as.character, ...)
  }
  else {
    unknown <- as.character(x = unknown, ...)
  }
  if (is.list(x) && !inherits(x = x, what = "POSIXlt")) {
    x <- lapply(x, FUN = as.character, ...)
  }
  else {
    x <- as.character(x = x, ...)
  }
  isUnknown.default(x = as.character(x), unknown = as.character(unknown))
}

##### .unknownList #####
.unknownList <- function (x, unknown) {
  n <- length(x)
  unkN <- length(unknown)
  namesX <- names(x)
  namesXNullTest <- is.null(namesX)
  unkNames <- names(unknown)
  unkNamesNullTest <- is.null(unkNames)
  defInNames <- ".default" %in% unkNames
  defInd <- unkNames %in% ".default"
  def <- unknown[defInd]
  if (defInNames) {
    unkN <- unkN - 1
    unkNames <- unkNames[!defInd]
    unknown <- unknown[!defInd]
  }
  if (!namesXNullTest) {
    test <- !(unkNames %in% namesX)
    if (any(test)) 
      stop(sprintf("name(s) %s not in names of 'x'", paste(sQuote(unkNames[test]), 
                                                           collapse = " ")))
  }
  if (unkN < n) {
    if (unkNamesNullTest | defInNames) {
      if (defInNames) {
        names(def) <- NULL
        unknownDef <- rep(def, length = (n - unkN))
        names(unknownDef) <- namesX[!(namesX %in% unkNames)]
        unknown <- c(unknownDef, unknown)
      }
      else {
        unknownDef <- unknown
        unknown <- rep(unknownDef, length = n)
      }
    }
    else {
      stop("can not propely recycle named 'unknown'")
    }
  }
  if (!namesXNullTest) {
    if (unkNamesNullTest) {
      names(unknown) <- namesX
    }
    else {
      unknown <- unknown[match(namesX, names(unknown))]
    }
  }
  unknown
}