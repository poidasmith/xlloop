## simple XLLoop server written in R (based on xlloop 0.0.7 protocol)
##
## Copyright (c) 2009 Simon Urbanek <simon.urbanek@r-project.org>
##
## it is very, very simplistic ... and not meant to be pretty ...
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use,
## copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following
## conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
## OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
## HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
## WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
## OTHER DEALINGS IN THE SOFTWARE.

readLong <- function(con) { readBin(con, 1L, 1L, 4L); readBin(con, 1L, 1L, 4L, endian="big") }
readDouble <- function(con) readBin(con, 1, 1L, 8L, endian="big")
readString <- function(con) { n <- readBin(con, 1L, 1L, 4L, endian="big"); if (n) readChar(con, n, TRUE) else "" }

readCollection <- function(con) {
  n <- readBin(con, 1L, 1L, 4L, endian="big")
  if (n == 0) return(list())
  lapply(1:n, function(x) readVariant(con))
}

readStruct <- function(con) {
  n <- readBin(con, 1L, 1L, 4L, endian="big")
  if (n == 0) return(list())
  l <- lapply(1:n, function(x) NULL)
  for (i in 1:n) {
    names(l)[i] <- readString(con)
    l[[i]] <- readVariant(con)
  }
  l
}

readVariant <- function(con) {
  t <- readBin(con, 1L, 1L, 1L)
  if (t < 2L || t > 7L) stop("protocol error, unknown data type ", t)
  if (t == 7L) return(NULL)
  if (t == 6L) return(readLong(con))
  if (t == 5L) return(readDouble(con))
  if (t == 4L) return(readString(con))
  if (t == 3L) return(readCollection(con))
  readStruct(con)
}

vread <- function(con) {
  req <- 0L
  t <- readBin(con, 1L, 1L, 1L)
  if (t == 6) { req <- readLong(con); t <- readBin(con, 1L, 1L, 1L) }
  else if (t != 4) stop("protocol error (expecting type 4 or 6, got ",t,")")
  if (t != 4) stop("protocol error (expecting type 4, got ",t,")")
  list(req=req, name=readString(con), data=readVariant(con))  
}

writeRObj <- function(con, x, name = NULL) {
  if (length(x) == 0L) return(writeBin(7L, con, 1L))
  if (length(x) == 1L) {
    if (is.integer(x)) { writeBin(6L, con, 1L); return(writeBin(c(0L, x), con, endian="big")) }
    if (is.numeric(x)) { writeBin(5L, con, 1L); return(writeBin(as.double(x), con, endian="big")) }
    if (!is.list(x)) {
      x = charToRaw(as.character(x))
      writeBin(4L, con, 1L)
      writeBin(length(x), con, 4L, endian="big")
      writeBin(x, con)
      return(length(x))
    }
  }
  if (is.null(names(x))) {
    writeBin(3L, con, 1L)
    writeBin(length(x), con, endian="big")
    for (i in 1:length(x)) writeRObj(con, x[[i]])
    return(length(x))
  }
  writeBin(2L, con, 1L)
  writeBin(length(x), con, endian="big")
  for (i in 1:length(x)) {
    na = names(x)[i]
    na <- if (is.null(na)) "" else as.character(na)
    writeBin(4L, con, 1L)
    writeBin(charToRaw(na), con)
    writeRObj(con, x[[i]])
  }
  length(x)
}

vsend <- function(con, data, ok=TRUE) {
  ok <- if (ok) "Ok" else "Error"
  writeBin(4L, con, 1L)
  writeBin(nchar(ok), con, 4L, endian="big")
  writeBin(charToRaw(ok), con)
  writeRObj(con, data)
}

# this is just to make xlloop happy - it calls "GetFunctions" on startup (not sure why since it doesn't affect anythying)
GetFunctions <- function(...) c("GetFunctions")

# xlloop has no concept of vectors vs lists, so we have to guess ...
tryunlist <- function(x) {
  if (is.list(x) && length(x)) {
    ctl <- lapply(x, is.list)
    if (!any(ctl)) { # do unlisting only if the list is not nested
      x <- lapply(x, function(.) if(is.null(.)) NA else .) # replace NULLs with NAs
      unlist(x)
    } else x
  } else x
}

xllServer <- function(host="localhost", port=5454L, verbose=FALSE) {
  con <- socketConnection(host, port, TRUE, open="a+b", encoding="latin1")
  on.exit(close(con))
  active <- TRUE
  while (active) {
    r <- vread(con)
    ## first try it as a function/variable name - if that fails, parse+evaluate
    fn <- if (exists(r$name)) get(r$name) else try(eval(parse(text=r$name)), silent=TRUE)
    res <- if (is.function(fn)) {
      if (verbose) cat("Call function '",r$name,"'\n",sep='')
      l <- r$data
      ## this may sound utterly stupid, but in R it actually is a non-no-op :P
      while (length(l) && is.null(l[[length(l)]])) l[[length(l)]] <- NULL
      if (length(l)) l <- lapply(l, tryunlist)
      if (verbose) { cat("args: \n"); str(l) }
      try(do.call(fn, l), silent=TRUE)
    } else fn # if it's not a function, return it as-is in the expectation that it was an expression
    if (verbose) { cat("result: \n"); str(res) }
    vsend(con, res, !inherits(res, "try-error"))
  }
}
