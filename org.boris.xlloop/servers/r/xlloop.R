##
## This version has been modified by Peter Smith to support xlloop 0.1.0 protocol (and higher)
##


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

decodeDouble <- function(con) readBin(con, 1, 1L, 8L, endian="big")
decodeStr <- function(con) { 
	len = readBin(con, 1L, 1L, 1L)
	if (len) readChar(con, len, TRUE) else "" 
}
decodeBool <- function(con) readBin(con, 1L, 1L, 1L)
decodeErr <- function(con) {
	err = decodeInt(con)
	class(err) <- "xloper.error"
}
decodeMulti <- function(con) {
  rows = decodeInt(con)
  cols = decodeInt(con)
  len = rows * cols
  if (len == 0) return(list())
  lapply(1:len, function(x) decodeXloper(con))
}
decodeInt <- function(con) readBin(con, 1L, 1L, 4L, endian="big")
decodeSRef <- function(con) {
	colFirst = decodeInt(con)
	colLast = decodeInt(con)
	rowFirst = decodeInt(con)
	rowLast = decodeInt(con)
	sref = list(colFirst=colFirst, colLast=colLast, rowFirst=rowFirst, rowLast=rowLast)
	class(sref) <- "xloper.sref" 
}
decodeXloper <- function(con) {
  t <- readBin(con, 1L, 1L, 1L)
  if (t < 1L || t > 9L) stop("protocol error, unknown data type ", t)
  if (t == 1L) return(decodeDouble(con))
  if (t == 2L) return(decodeStr(con))
  if (t == 3L) return(decodeBool(con))
  if (t == 4L) return(decodeErr(con))
  if (t == 5L) return(decodeMulti(con))
  if (t == 6L) return(NULL)
  if (t == 7L) return(NULL)
  if (t == 8L) return(decodeInt(con))
  if (t == 9L) return(decodeSRef(con))
}

encodeInt <- function(value, con) return(writeBin(value, con, 4L, endian="big"))
encodeXloper <- function(value, con) {
	if (length(value) == 0) return(writeBin(7L, con, 1L))
	if (length(value) == 1L) {
		if (is.integer(value)) { writeBin(8L, con, 1L); return(writeBin(c(0L, value), con, endian="big")) }
		if (is.numeric(value)) { writeBin(1L, con, 1L); return(writeBin(as.double(value), con, endian="big")) }
		if (!is.list(value)) {
			value = charToRaw(as.character(value))
			writeBin(2L, con, 1L)
			writeBin(length(value), con, 1L, endian="big")
			writeBin(value, con)
			return(length(value))
		}
	}
	writeBin(5L, con, 1L)
	writeBin(length(value), con, 4L, endian="big") ## Rows
	writeBin(1L, con, 4L, endian="big")
	for (i in 1:length(value)) encodeXloper(value[i], con)
	return(length(value))
}

XLLoopHandler <- function(context, name, argv) {
    fn <- if (exists(name)) get(name) else try(eval(parse(text=name)), silent=TRUE)
	if (is.function(fn)) {
		return(try(do.call(fn, argv), silent=TRUE))
	} else {
		return("#Unknown Function")
	}
}

XLLoopHandlerInvoker <- function(con, context, name) {
	argc = decodeXloper(con)
	argv = list()
	if(argc > 0) {
		argv <- lapply(1:argc, function(x) NULL)
		for(i in 1:argc) {
			argv[i] = decodeXloper(con)
		}
		# chomp trailing nulls
		while (length(argv) && is.null(argv[[length(argv)]])) argv[[length(argv)]] <- NULL
	}
	cat("name "); str(name)
	result = XLLoopHandler(context, name, argv)
	encodeXloper(result, con)
}

XLLoopServer <- function(host="localhost", port=5454L, verbose=FALSE) {
  con <- socketConnection(host, port, TRUE, open="a+b", encoding="latin1")
  on.exit(close(con))
  active <- TRUE
  while (active) {
	name = decodeXloper(con)
	if(verbose) {
		cat("function name ")
		str(name)
	}
	if(is.numeric(name)) {
		hasContext = decodeXloper(con)
		if(verbose) {
			cat("has context ")
			str(hasContext)
		}
		if(hasContext == 1) {
			caller = decodeXloper(con)
			sheetName = decodeXloper(con)
			context = list(caller=caller, sheetName=sheetName)
			funName = decodeXloper(con)
			XLLoopHandlerInvoker(con, context, funName)
		} else {
			funName = decodeXloper(con)
			XLLoopHandlerInvoker(con, NULL, funName)
		}
	} else {
		XLLoopHandlerInvoker(con, NULL, name)
	}
  }
}

