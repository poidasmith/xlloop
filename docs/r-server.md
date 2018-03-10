# ![XLLoop](./logo.gif) XLLoop 
An R Function Server

## About

Included in the download is an R implementation of the XLLoop server process. For more information on R try www.r-project.org.

## Usage

The R server implementation consists of a single file: xlloop.R. The code listing for an example server is as follows:

```r
> ProductTest <- function(x, y) x*y
>
> source("xlloop.R")
>
> XLLoopServer()
```

This creates a new server (a socket listening on port 5454) and will provide the function ProductTest, which can be invoked as:

```
=FS("ProductTest", 32, 1886.5)
```