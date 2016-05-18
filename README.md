# XLLoop X64 Version, 64bit

Tested on Windows 7 64bit, Microsoft Visual Studio 2015 Community Edition, Microsoft Excel 2016 64bit

Excel User-Defined Functions in Java, Javascript, Ruby, Python, Erlang

## Download

More information and downloads can be found at: <a href="http://xlloop.sourceforge.net/">xlloop.sf.net</a>

## Requirements

You need the following two libraries compiled for x64:
- xlcall32.lib : Download and install the [Excel SDK](https://msdn.microsoft.com/en-us/library/office/bb687883.aspx). You'll find the lib in `<installdir>/Excel2013XLLSDK/Excel2013XLLSDK/lib/x64`
- yajl.lib : Compiled via this [project](https://github.com/PATRONAS/xlloop/tree/master/yajl_x64)

- JDK >7 should be installed with `JAVA_HOME` environment variable set appropriately.
- Microsoft Foundation Classes for C++ are also required for building the XLLoop.xll.

## Compile the 64bit XLLoop.xll

1. Checkout this repository
2. Open & Build the `yajl_x64` Project
  1. The generated yajl.lib is your `yajl_64.lib`
3. Open the `xlloop-addin_x64` Project
  1. Create a lib folder, eg `xlloop-addin_x64\lib\x64`
  2. Add the yajl_x64.lib (step 2.1) and the xlcall32.lib (excel sdk) into this directory
4. Build the project
5. Load the generated xlloop.xll into you 64bit excel
