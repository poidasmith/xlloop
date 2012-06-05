# XLLoop - Excel User-Defined Functions in Java, Javascript, Ruby, Python, Erlang

## About

XLLoop is an open source framework for implementing Excel user-defined functions (UDFs) on a centralised server (a function server). 

*Why is this useful?*
* Functions can be written in any language (e.g. Java, scripting languages etc..)
* Functions can be added quickly and dynamically without users having to restart Excel.
* Functions can be managed separately and centrally, which avoids the costly overhead of managing many XLLs and ensures all users * are using the same functions.
* Data can be shared across excel sessions/users (e.g. current stock market prices can be stored on a single server and all Excel sessions could retrieve this data via a GetStock function).

It has the following features:
* Extensible Java server framework.
* Native server frameworks written in many other languages (see languages section).
* Popup for long running operations (with option to cancel)
* Support for hosting C++ XLLs via JXLL.
* Reflection-based function adaptor for extremely quick deployment of java methods.
* Bean scripting framework adaptor so functions can be written in any BSF-supported language (eg. Ruby, Python, Javascript).
* Lisp Function Handler for evaluating Lisp expressions on the fly.
* Functions can be dynamically added to Excel without restart.
* Functions can be registered as Excel functions (with help information).
* Fast binary protocol with multiple-server fail-over/scalability.
* JSON over HTTP(S) protocol
* Can be configured to connect to multiple function servers (providers)
* XLLoop consists of two main components:
* An Excel addin implementation (XLL written in c++).
* A server and framework written in java (and in many other languages - see languages section).

The addin and server communicate via a simple socket-based protocol, sending and receiving (binary) serialised excel objects (called xlopers). 

In JSON mode the addin and server communicate over http(s), sending and receiving JSON serialised excel objects. 

XLLoop is licensed under the Common Public License (CPL).

## Download

Download the latest at [xlloop.sf.net][http://xlloop.sourceforge.net/] (for now)

## Language Support

<table width="400">
	<tr><th>Language</th><th>Supported?</th></tr>
	<tr><td>Java</td><td>Yes, native server (binary and json)</td></tr>
	<tr><td>C++</td><td>Yes, via XLLServer (included in download)</td></tr>
	<tr><td>Erlang</td><td>Yes, <a href="erlangserver.html">native server</a></td></tr>
	<tr><td>Lisp</td><td><a href="lisphandler.html">Yes</a>, using <a href="http://jatha.sourceforge.net/">Jatha</a></td></tr>
	<tr><td>Ruby</td><td>Yes, a native <a href="rubyserver.html">ruby server</a> is included in the download.</td></tr>
	<tr><td>Python</td><td>Yes, a native <a href="pythonserver.html">python server</a> is included in the download.</td></tr>
	<tr><td>PHP</td><td>Yes, a native server (json) is included in the download.</td></tr>
	<tr><td>Javascript</td><td>Yes, using <a href="http://www.mozilla.org/rhino/">Rhino</a></td></tr>
	<tr><td>R</td><td>Yes, a native <a href="rserver.html">R server</a> is included in the download.</td></tr>
	<tr><td>Perl</td><td>Yes, a native Perl server has been implemented here: <a href="https://github.com/aero/XLLoop-perl5">github.com/aero/XLLoop-perl5</a></td></tr>
</table>

# Usage

