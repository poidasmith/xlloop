package org.boris.xlloop;

public class RequestException extends Exception 
{
    public RequestException() {
        super();
    }

    public RequestException(String message, Throwable cause) {
        super(message, cause);
    }

    public RequestException(String message) {
        super(message);
    }

    public RequestException(Throwable cause) {
        super(cause);
    }
}
