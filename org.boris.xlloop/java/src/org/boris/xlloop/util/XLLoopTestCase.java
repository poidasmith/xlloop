package org.boris.xlloop.util;

import java.io.IOException;
import java.net.InetAddress;

import junit.framework.TestCase;

import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestExecutor;
import org.boris.xlloop.xloper.XLoper;

public abstract class XLLoopTestCase extends TestCase
{
    private String host = "localhost";
    private int port = 5454;
    private RequestExecutor executor;

    protected void setUp() throws Exception {
        super.setUp();
        createExecutor();
    }

    protected void setHost(String host) throws Exception {
        this.host = host;
        createExecutor();
    }

    protected String getHost() {
        return host;
    }

    protected void setPort(int port) throws Exception {
        this.port = port;
        createExecutor();
    }

    protected int getPort() {
        return port;
    }

    private void createExecutor() throws Exception {
        executor = new RequestExecutor(InetAddress.getByName(host), port);
    }

    protected XLoper executeFunction(String name, XLList args)
            throws RequestException, IOException {
        return executor.execute(name, args);
    }
}
