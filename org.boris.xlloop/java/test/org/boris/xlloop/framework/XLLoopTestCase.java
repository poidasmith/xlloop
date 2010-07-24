package org.boris.xlloop.framework;

import java.io.IOException;
import java.net.InetAddress;

import junit.framework.TestCase;

import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestExecutor;
import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLoper;

public abstract class XLLoopTestCase extends TestCase
{
    private String host = "localhost";
    private int port = 5454;
    private RequestExecutor executor;
    private XLoperObjectConverter converter = new XLoperObjectConverter();

    protected void setUp() throws Exception {
        super.setUp();
        create();
    }

    protected void setHost(String host) throws Exception {
        this.host = host;
        create();
    }

    protected String getHost() {
        return host;
    }

    protected void setPort(int port) throws Exception {
        this.port = port;
        create();
    }

    protected int getPort() {
        return port;
    }

    protected void create() throws Exception {
        executor = new RequestExecutor(InetAddress.getByName(host), port);
    }

    protected XLoper execute(String name, XLoper... args)
            throws RequestException, IOException {
        return executor.execute(name, args);
    }

    protected Object execute(String name, Object... args) throws Exception {
        XLoper[] a = new XLoper[args.length];
        for (int i = 0; i < a.length; i++)
            a[i] = converter.createFrom(args[i]);
        XLoper res = execute(name, a);
        return converter.createFrom(res, Object.class);
    }
}
