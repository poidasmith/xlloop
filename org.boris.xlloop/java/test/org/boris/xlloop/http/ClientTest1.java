/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.http;

import java.net.URL;

import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLoper;

public class ClientTest1
{
    public static String reqj = "{\"args\":[],\"name\":\"org.boris.xlloop.GetFunctions\",\"request\":\"XLLoop\",\"version\":\"0.1.0\"}";

    public static void main(String[] args) throws Exception {
        URL u = new URL("http://localhost:8000/xlloop/TestServer.php");
        String n = "Sum";
        XLoper[] xlargs = createArgs();
        XLoper res = FunctionExecutor.execute(u, n, xlargs);
        System.out.println(JSONCodec.encode(res).toString(4));
    }

    private static XLoper[] createArgs() {
        // return new XLoper[] { new XLArray(2, 2), CompTest1.makeRandom(),
        // CompTest1.makeRandom(), CompTest1.makeRandom() };
        return new XLoper[] { createRandomArray() };
    }

    private static XLoper createRandomArray() {
        XLArray x = new XLArray(15, 10);
        for (int i = 0; i < 15; i++) {
            int len = (int) (Math.random() * 10) + 1;
            for (int j = 0; j < len; j++) {
                x.set(i, j, i * j);
            }
        }
        return x;
    }
}
