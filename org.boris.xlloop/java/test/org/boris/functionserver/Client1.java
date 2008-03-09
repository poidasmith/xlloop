package org.boris.functionserver;

import java.net.InetAddress;

import org.boris.functionserver.request.EchoRequest;
import org.boris.variantcodec.Variant;

public class Client1 {
    public static void main(String[] args) throws Exception {
        RequestExecutor re = new RequestExecutor(InetAddress.getLocalHost(),
                5454);

        for (int i = 0; i < 1000; i++) {
            EchoRequest er = new EchoRequest();
            er.addArg("test", 1);
            Variant v = re.execute(er);
            System.out.println(v);
        }

        re.disconnect();
    }
}
