package org.boris.functionserver;

import java.net.InetAddress;

import org.boris.functionserver.request.EchoRequest;
import org.boris.variantcodec.Variant;

public class Client1 {
    public static void main(String[] args) throws Exception {
        RequestExecutor re = new RequestExecutor(InetAddress.getLocalHost(),
                5454);

        long t0 = System.currentTimeMillis();
        for (int i = 0; i < 10000; i++) {
            EchoRequest er = new EchoRequest();
            er.addArg("asdf", 1);
            er.addArg("num", i);
            er.addArg("num2", i);
            er.addArg("num3", i);
            er.addArg("num4", i);
            er.addArg("num5", i);
            er.addArg("num6", i);
            er.addArg("num7", i);
            er.addArg("num8", i);
            er.addArg("num9", i);
            er.addArg("num23", i);
            er.addArg("num24", i);
            er.addArg("num25", i);
            er.addArg("num26", i);
            er.addArg("num27", i);
            er.addArg("num28", i);
            er.addArg("num29", i);
            Variant v = re.execute(er);
            // System.out.println(v);
        }
        System.out.println(System.currentTimeMillis() - t0);

        re.disconnect();
    }
}
