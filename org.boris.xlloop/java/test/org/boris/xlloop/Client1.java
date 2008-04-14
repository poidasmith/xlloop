package org.boris.xlloop;

import java.net.InetAddress;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.RequestExecutor;

public class Client1 {
    public static void main(String[] args) throws Exception {
        RequestExecutor re = new RequestExecutor(InetAddress.getLocalHost(),
                5454);

        long t0 = System.currentTimeMillis();
        for (int i = 0; i < 10; i++) {
            VTStruct er = new VTStruct();
            er.add("asdf", 1);
            er.add("num", i);
            er.add("num2", i);
            er.add("num3", i);
            er.add("num4", i);
            er.add("num5", i);
            er.add("num6", i);
            er.add("num7", i);
            er.add("num8", i);
            er.add("num9", i);
            er.add("num23", i);
            er.add("num24", i);
            er.add("num25", i);
            er.add("num26", i);
            er.add("num27", i);
            er.add("num28", i);
            er.add("num29", i);
            Variant v = re.execute("Echo", er);
            System.out.println(v);
        }
        System.out.println(System.currentTimeMillis() - t0);

        re.disconnect();
    }
}
