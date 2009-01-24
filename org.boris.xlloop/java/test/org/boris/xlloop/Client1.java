package org.boris.xlloop;

import java.net.InetAddress;

import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLoper;

public class Client1
{
    public static void main(String[] args) throws Exception {
        RequestExecutor re = new RequestExecutor(InetAddress.getLocalHost(),
                5454);

        long t0 = System.currentTimeMillis();
        for (int i = 0; i < 10; i++) {
            XLList er = new XLList();
            er.add(1);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            er.add(i);
            XLoper v = re.execute("Echo", er.toXLoper().array);
            System.out.println(v);
        }
        System.out.println(System.currentTimeMillis() - t0);

        re.disconnect();
    }
}
