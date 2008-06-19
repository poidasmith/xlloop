/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import org.boris.variantcodec.BinaryCodec;
import org.boris.variantcodec.HexDump;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTMap;
import org.boris.xlloop.util.IO;

public class BinCodec1
{
    public static void main(String[] args) throws Exception {
        //file1();
        file14();
    }
    
    public static void file1() throws Exception {
        VTMap fcall = new VTMap();
        fcall.add("name", "FTest");
        VTCollection ar = new VTCollection();
        fcall.add("args", ar);
        ar.add(1);
        ar.add("asdf");
        ar.add(3.5);
        VTCollection em = new VTCollection();
        for(int i = 0; i < 10; i++) {
            em.add(Math.random());
        }
        ar.add(em);
        System.out.println(fcall);
        FileOutputStream fos = new FileOutputStream("erlang-server/file1.bvc");
        BinaryCodec.encode(fcall, fos);
        fos.close();
    }
    
    public static void file12() throws Exception {
        HexDump.dump(IO.toBytes(new File("erlang-server/file1.bvc")));
    }

    public static void file13() throws Exception {
        System.out.println(BinaryCodec.decode(new FileInputStream("erlang-server/file2.bvc")));
    }

    public static void file14() throws Exception {
        System.out.println(BinaryCodec.decode(new FileInputStream("erlang-server/file1.bvc")));
        System.out.println(BinaryCodec.decode(new FileInputStream("erlang-server/file2.bvc")));
    }
}
