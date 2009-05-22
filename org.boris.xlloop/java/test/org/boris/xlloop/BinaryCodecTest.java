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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import junit.framework.TestCase;

import org.boris.xlloop.codec.BinaryCodec;
import org.boris.xlloop.xloper.XLoper;

public class BinaryCodecTest extends TestCase
{
    public void testEof() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        out.write(XLoper.xlTypeStr);
        out.write(10);
        out.write("hello".getBytes());
        ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
        XLoper x = BinaryCodec.decode(in);
        System.out.println(x);
    }
}
