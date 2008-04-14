/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.script;

import java.io.IOException;
import java.io.Reader;

import org.boris.xlloop.Function;

public interface ScriptFactory 
{
    public Function create(Reader r) throws IOException;
}
