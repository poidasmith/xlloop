/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.util.Collection;
import java.util.Iterator;

public class CircularIterator implements Iterator
{
    private Collection collection;
    private Iterator iterator;

    public CircularIterator(Collection collection) {
        this.collection = collection;
    }

    public boolean hasNext() {
        return collection.size() > 0;
    }

    public Object next() {
        if (iterator == null || !iterator.hasNext())
            iterator = collection.iterator();

        return iterator.next();
    }

    public void remove() {
    }
}
