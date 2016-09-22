/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import java.util.List;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:31
* To change this template use File | Settings | File Templates.
*/
public abstract class ElementSpec {
    public abstract List<String> getAllTagNames();

    public abstract Boolean isValid();

}
