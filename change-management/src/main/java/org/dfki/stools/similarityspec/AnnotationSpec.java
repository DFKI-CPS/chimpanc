/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import java.util.List;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:29
* To change this template use File | Settings | File Templates.
*/


public abstract class AnnotationSpec {
    public abstract List<String> getAllAnnotationNames();

    public abstract Boolean isValid();
}
