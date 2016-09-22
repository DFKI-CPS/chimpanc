/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import java.util.ArrayList;
import java.util.List;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:33
* To change this template use File | Settings | File Templates.
*/
public class AnnotationSpecs extends ArrayList<AnnotationSpec> {

    public AnnotationSpecs() { super(); }

    public AnnotationSpecs(List<AnnotationSpec> l) {
        super();
        addAll(l);
    }

    public AnnotationSpecs(AnnotationSpec... l) {
       super();
        for(AnnotationSpec s:l) add(s);
    }

    public Boolean contains(AnnotationNameSpec as) {
        for (AnnotationSpec e : this) {
            if (e instanceof AnnotationNameSpec) {
                AnnotationNameSpec es = (AnnotationNameSpec) e;
                if (es.getNameSpace().equals(as.getNameSpace()) &&
                        es.getName().equals(as.getName())) return true;
            }
        }
        return false;
    }

    public void insert(AnnotationNameSpec as) {
        if (!contains(as)) add(as);
    }

    public void insertAll(List<AnnotationNameSpec> asl) {
        for (AnnotationNameSpec as : asl) insert(as);
    }
}
