/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.xml.XMLConstants;
import java.util.ArrayList;
import java.util.List;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:30
* To change this template use File | Settings | File Templates.
*/
public class AnnotationNameSpec extends AnnotationSpec {
    protected SimilaritySpec spec;
    protected String ns;
    protected String annotationName;

    private static final Log log = LogFactory.getLog(AnnotationNameSpec.class);

    public AnnotationNameSpec(String ns, String s, SimilaritySpec spec) {
        this.spec = spec;
        this.ns = (ns==null) ? "" : ns;
        annotationName = s;
        if (s==null) log.error("Passed null as annotationName");
    }

    public String getNameSpace() {
        return ns;
    }

    public String getName() {
        return annotationName;
    }

    @Override
    public String toString() {
        if (!ns.isEmpty() && !ns.equals(XMLConstants.NULL_NS_URI)) {
            if (!spec.namespaceContext().getPrefix(ns).equals("")) {
                return spec.namespaceContext().getPrefix(ns) + ":" + annotationName;
            }
            // return ns+":"+annotationName;
        }
        return annotationName;
    }

    @Override
    public List<String> getAllAnnotationNames() {
        ArrayList<String> res = new ArrayList<String>();
        res.add(annotationName);
        return res;
    }

    @Override
    public Boolean isValid() {
        return true;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
