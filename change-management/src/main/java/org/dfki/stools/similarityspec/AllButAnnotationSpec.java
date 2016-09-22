/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.antlr.runtime.tree.Tree;
import org.dfki.stools.ISElement;
import org.dfki.stools.SAnnotation;
import org.dfki.utils.Collectionxx;

import java.util.ArrayList;
import java.util.List;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:30
* To change this template use File | Settings | File Templates.
*/
public class AllButAnnotationSpec extends AnnotationSpec {
    protected ArrayList<AnnotationNameSpec> entries;

    public AllButAnnotationSpec() {
        entries = new ArrayList<AnnotationNameSpec>();
    }

    public AllButAnnotationSpec(ArrayList<AnnotationNameSpec> e) {
        entries = e;
    }

    public AllButAnnotationSpec(Tree tree, SimilaritySpec spec) {
        entries = new ArrayList<AnnotationNameSpec>();
        for(int i=0;i<tree.getChildCount();i++) {
           entries.add(AnnotationSimilaritySpec.parseAnnoQname(tree.getChild(i),spec));
        }
    }
    
    private <U> Boolean isExcluded(SAnnotation<U> sa) {
        for (AnnotationNameSpec ans : entries) {
            if (ans.getNameSpace().equals(sa.getNameSpace()) &&
                    ans.getName().equals(sa.getName())) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        String res = null;
        for(AnnotationNameSpec a:entries) {
            if (res==null) res = " _ \\ { "+a.toString();
            else res = res + ", "+a.toString();
        }
        if (res==null) res = " _ ";
        else res = res + "}";
        return res;

    }

    @Override
    public List<String> getAllAnnotationNames() {
        ArrayList<String> res = new ArrayList<String>();
        for (AnnotationNameSpec e : entries) {
            res.addAll(e.getAllAnnotationNames());
        }
        return res;
    }

    @Override
    public Boolean isValid() {
        Boolean result = true;
        for (AnnotationNameSpec e : entries) {
            if (!e.isValid()) return false;
        }

        return entries.size() > 0 && !Collectionxx.checkDuplicate(getAllAnnotationNames());
    }

    public <T, U> List<AnnotationNameSpec> eval(ISElement<T> e,SimilaritySpec sspec) {
        ArrayList<AnnotationNameSpec> res = new ArrayList<AnnotationNameSpec>();
        for (SAnnotation<?> sa : e.getAnnotations()) {
            if (!isExcluded(sa)) res.add(new AnnotationNameSpec(sa.getNameSpace(), sa.getName(), sspec));
        }
        return res;
    }
}
