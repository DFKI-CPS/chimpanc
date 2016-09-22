/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.antlr.runtime.tree.Tree;
import org.dfki.stools.ISElement;
import org.dfki.utils.Collectionxx;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 16.02.2011
 * Time: 13:32
 * To change this template use File | Settings | File Templates.
 */
public class AllButElementSpec extends ElementSpec {
    protected ArrayList<ElementNameSpec> entries;


    public AllButElementSpec(List<ElementNameSpec> e) {
        entries = new ArrayList<ElementNameSpec>();
        entries.addAll(e);
    }
    
    public  AllButElementSpec(Tree tree, SimilaritySpec spec) {
        entries = new ArrayList<ElementNameSpec>();
        for(int i=0;i<tree.getChildCount();i++) {
            entries.add((ElementNameSpec) SubElementSimilaritySpec.parseElementSpec(tree.getChild(i),spec));
        }
    }

    private Boolean isExcluded(ISElement se) {
        for (ElementNameSpec ans : entries) {
            if (ans.getNamespace().equals(se.getNamespace()) &&
                    ans.getName().equals(se.getType())) {
                return true;
            }
        }
        return false;
    }

    public List<ElementNameSpec> eval(List<ISElement<?>> e, String equivspec, SimilaritySpec sspec) {
        ArrayList<ElementNameSpec> res = new ArrayList<ElementNameSpec>();
        for (ISElement<?> se : e) {
            if (!isExcluded(se)) {
                //System.err.println(String.format("Eval %s equiv %s sspec %s",se,equivspec,sspec));
                //System.err.println("Adding eval "+new ElementNameSpec(se.getNamespace(), se.getType(), equivspec, sspec));
                res.add(
                        new ElementNameSpec(se.getNamespace(), se.getType(), equivspec, sspec));
            }
        }
        return res;
    }


    @Override
    public List<String> getAllTagNames() {
        ArrayList<String> res = new ArrayList<String>();
        for (ElementNameSpec e : entries) {
            res.addAll(e.getAllTagNames());
        }
        return res;
    }

    @Override
    public Boolean isValid() {
        for (ElementNameSpec e : entries) {
            if (!e.isValid()) return false;
        }

        return entries.size() > 0 && !Collectionxx.checkDuplicate(getAllTagNames());
    }

    @Override
    public String toString() {
        String res = null;
        if (entries.isEmpty()) {
            res = " _ ";
        } else {
            res = "_ \\ {";
            Boolean first = true;
            for (ElementNameOrTupleSpec s : entries) {
                if (first) {
                    res = res + s.toString();
                    first = false;
                } else res = ", " + s.toString();
            }
        }
        res = res + "}";
        return res;
    }

}
