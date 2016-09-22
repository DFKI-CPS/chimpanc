/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.antlr.runtime.tree.Tree;
import org.dfki.stools.ISElement;
import org.dfki.stools.SElementTuple;
import org.dfki.utils.Collectionxx;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 16.02.2011
 * Time: 13:33
 * To change this template use File | Settings | File Templates.
 */
public class ElementTupleSpec extends ElementNameOrTupleSpec {
    protected List<ElementNameSpec> namespecs;

    public ElementTupleSpec(List<ElementNameSpec> tns) {
        namespecs = tns;
    }

    public ElementTupleSpec(Tree tree, SimilaritySpec spec) {
        namespecs = new ArrayList<ElementNameSpec>();
        for(int i=0;i<tree.getChildCount();i++) {
            namespecs.add((ElementNameSpec) SubElementSimilaritySpec.parseElementSpec(tree.getChild(i),spec));
        }
    }
    public Boolean contains(ElementNameSpec ens) {
        for (ElementNameSpec e : namespecs) {
            if (e.getNamespace().equals(ens.getNamespace()) &&
                    e.getName().equals(ens.getName()) &&
                    e.getEquivSpec().equals(e.getEquivSpec())) return true;
        }
        return false;
    }

    public ElementNameSpec get(int i) {
        return namespecs.get(i);
    }

    @Override
    public List<String> getAllTagNames() {
        ArrayList<String> names = new ArrayList<String>();
        for (ElementNameSpec e : namespecs) names.addAll(e.getAllTagNames());
        return names;
    }

    @Override
    public Boolean isValid() {
        return namespecs.size() > 1 && !Collectionxx.checkDuplicate(getAllTagNames());
    }

    public String toString() {
        String res = "(";
        Boolean first = true;
        for (ElementNameSpec s : namespecs) {
            if (first) {
                res = res + s.toString();
                first = false;
            } else res = res + ", " + s.toString();
        }
        res = res + ")";
        return res;
    }

    public List<SElementTuple<?>> eval(List<ISElement<?>> al) {
        ArrayList<SElementTuple<?>> res = new ArrayList<SElementTuple<?>>();
        res = new ArrayList<SElementTuple<?>>();
        for (int i = 0; i < al.size(); i++) {
            SElementTuple<?> m = match(al, i);
            if (m != null) res.add(m);
        }
        return res;
    }

    public SElementTuple<?> match(List<ISElement<?>> l, int from) {
        if (l.size() > from + namespecs.size()) {
            SElementTuple<?> t = new SElementTuple();
            for (int i = 0; i < namespecs.size(); i++) {
                ISElement<?> se = l.get(from + i);
                ElementNameSpec ens = namespecs.get(i);
                if (se.getNamespace().equals(ens.getNamespace()) &&
                        se.getType().equals(ens.getName())) {
                    t.add(se);
                } else return null;
            }
            // Ajdusting the specific equivspec names for the recursion
            for (int i = 0; i < namespecs.size(); i++) {
                if (namespecs.get(i).equivspec != null) {
                    t.get(i).setEquivSpec(namespecs.get(i).equivspec);
                }
            }
            return t;
        } else return null;
    }
}
