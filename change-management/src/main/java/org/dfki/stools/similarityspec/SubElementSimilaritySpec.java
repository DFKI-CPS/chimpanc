/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.antlr.runtime.tree.Tree;
import org.dfki.stools.ISElement;
import org.dfki.utils.Collectionxx;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static org.dfki.stools.SimilaritySpecLexer.*;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 16.02.2011
 * Time: 13:34
 * To change this template use File | Settings | File Templates.
 */


// ************************************************************
//
// Similarity Specifications for Elements consist of
//  (1) Elements or Tuples that must be absent
//  (2) Elements or Tuples that should be checked for equality and ordered, if present
//  (3) Elements or Tuples that should be checked for equality and unordered, if present
//  (4) Elements or Tuples that are mandatory, i.e. they must be present otherwise
//      the check should fail
//  (5) Elements or Tuples that are a limit, that is any change inside them causes the whole
//      Element or Tuple to be replaced
// ************************************************************

public class SubElementSimilaritySpec extends AnnotationOrSubElementSimilaritySpec {
    public ElementSpecs absent;
    public ElementSpecs mandatory;
    public ElementSpecs optional;
    public ElementSpecs ordered;
    public ElementSpecs unordered;
    public ElementSpecs limit;


    public SubElementSimilaritySpec() {
        absent = new ElementSpecs();
        mandatory = new ElementSpecs();
        optional = new ElementSpecs();
        ordered = new ElementSpecs();
        unordered = new ElementSpecs();
        limit = new ElementSpecs();
    }

    public SubElementSimilaritySpec(ElementSpecs ordered,
                                    ElementSpecs unordered,
                                    ElementSpecs mandatory,
                                    ElementSpecs absent,
                                    ElementSpecs limit) {
        this.mandatory = mandatory;
        this.absent = absent;
        this.ordered = ordered;
        this.unordered = unordered;
        this.limit = limit;
    }

    public SubElementSimilaritySpec(Tree tree, SimilaritySpec spec) {
        absent = new ElementSpecs();
        mandatory = new ElementSpecs();
        optional = new ElementSpecs();
        ordered = new ElementSpecs();
        unordered = new ElementSpecs();
        limit = new ElementSpecs();
        for (int i = 0; i < tree.getChildCount(); i++) {
            Tree child = tree.getChild(i);
            switch (child.getType()) {
                case ABSENT:
                    //System.err.println("ABSENT Childs: "+child.getChildCount());
                    for (int c = 0; c < child.getChildCount(); c++) {
                        absent.add(parseElementSpec(child.getChild(c), spec));
                    }
                    //System.err.println("Parsed absent to "+absent);
                    break;
                case ORDERED:
                    for (int c = 0; c < child.getChildCount(); c++) {
                        ordered.add(parseElementSpec(child.getChild(c), spec));
                    }
                    break;
                case UNORDERED:
                    for (int c = 0; c < child.getChildCount(); c++) {
                        unordered.add(parseElementSpec(child.getChild(c), spec));
                    }
                    break;
                case MANDATORY:
                    for (int c = 0; c < child.getChildCount(); c++) {
                        mandatory.add(parseElementSpec(child.getChild(c), spec));
                    }
                    break;
                case LIMIT:
                    for (int c = 0; c < child.getChildCount(); c++) {
                        limit.add(parseElementSpec(child.getChild(c), spec));
                    }
                    break;
            }
        }
    }

    public static ElementSpec parseElementSpec(Tree tree, SimilaritySpec spec) {
        ElementSpec result = null;
        switch (tree.getType()) {
            case ALLBUT:
                result = new AllButElementSpec(tree, spec);
                break;
            case TUPLE:
                result = new ElementTupleSpec(tree, spec);
                break;
            case EQUIVELEM:
                result = parseElementSpec(tree.getChild(0), spec);
                ((ElementNameSpec) result).equivspec = tree.getChild(1).getText();
                break;
            case PREFIXEDNAME:
                String uri = spec.namespaceContext().getNamespaceURI(tree.getChild(0).getText());
                //System.err.println("Setting namesapecuri "+uri);
                result = new ElementNameSpec(uri, tree.getChild(1).getText(), null, spec);
                break;
            case UNPREFIXEDNAME:
                result = new ElementNameSpec("", tree.getChild(0).getText(), null, spec);
                break;
            case TEXTNODE:
            case COMMENTNODE:
                result = new ElementNameSpec("", tree.getText(), null, spec);
                break;
            case SPLITNODE:
                result = new ElementNameSpec("", "<SPLIT(" + tree.getChild(0).getText() + ")>", null, spec);
                break;

        }
        return result;
    }

    @Override
    public String toString() {
        String res = null;
        if (!mandatory.isEmpty()) {
            if (res == null) res = "{mandatory{";
            else res = res + ", mandatory{";
            Boolean first = true;
            for (ElementSpec s : mandatory) {
                if (!first) {
                    res = res + ", ";
                }
                first = false;
                res = res + s.toString();
            }
            res = res + "}";
        }
        if (!absent.isEmpty()) {
            if (res == null) res = "{absent{";
            else res = res + ", absent{";
            Boolean first = true;
            for (ElementSpec s : absent) {
                if (!first) {
                    res = res + ", ";
                }
                first = false;
                res = res + s.toString();
            }
            res = res + "}";
        }
        if (!ordered.isEmpty()) {

            if (res == null) res = "{ordered{";
            else res = res + ", ordered{";
            Boolean first = true;
            for (ElementSpec s : ordered) {
                if (!first) res = res + ", ";
                else first = false;
                res = res + s.toString();
            }
            res = res + "}";
        }

        if (!unordered.isEmpty())

        {
            if (res == null) res = "{unordered{";
            else res = res + ", unordered{";
            Boolean first = true;
            for (ElementSpec s : unordered) {
                if (!first) res = res + ", ";
                else first = false;
                res = res + s.toString();
            }
            res = res + "}";
        }

        if (!limit.isEmpty())

        {
            if (res == null) res = "{limit{";
            else res = res + ", limit{";
            Boolean first = true;
            for (ElementSpec s : limit) {
                if (!first) res = res + ", ";
                else first = false;
                res = res + s.toString();
            }
            res = res + "}";
        }

        if (res != null) res = "constituents " + res + "}";
        else res = "";
        return res;
    }

    public SubElementSimilaritySpec expand(String equivspec, SimilaritySpec spec, List<ISElement<?>>... lists) {
        // Absent
        ElementSpecs newabsent = new ElementSpecs();
        for (ElementSpec es : absent) {
            if (es instanceof ElementNameOrTupleSpec) newabsent.insert((ElementNameOrTupleSpec) es);
        }
        for (ElementSpec es : absent) {
            if (es instanceof AllButElementSpec) {
                AllButElementSpec abes = (AllButElementSpec) es;
                for(List<ISElement<?>> al:lists) {
                    newabsent.insertAll(abes.eval(al, equivspec, spec));
                }
            }
        }
        // Mandatory
        ElementSpecs newmandatory = new ElementSpecs();
        for (ElementSpec es : mandatory) {
            if (es instanceof ElementNameOrTupleSpec) newmandatory.insert((ElementNameOrTupleSpec) es);
        }
        for (ElementSpec es : mandatory) {
            if (es instanceof AllButElementSpec) {
                AllButElementSpec abes = (AllButElementSpec) es;
                for(List<ISElement<?>> al:lists) {
                    newmandatory.insertAll(abes.eval(al, equivspec, spec));
                }
            }
        }
        List<ElementNameSpec> collectors = new ArrayList<ElementNameSpec>();

        // Ordered
        ElementSpecs newordered = new ElementSpecs();
        for (ElementSpec es : ordered) {
            if (es instanceof ElementNameOrTupleSpec) {
                newordered.insert((ElementNameOrTupleSpec) es);
                if (es instanceof ElementNameSpec) collectors.add((ElementNameSpec) es);
                else {
                    collectors.addAll(((ElementTupleSpec) es).namespecs);
                }
            }
        }
        for (ElementSpec es : ordered) {
            if (es instanceof AllButElementSpec) {
                AllButElementSpec abes = (AllButElementSpec) es;
                for(List<ISElement<?>> al:lists) {
                    List<ElementNameSpec> temp = abes.eval(al, equivspec, spec);
                    newordered.insertAll(temp);
                    collectors.addAll(temp);
                }
            }
        }


        // Unordered
        ElementSpecs newunordered = new ElementSpecs();
        for (ElementSpec es : unordered) {
            if (es instanceof ElementNameOrTupleSpec) {
                newunordered.insert((ElementNameOrTupleSpec) es);
                if (es instanceof ElementNameSpec) collectors.add((ElementNameSpec) es);
                else {
                    collectors.addAll(((ElementTupleSpec) es).namespecs);
                }
            }
        }
        for (ElementSpec es : unordered) {
            if (es instanceof AllButElementSpec) {
                AllButElementSpec abes = (AllButElementSpec) es;
                for(List<ISElement<?>> al:lists) {
                    List<ElementNameSpec> temp = abes.eval(al, equivspec, spec);
                    newunordered.insertAll(temp);
                    collectors.addAll(temp);
                }
            }
        }


        // Finalizing ordered with missing elements;

        ElementSpecs newo = new ElementSpecs();

        AllButElementSpec abesa = new AllButElementSpec(collectors);

        for(List<ISElement<?>> al:lists) {
            List<ElementNameSpec> tempa = abesa.eval(al, equivspec, spec);
            //System.err.println("tempa = "+tempa);
            newo.insertAll(tempa);
        }

        newordered.addAll(newo);

        // Limit
        ElementSpecs newlimit = new ElementSpecs();
        for (ElementSpec es : limit) {
            if (es instanceof ElementNameOrTupleSpec) newlimit.insert((ElementNameOrTupleSpec) es);
        }
        for (ElementSpec es : limit) {
            if (es instanceof AllButElementSpec) {
                AllButElementSpec abes = (AllButElementSpec) es;
                for(List<ISElement<?>> al:lists) {
                    List<ElementNameSpec> temp = abes.eval(al, equivspec, spec);
                    newlimit.insertAll(temp);
                }
            }
        }
        return new SubElementSimilaritySpec(newordered, newunordered, newmandatory,
                newabsent, newlimit);
    }

    public void setSubElementEquivSpec(ISElement e, @NotNull String parentequivspec) {
        // If we come here, then we have'nt found it in one the lists and we assign the parent's equivspec
        e.setEquivSpec(getSubElementEquivSpec(e, parentequivspec));
    }

    public String getSubElementEquivSpec(ISElement e, String parentequivspec) {
        String result = null;

        Iterator<ElementSpec> it = Collectionxx.append(ordered, unordered).iterator();
        if (!it.hasNext()) result = parentequivspec;
        else {
            while (it.hasNext() && result == null) {
                ElementSpec es = it.next();
                if (es instanceof ElementNameSpec) {
                    ElementNameSpec ens = (ElementNameSpec) es;
                    if (ens.getNamespace().equals(e.getNamespace()) &&
                            ens.getName().equals(e.getType())) {
                        if (!ens.getEquivSpec().isEmpty()) result = ens.getEquivSpec();
                        else result = parentequivspec;
                    }
                } else if (es instanceof ElementTupleSpec) {
                    for (ElementNameSpec ens : ((ElementTupleSpec) es).namespecs) {
                        if (ens.getNamespace().equals(e.getNamespace()) &&
                                ens.getName().equals(e.getType())) {
                            if (!ens.getEquivSpec().isEmpty()) result = ens.getEquivSpec();
                            else result = parentequivspec;
                        }
                    }
                }
            }
        }
        if (result==null) return parentequivspec;
        else return result;
    }


    private Boolean isValid(ElementSpecs as) {
        ArrayList<String> names = new ArrayList<String>();
        for (ElementSpec a : as) {
            if (!a.isValid()) return false;
            else names.addAll(a.getAllTagNames());
        }
        return !Collectionxx.checkDuplicate(names);
    }

    private ArrayList<String> getAllTagNames(ElementSpecs as) {
        ArrayList<String> names = new ArrayList<String>();
        for (ElementSpec a : as) {
            names.addAll(a.getAllTagNames());
        }
        return names;
    }

    public ArrayList<String> getAllTagNames() {
        ArrayList<String> res = getAllTagNames(mandatory);
        for (Object o : new Object[]{absent, ordered, unordered, limit}) {
            ElementSpecs el = (ElementSpecs) o;
            for (ElementSpec e : el) {
                for (String name : e.getAllTagNames()) {
                    if (!res.contains(name)) res.add(name);
                }
            }
        }
        return res;
    }

    @Override
    public Boolean isValid() {
        if (!(isValid(absent) && isValid(mandatory) && isValid(limit))) {
            return false;
        } else {

            // absent and mandatory must be disjoint as well as pairwise, absent and ordered, and
            // absent and unordered, and ordered and unordered
            if (!Collectionxx.disjoint(getAllTagNames(absent), getAllTagNames(mandatory))) {
                return false;
            }
            if (!isValid(ordered)) return false;
            if (!Collectionxx.disjoint(getAllTagNames(absent), getAllTagNames(ordered))) {
                return false;
            }

            if (!isValid(unordered)) return false;
            if (!Collectionxx.disjoint(getAllTagNames(absent), getAllTagNames(unordered))) {
                return false;
            }

            if (!Collectionxx.disjoint(getAllTagNames(unordered), getAllTagNames(ordered))) {
                return false;
            }
            return true;
        }


    }
}
