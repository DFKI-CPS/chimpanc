/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.antlr.runtime.tree.Tree;
import org.dfki.stools.ISElement;
import org.dfki.stools.SAnnotation;
import org.dfki.utils.Collectionxx;
import org.dfki.utils.Test;

import java.util.ArrayList;
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
// Similarity Specifications for Annotations consist of
//  (1) annotations that must be absent
//  (2) annotations that should be checked for equality, if present
//  (3) annotations that are mandatory, i.e. they must be present otherwise
//      the check should fail
//
// ************************************************************

public class AnnotationSimilaritySpec extends AnnotationOrSubElementSimilaritySpec {
    public AnnotationSpecs absent;
    public AnnotationSpecs mandatory;
    public AnnotationSpecs checkannotations;

    public AnnotationSimilaritySpec() {
        this.mandatory = new AnnotationSpecs();
        this.absent = new AnnotationSpecs();
        this.checkannotations = new AnnotationSpecs();
    }

    public AnnotationSimilaritySpec(AnnotationSpecs checkannotations,
                                    AnnotationSpecs mandatory,
                                    AnnotationSpecs absent) {
        this.mandatory = mandatory;
        this.absent = absent;
        this.checkannotations = checkannotations;
    }

    public AnnotationSimilaritySpec(Tree tree, SimilaritySpec spec) {
        this.mandatory = new AnnotationSpecs();
        this.absent = new AnnotationSpecs();
        this.checkannotations = new AnnotationSpecs();
        if (tree.getType()== ANNOTATIONS) {
           for(int i=0;i<tree.getChildCount();i++) {
               Tree child = tree.getChild(i);
               switch (child.getType()) {
                   case ALLBUT:
                       checkannotations.add(new AllButAnnotationSpec(child,spec));
                       break;
                   case ABSENT:
                       for(int c=0;c<child.getChildCount();c++) {
                           absent.add(parseAnnoQname(child.getChild(c),spec));
                       }
                       break;
                   case PREFIXEDNAME:
                   case UNPREFIXEDNAME:
                       checkannotations.add(parseAnnoQname(child,spec));
                       break;
                   case OPTIONAL:
                       checkannotations.add(parseAnnoQname(child.getChild(0), spec));
                       break;
                   case MANDATORY:
                       mandatory.add(parseAnnoQname(child.getChild(0),spec));
                       break;
               }
           }
        }
    }

    public static AnnotationNameSpec parseAnnoQname (Tree c, SimilaritySpec spec) {
        AnnotationNameSpec res = null;
        switch (c.getType()) {
            case UNPREFIXEDNAME:
                res = new AnnotationNameSpec("",c.getChild(0).getText(),spec);
                break;
            case PREFIXEDNAME:
                res = new AnnotationNameSpec(c.getChild(0).getText(),c.getChild(1).getText(),spec);
                break;
        }
        return res;
    }

    public AnnotationSimilaritySpec expand(ISElement a, ISElement b,SimilaritySpec spec) {
        AnnotationSpecs newabsent = new AnnotationSpecs();
        for (AnnotationSpec as : absent) {
            if (as instanceof AnnotationNameSpec) {
                newabsent.insert((AnnotationNameSpec) as);
            } else if (as instanceof AllButAnnotationSpec) {
                newabsent.insertAll(((AllButAnnotationSpec) as).eval(a,spec));
                newabsent.insertAll(((AllButAnnotationSpec) as).eval(b,spec));
            }
        }
        AnnotationSpecs newmandatory = new AnnotationSpecs();
        for (AnnotationSpec as : mandatory) {
            if (as instanceof AnnotationNameSpec) {
                newmandatory.insert((AnnotationNameSpec) as);
            } else if (as instanceof AllButAnnotationSpec) {
                newmandatory.insertAll(((AllButAnnotationSpec) as).eval(a,spec));
                newmandatory.insertAll(((AllButAnnotationSpec) as).eval(b,spec));
            }
        }
        AnnotationSpecs newcheckannotations = new AnnotationSpecs();
        for (AnnotationSpec as : checkannotations) {
            if (as instanceof AnnotationNameSpec) {
                newcheckannotations.insert((AnnotationNameSpec) as);
            } else if (as instanceof AllButAnnotationSpec) {
                newcheckannotations.insertAll(((AllButAnnotationSpec) as).eval(a,spec));
                newcheckannotations.insertAll(((AllButAnnotationSpec) as).eval(b,spec));
            }
        }
        return new AnnotationSimilaritySpec(newcheckannotations, newmandatory, newabsent);
    }

    public List<SAnnotation<?>> select(ISElement<?> a) {
        ArrayList<SAnnotation<?>> res = new ArrayList<SAnnotation<?>>();
        for (AnnotationSpec as : checkannotations) {
            if (as instanceof AnnotationNameSpec) {
                AnnotationNameSpec ans = (AnnotationNameSpec) as;
                if (a.hasAnnotation(ans.getNameSpace(), ans.getName()))
                    res.add((SAnnotation<?>) a.getAnnotation(ans.getNameSpace(), ans.getName()));
            }
        }
        for (AnnotationSpec as : mandatory) {
            if (as instanceof AnnotationNameSpec) {
                AnnotationNameSpec ans = (AnnotationNameSpec) as;
                if (a.hasAnnotation(ans.getNameSpace(), ans.getName()) &&
                        !Collectionxx.some(res, new Test(ans) {
                            @Override
                            public Boolean check(Object o) {
                                AnnotationNameSpec orig = ((AnnotationNameSpec) this.base);
                                SAnnotation<?> sa = (SAnnotation<?>) o;
                                return (orig.getNameSpace().equals(sa.getNameSpace())
                                        && orig.getName().equals(sa.getName()));
                            }
                        }))
                    res.add((SAnnotation<?>) a.getAnnotation(ans.getNameSpace(), ans.getName()));
            }
        }
        return res;
    }

    private Boolean isValid(AnnotationSpecs as) {
        ArrayList<String> names = new ArrayList<String>();
        for (AnnotationSpec a : as) {
            if (!a.isValid()) return false;
            else names.addAll(a.getAllAnnotationNames());
        }
        return !Collectionxx.checkDuplicate(names);
    }

    private ArrayList<String> getAllAnnotationNames(AnnotationSpecs as) {
        ArrayList<String> names = new ArrayList<String>();
        for (AnnotationSpec a : as) {
            names.addAll(a.getAllAnnotationNames());
        }
        return names;
    }

    public ArrayList<String> getAllAnnotationNames() {
        ArrayList<String> res = getAllAnnotationNames(mandatory);
        for (Object o : new Object[]{absent, checkannotations}) {
            AnnotationSpecs el = (AnnotationSpecs) o;
            for (AnnotationSpec e : el) {
                for (String name : e.getAllAnnotationNames()) {
                    if (!res.contains(name)) res.add(name);
                }
            }
        }
        return res;
    }

    @Override
    public Boolean isValid() {
        if (!isValid(mandatory) || !isValid(checkannotations) || !isValid(absent)) {
            return false;
        } else {
            // absent and mandatory must be disjoint as well as absent and checkannotations
            if (!Collectionxx.disjoint(getAllAnnotationNames(absent), getAllAnnotationNames(mandatory))) {
                return false;
            }
            if (!Collectionxx.disjoint(getAllAnnotationNames(absent), getAllAnnotationNames(checkannotations))) {
                return false;
            } else return true;
        }
    }

    public String toString() {
        String res = "annotations {";
        Object[] o = new Object[]{null, checkannotations, "absent", absent};
        Boolean first = true;
        for (int i = 0; i < 3; i = i + 2) {
            String name = (String) o[i];
            AnnotationSpecs spec = (AnnotationSpecs) o[i + 1];
            if (spec.size() > 0) {
                if (first) first = false;
                else res = res + ",\n";
                if (name != null) {
                    res = res + " " + name + " {";
                }
                Boolean tempfirst = true;
                for (AnnotationSpec a : spec) {
                    if (tempfirst) {
                        res = res + a.toString();
                        tempfirst = false;
                    } else res = res + ", " + a.toString();
                    if (mandatory.contains(a)) res = res + "!";
                }
                if (name != null) {
                    res = res + "}";
                }
            }
        }
        res = res + "}";
        return res;
    }
}
