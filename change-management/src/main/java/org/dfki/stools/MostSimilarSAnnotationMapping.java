/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools;

import java.util.List;

import org.dfki.utils.MostSimilarSubsetMapping;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 14.02.2011
 * Time: 11:31
 * To change this template use File | Settings | File Templates.
 */
public class MostSimilarSAnnotationMapping extends MostSimilarSubsetMapping<SAnnotation<?>> {
    private List<SAnnotation<?>> X;
    private List<SAnnotation<?>> Y;
    private STools manager;

    public MostSimilarSAnnotationMapping () {}

    public MostSimilarSAnnotationMapping(STools m,List<SAnnotation<?>> l, List<SAnnotation<?>> r) {
        super();
        X = l;
        Y = r;
        manager = m;
    }

    @Override
    public void setX(List<SAnnotation<?>> xl) {
        X =xl;
    }

    @Override
    public void setY(List<SAnnotation<?>> yl) {
        Y = yl;
    }

    @Override
    public void initFrom(MostSimilarSubsetMapping<SAnnotation<?>> other) {
        if (other instanceof MostSimilarSAnnotationMapping) {
            manager = ((MostSimilarSAnnotationMapping) other).manager;
        }
    }

    @Override
    protected int sizeOfX() {
        return X.size();
    }

    @Override
    protected int sizeOfY() {
        return Y.size();
    }

    @Override
    protected SAnnotation valueOfX(int index) {
        return X.get(index);
    }

    @Override
    protected SAnnotation valueOfY(int index) {
        return Y.get(index);
    }

    @Override
    protected double similarity(SAnnotation x1, SAnnotation y1) {
        if (x1.getNameSpace().equals(y1.getNameSpace())
                && x1.getName().equals(y1.getName())
                && x1.getValue().equals(y1.getValue())) return 1.0;
        else return 0.0;
    }


    @Override
    protected Boolean equals(SAnnotation x1, SAnnotation y1) {
        return (x1.getNameSpace().equals(y1.getNameSpace()) &&
                x1.getName().equals(y1.getName()) &&
                x1.getValue().equals(y1.getValue()));
    }

}
