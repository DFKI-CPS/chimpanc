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
public class MostSimilarSElementSubsetMapping extends MostSimilarSubsetMapping<ISElement<?>> {
    private List<ISElement<?>> X;
    private List<ISElement<?>> Y;
    private STool ST;

    public MostSimilarSElementSubsetMapping() {}

    public MostSimilarSElementSubsetMapping(STool st,List<ISElement<?>> l, List<ISElement<?>> r) {
        super();
        X = l;
        Y = r;
        ST = st;
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
    public void setX(List<ISElement<?>> xl) {
        X = xl;
    }

    @Override
    public void setY(List<ISElement<?>> yl) {
        Y = yl;
    }

    @Override
    public void initFrom(MostSimilarSubsetMapping<ISElement<?>> other) {
        if (other instanceof MostSimilarSElementSubsetMapping) {
            ST = ((MostSimilarSElementSubsetMapping) other).ST;
        }
    }


    @Override
    protected ISElement<?> valueOfX(int index) {
        return X.get(index);
    }

    @Override
    protected ISElement<?> valueOfY(int index) {
        return Y.get(index);
    }

    @Override
    protected double similarity(ISElement<?> x1, ISElement<?> y1) {
        return ST.similarity(x1,y1);
    }

    @Override
    protected Boolean equals(ISElement<?> x1, ISElement<?> y1) {
        return (x1.getObject().equals(y1.getObject()));
    }
}
