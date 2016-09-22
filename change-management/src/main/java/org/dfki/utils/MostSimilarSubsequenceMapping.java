/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2010.
 */

package org.dfki.utils;

import scala.Tuple2;
import scala.Tuple3;

import java.text.DecimalFormat;
import java.util.*;

import static java.lang.Math.abs;


/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: Nov 3, 2010
 * Time: 12:57:49 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * This abstract class implements the well known algorithm to compute the most similar subsequence between two lists X
 * and Y. Both lists contain elements of type <VALUE>.
 * @param <VALUE> type of the elements for the lists
 *                for the edit scripts, there are four types: ADD("+", "add"), REMOVE("-", "remove"), NONE(" ", "none"), UPDATE("U", "update");
 *                instead of the classical edit distance problem, where we minimize the edit distance, we try to maximize the similarity.
 */

public abstract class MostSimilarSubsequenceMapping<VALUE> {
    /* solution matrix; The ith row and jth column shows the length of the LCS between X[1..i] and Y[1..j].*/
    private MatrixEntry[][] c;
    /* a mapping that stores similar values that occur in both mappings (i.e., are not deleted and inserted) */
    HashMap<Integer, Integer> mapping;
    /* contains the edit script as a list of ADD/DELETE/UPDATE/NONE <VALUE> */
    private ArrayList<DiffEntry<VALUE>> diff;
    /* the values/subsequence of X that is preserved (not deleted) */
    private ArrayList<VALUE> backtrackX;
    /* the values/subsequence of Y that is preserved (not deleted, not added) */
    private ArrayList<VALUE> backtrackY;

    /* Contains the fields that are marked and that must occur in the subsequence */
    HashMap<Integer, Integer> marked = new HashMap<Integer, Integer>();

    /**
     * A constructor for classes inheriting this one, allowing them to
     * do some initialization before setting the values of X and Y.  Once
     * the initialization is complete, the inheriting class must call
     * initValues(VALUE[] x, VALUE[] y)
     */

    /* abstract setter methods */
    public abstract void setX(List<VALUE> xl);

    public abstract void setY(List<VALUE> yl);

    public abstract void initFrom(MostSimilarSubsequenceMapping<VALUE> other);

    /* getter methods */

    public List<VALUE> getX() {
        List<VALUE> r = new ArrayList<VALUE>(lengthOfX());
        for (int i = 0; i < lengthOfX(); i++) r.add(valueOfX(i));
        return r;
    }

    public List<VALUE> getY() {
        List<VALUE> r = new ArrayList<VALUE>(lengthOfY());
        for (int i = 0; i < lengthOfY(); i++) r.add(valueOfY(i));
        return r;
    }

    /* length of the first list X */
    protected abstract int lengthOfX();

    /* length of the second list Y */
    protected abstract int lengthOfY();

    /* returns the specified element from the first list X */
    protected abstract VALUE valueOfX(int index);

    /* returns the specified element from the second list Y */
    protected abstract VALUE valueOfY(int index);

    /* computes the similarity between two objects of type VALUE */
    protected abstract double similarity(VALUE x1, VALUE y1);

    /* checks whether two objects of type VALUE are equal */
    protected abstract Boolean equals(VALUE x1, VALUE y1);

    /* returns the similarity between X[i] and Y[j] */
    private double XYSimilarity(int i, int j) {
        return similarity(valueOfXInternal(i), valueOfYInternal(j));
    }

    /* checks whether X[i] and Y[j] are equal */
    private Boolean XYequals(int i, int j) {
        return equals(valueOfXInternal(i), valueOfYInternal(j));
    }

    /*
    * */
    private VALUE valueOfXInternal(int i) {
        return valueOfX(i - 1);
    }

    private VALUE valueOfYInternal(int j) {
        return valueOfY(j - 1);
    }

    /**
     * computes all solutions of all subproblems. If the solutions are already computed in c, the computation is not
     * performed again.
     */
    public void calculateMSM() {
        if (c != null) {
            return;
        }

        // step 1: generate the array that holds the solutions of the subproblems
        // fill the first row and the first column for which the solution is known

        c = new MatrixEntry[lengthOfX() + 1][];
        for (int i = 0; i < c.length; i++) {
            c[i] = new MatrixEntry[lengthOfY() + 1];
            if (i == 0) c[i][0] = new MatrixEntry(Direction.init, 1.0, 0);
            else c[i][0] = new MatrixEntry(Direction.init, 0.0, 0);
            if (i == 0) {
                for (int j = 0; j < lengthOfY() + 1; j++) c[i][j] = new MatrixEntry(Direction.init, 0.0, 0);
            }
        }

        // stepwise extend the solution to longer subproblems
        for (int i = 1; i < c.length; i++) {
            for (int j = 1; j < c[i].length; j++) {
                Double XYsimilarity = XYSimilarity(i, j);
                Double northwestsimilarity = XYsimilarity + c[i - 1][j - 1].similarity;
                Double northsimilarity = c[i][j - 1].similarity;
                Double westsimilarity = c[i - 1][j].similarity;
                if (northwestsimilarity > westsimilarity && northwestsimilarity > northsimilarity) {
                    if (XYsimilarity > 0)
                        c[i][j] = new MatrixEntry(Direction.northwest, northwestsimilarity, c[i - 1][j - 1].length + 1);
                    else
                        c[i][j] = new MatrixEntry(Direction.northwest, northwestsimilarity, c[i - 1][j - 1].length);
                } else if (westsimilarity > northsimilarity) {
                    c[i][j] = new MatrixEntry(Direction.west, westsimilarity, c[i - 1][j].length);
                } else c[i][j] = new MatrixEntry(Direction.north, northsimilarity, c[i][j - 1].length);
            }
        }
    }


    /**
     * returns the length of the most similar subsequence of X and Y
     * @return
     */
    public int getMSMLength() {
        calculateMSM();
        return c[lengthOfX()][lengthOfY()].length;
    }

    /**
     * returns the similarity measure between X and Y
     * @return
     */
    public Double getMSMSimilarity() {
        calculateMSM();
        return c[lengthOfX()][lengthOfY()].similarity;
    }

    /**
     * returns the minimal edit distance (where the costs for insert and deletion are 1)
     * @return
     */
    public int getMinEditDistance() {
        calculateMSM();
        return lengthOfX() + lengthOfY() - 2 * abs(getMSMLength());
    }

    /**
     * analyzes the resulting matrix to compute the optimal solution; this is done by starting
     * at the rightmost lowermost entry and working backwards using the internal function backtrack(x,y)
     * @return
     */
    public HashMap<Integer, Integer> backtrack() {
        calculateMSM();
        if (this.mapping == null) {
            this.mapping = new HashMap<Integer, Integer>();
            this.diff = new ArrayList<DiffEntry<VALUE>>();
            this.backtrackX = new ArrayList<VALUE>();
            this.backtrackY = new ArrayList<VALUE>();

            backtrack(lengthOfX(), lengthOfY());
            Collections.reverse(diff);


            for (int i = 0; i < lengthOfX(); i++) {
                if (mapping.containsKey(i)) {
                    backtrackX.add(valueOfX(i));
                    backtrackY.add(valueOfY(mapping.get(i)));
                }
            }
        }
        return this.mapping;
    }

    /** returns the subsequence of X that is preserved */
    public List<VALUE> getBacktrackX() {
        backtrack();
        return backtrackX;
    }

    /**
     * returns the subsequence of X that is preserved
     * @return
     */
    public List<VALUE> getBacktrackY() {
        backtrack();
        return backtrackY;
    }

    /**
     * returns the subsequence of Y that is preserved
     * @return
     */
    public List<DiffEntry<VALUE>> getDiff() {
        backtrack();
        return diff;
    }

    /**
     * computes the solution for the entry i,j in the solution matrix and updates the mapping and the diff accordingly.
     * @param i row of the solution matrix to be analyzed
     * @param j column of the solution matrix to be analyzed
     */
    private void backtrack(int i, int j) {
        calculateMSM();

        if (i == 0 || j == 0) {
            if (i == 0 && j != 0) {
                for (int c = 1; c <= j; c++) diff.add(new DiffEntry(DiffType.ADD, valueOfYInternal(c)));
            } else if (i != 0 && j == 0) {
                for (int c = 1; c <= i; c++) diff.add(new DiffEntry(DiffType.REMOVE, valueOfXInternal(c)));
            }
            return;
        } else {
            switch (computeDirection(i, j)) {
                case northwest:
                    if (c[i][j].similarity > c[i - 1][j - 1].similarity) {
                        mapping.put(i - 1, j - 1);
                        if (XYequals(i, j)) diff.add(new DiffEntry<VALUE>(DiffType.NONE, valueOfXInternal(i)));
                        else diff.add(new DiffEntry<VALUE>(DiffType.UPDATE, valueOfYInternal(j)));
                    }
                    backtrack(i - 1, j - 1);
                    break;
                case west:
                    diff.add(new DiffEntry(DiffType.REMOVE, valueOfXInternal(i)));
                    backtrack(i - 1, j);
                    break;
                case north:
                    diff.add(new DiffEntry(DiffType.ADD, valueOfYInternal(j)));
                    backtrack(i, j - 1);
                    break;
            }
        }
    }

    private Direction computeDirection(int i, int j) {
        Direction optimalDirection = c[i][j].direction;
        List<Direction> compatibleDirections = compatibleDirectionsToNextMarked(i, j);
        if (compatibleDirections.contains(optimalDirection)) return optimalDirection;
        else if (compatibleDirections.contains(Direction.north)) return Direction.north;
        else return Direction.west;
    }

    private List<Direction> compatibleDirectionsToNextMarked(int i, int j) {
        List<Direction> result = Collectionxx.newList(Direction.north, Direction.northwest, Direction.west);
        Tuple2<Integer, Integer> nextMarked = nextPrecedingPosition(i, j);
        /* if (nextMarked != null)
            System.out.println(String.format("Next marked (%s,%s)", nextMarked._1(), nextMarked._2()));
            */
        if (nextMarked == null || (nextMarked._1() < i - 1 && nextMarked._2() < j - 1)) {
            // It does not matter which direction we go.

        } else {

            if (nextMarked._1().equals(i - 1)) result.remove(Direction.west);
            if (nextMarked._2().equals(j - 1)) result.remove(Direction.north);
            if (result.size() == 2) result.remove(Direction.northwest);
        }
        //System.out.println(String.format("Compatible directions for (%s,%s) = %s", i, j, result));
        return result;
    }

    private Tuple2<Integer, Integer> nextPrecedingPosition(int i, int j) {
    	Tuple2<Integer, Integer> result = null;
        Integer lastdistance = null;
        for (Integer xpos : marked.keySet()) {
            if (xpos < i && marked.get(xpos) < j) {
                Integer dist = manhattanDistance(xpos, marked.get(xpos), i, j);
                if (lastdistance == null || dist < lastdistance) {
                    result = new Tuple2(xpos, marked.get(xpos));
                    lastdistance = dist;
                }
            }
        }
        return result;
    }

    private Integer manhattanDistance(Integer x1, Integer y1, int x2, int y2) {
        return Math.abs(x1 - x2) + Math.abs(y1 - y2);
    }

    public Map<Integer, Integer> getAllCellsWithMSMLength(Integer l) {
        return null;
    }

    private String fillup(String s) {
        int size = 8;
        for (int j = 1; j <= lengthOfY(); j++) {
            size = Math.max(size, valueOfYInternal(j).toString().length());
        }
        for (int j = 1; j <= lengthOfX(); j++) {
            size = Math.max(size, valueOfXInternal(j).toString().length());
        }
        String res = s;
        for (int j = 1; j <= size; j++) res = " " + res;
        return res.substring(res.length() - size, res.length());
    }

    public Boolean fixMapping(Map<Integer, Integer> mapXY) {
        marked = new HashMap<Integer, Integer>();
        for (Integer xpos : mapXY.keySet()) {
            marked.put(xpos, mapXY.get(xpos));
        }
        return true;
    }

    public List<Map.Entry<Integer, Integer>> getOrderedMapping() {
        List<Map.Entry<Integer, Integer>> result = new ArrayList<Map.Entry<Integer, Integer>>(mapping.size());
        result.addAll(mapping.entrySet());
        Collections.sort(result, new Comparator<Map.Entry<Integer, Integer>>() {
            @Override
            public int compare(Map.Entry<Integer, Integer> e1,
                               Map.Entry<Integer, Integer> e2) {
                if (e1.getKey() > e2.getKey()) return 1;
                else if (e1.getKey() < e2.getKey()) return -1;
                else return 0;
            }
        });
        return result;
    }

    public static Tuple2<Map<Integer, Integer>, Map<Integer, Integer>> computeMappingsFromCommonMapping
            (Map<Integer, Integer> mapOrigin2CommonAB, List<Map.Entry<Integer, Integer>> orderedMapAB) {

        Map<Integer, Integer> mappingOrigin2A = new HashMap<Integer, Integer>();
        Map<Integer, Integer> mappingOrigin2B = new HashMap<Integer, Integer>();
        for (Integer originpos : mapOrigin2CommonAB.keySet()) {
            Map.Entry<Integer, Integer> mapABentry = orderedMapAB.get(mapOrigin2CommonAB.get(originpos));
            mappingOrigin2A.put(originpos, mapABentry.getKey());
            mappingOrigin2B.put(originpos, mapABentry.getValue());
        }
        return new Tuple2<Map<Integer, Integer>, Map<Integer, Integer>>(mappingOrigin2A, mappingOrigin2B);
    }

    public List<DiffEntry<VALUE>> getDiff3(VALUE[] origin) {
        return getDiff3Alignment(origin).getDiff3();
    }

    public Alignment<VALUE> getDiff3Alignment(VALUE[] origin) {
        Alignment<VALUE> result = null;
        try {
            Class c = this.getClass();
            MostSimilarSubsequenceMapping<VALUE> MSSMOrigin2commonXY = (MostSimilarSubsequenceMapping<VALUE>) c.newInstance();
            MSSMOrigin2commonXY.initFrom(this);
            MostSimilarSubsequenceMapping<VALUE> MSSMOrigin2X = (MostSimilarSubsequenceMapping<VALUE>) c.newInstance();
            MSSMOrigin2X.initFrom(this);
            MostSimilarSubsequenceMapping<VALUE> MSSMOrigin2Y = (MostSimilarSubsequenceMapping<VALUE>) c.newInstance();
            MSSMOrigin2Y.initFrom(this);

            // 1. Determine longest common subsequence between X and Y
            //System.out.println("Class = "+this.getClass());
            //System.out.println("X = "+getX());
            //System.out.println("Y = "+getY());
            backtrack();
            //System.out.println(getBacktrackX());
            List<VALUE> commonXY = getBacktrackX();
            //System.out.println("CommonXY = "+commonXY);

            // 2. Determine LCS between origin and commonXY
            MSSMOrigin2commonXY.setX(Arrays.asList(origin));
            MSSMOrigin2commonXY.setY(commonXY);
            MSSMOrigin2commonXY.backtrack();
            //System.out.println(MSSMOrigin2commonXY);
            // 3. Compute mappings for marking object between Origin and X as well as Origin and Y
            Tuple2<Map<Integer, Integer>, Map<Integer, Integer>> maps =
                    MostSimilarSubsequenceMapping.computeMappingsFromCommonMapping(MSSMOrigin2commonXY.mapping, this.getOrderedMapping());
            //System.out.println("O->X: "+maps._1());
            //System.out.println("O->Y: " + maps._2());

            // 4. Compute LCS(O,X) containing LCS(O,LCS(XY));
            List<VALUE> X = getX();
            MSSMOrigin2X.setX(Arrays.asList(origin));
            MSSMOrigin2X.setY(X);
            MSSMOrigin2X.fixMapping(maps._1());
            MSSMOrigin2X.backtrack();
            //System.out.println(MSSMOrigin2X);

            // 5. Compute LCS(O,Y) containing LCS(O,LCS(XY));
            List<VALUE> Y = getY();
            MSSMOrigin2Y.setX(Arrays.asList(origin));
            MSSMOrigin2Y.setY(Y);
            MSSMOrigin2Y.fixMapping(maps._2());
            MSSMOrigin2Y.backtrack();

            // 6. Compute the alignment between O, X and Y using the mappings Origin2X, Origin2Y and X2Y
            Alignment<VALUE> m = new Alignment<VALUE>(Arrays.asList(origin), X, Y, MSSMOrigin2X.mapping, MSSMOrigin2Y.mapping, this.mapping, this);
            m.compute();
            //System.out.println(m);

            result = m;

        } catch (InstantiationException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IllegalAccessException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return result;
    }

    @Override
    public String toString() {
        calculateMSM();

        StringBuffer buf = new StringBuffer();
        buf.append(fillup("") + "|" + fillup("") + " ");
        for (int j = 1; j <= lengthOfY(); j++) {
            buf.append(fillup(valueOfYInternal(j).toString()) + " ");
        }

        buf.append("\n");
        int size = buf.length();
        for (int j = 1; j < size; j++) buf.append("-");
        buf.append("\n");

        buf.append(fillup("") + "|");
        DecimalFormat df = new DecimalFormat("###.##");

        for (int j = 0; j < c[0].length; j++) {
            buf.append(fillup(df.format(c[0][j].similarity)) + " ");
        }
        buf.append("\n");
        for (int i = 1; i < c.length; i++) {
            buf.append(fillup(valueOfXInternal(i).toString()) + "|");
            for (int j = 0; j < c[i].length; j++) {
                buf.append(fillup((mapping.containsKey(i - 1) && mapping.get(i - 1) == j - 1 ? "*" : "") + df.format(c[i][j].similarity)
                        + c[i][j].direction) + " ");
            }
            buf.append("\n");
        }
        buf.append("X = ");
        for (int i = 1; i <= lengthOfX(); i++) buf.append(fillup(valueOfXInternal(i).toString()) + " ");
        buf.append("\nY = ");
        for (int i = 1; i <= lengthOfY(); i++) buf.append(fillup(valueOfYInternal(i).toString()) + " ");
        buf.append("\n");
        buf.append("MSM(" + getMSMLength() + ") X = " + backtrackX + "\n");
        buf.append("MSM(" + getMSMLength() + ") Y = " + backtrackY + "\n");
        buf.append("Diff: " + diff.toString() + "\n");
        buf.append("Min edit distance = " + getMinEditDistance() + "\n");

        return buf.toString();
    }

    public static enum Direction {
        north, west, northwest, init, south, east, southeast;

        public String toString() {
            switch (this) {
                case west:
                    return "^";
                case north:
                    return "<";
                case northwest:
                    return "NW";
                case init:
                    return "-";
                default:
                    return "?";
            }
        }

        public Direction next() {
            if (this.equals(north)) return south;
            if (this.equals(west)) return east;
            if (this.equals(northwest)) return southeast;
            return null;
        }
    }

    public static class MatrixEntry {
        public Direction direction;

        public Double similarity;
        public int length;

        public MatrixEntry(Direction dir, Double sim, int l) {
            direction = dir;
            similarity = sim;
            length = l;
        }

        public String toString() {
            return similarity + "(" + direction + ")";
        }
    }

    public static enum DiffType {
        ADD("+", "add"),
        REMOVE("-", "remove"),
        NONE(" ", "none"),
        UPDATE("U", "update"),
        CONFLICT("C", "conflict");

        private String val;
        private String name;

        DiffType(String val, String name) {
            this.val = val;
            this.name = name;
        }

        @Override
        public String toString() {
            return val;
        }

        public String getName() {
            return name;
        }

        public String getVal() {
            return val;
        }
    }

    public static class DiffEntry<VALUE> {
        protected DiffType type;
        private VALUE value;
        private VALUE othervalue;
        protected Boolean twovalues;

        public DiffEntry() {
        }

        public DiffEntry(DiffType type, VALUE value) {
            this.type = type;
            this.value = value;
            this.othervalue = null;
            this.twovalues = false;
        }

        public DiffEntry(DiffType type, VALUE value, VALUE other) {
            this.type = type;
            this.value = value;
            this.othervalue = other;
            this.twovalues = true;
        }

        public DiffType getType() {
            return type;
        }

        public void setType(DiffType type) {
            this.type = type;
        }

        public VALUE getValue() {
            return value;
        }

        public void setValue(VALUE value) {
            this.value = value;
        }

        @Override
        public String toString() {
            if (twovalues) return String.format("%s(%s,%s)", type, value, othervalue);
            else return type.toString() + value;
        }

        public VALUE getOthervalue() {
            return othervalue;
        }

        public Boolean getTwovalues() {
            return twovalues;
        }
    }

    public static class ConflictEntry<VALUE> extends DiffEntry<VALUE> {
        protected List<VALUE> leftvalues;
        protected List<VALUE> rightvalues;

        public ConflictEntry(VALUE left, VALUE right) {
            this.type = DiffType.CONFLICT;
            this.twovalues = true;
            leftvalues = new ArrayList<VALUE>();
            rightvalues = new ArrayList<VALUE>();
            if (left != null) leftvalues.add(left);
            if (right != null) rightvalues.add(right);
        }

        public void addLeft(VALUE left) {
            leftvalues.add(left);
        }

        public void addRight(VALUE right) {
            rightvalues.add(right);
        }

        public List<VALUE> getLeftvalues() {
            return leftvalues;
        }

        public List<VALUE> getRightvalues() {
            return rightvalues;
        }

        @Override
        public String toString() {
            return String.format("%s(%s,%s)", this.type, leftvalues, rightvalues);
        }
    }

    public static class UpdateDeleteConflictEntry<VALUE> extends ConflictEntry<VALUE> {
        public UpdateDeleteConflictEntry(VALUE left, VALUE right) {
            super(left,right);
        }
        @Override
        public String toString() {
            return String.format("%s(%s,%s)", this.type, leftvalues, rightvalues);
        }
    }

    public static class ListConflictEntry<VALUE> extends ConflictEntry<VALUE> {
        public ListConflictEntry(VALUE left, VALUE right) {
            super(left,right);
        }
        @Override
        public String toString() {
            return String.format("%s<%s,%s>", this.type, leftvalues, rightvalues);
        }
    }

    public static class SetConflictEntry<VALUE> extends ConflictEntry<VALUE> {
        public SetConflictEntry(VALUE left, VALUE right) {
            super(left,right);
        }
        @Override
        public String toString() {
            return String.format("%s{%s,%s}", this.type, leftvalues, rightvalues);
        }
    }

    /**
     * Created by IntelliJ IDEA.
     * User: autexier
     * Date: Nov 3, 2010
     * Time: 2:13:31 PM
     * To change this template use File | Settings | File Templates.
     */

    static class Alignment<T extends Object> {
        T[] O;
        T[] A;
        T[] B;
        Map<Integer, Integer> mapOA;
        Map<Integer, Integer> mapOB;
        Map<Integer, Integer> mapAB;
        List<T>[] alignment;
        List<DiffEntry<T>> diff;
        MostSimilarSubsequenceMapping<T> mssm;

        public Alignment(List<T> o, List<T> a, List<T> b, Map<Integer, Integer> oa, Map<Integer, Integer> ob, Map<Integer, Integer> ab,
                         MostSimilarSubsequenceMapping<T> mssm) {
            O = (T[]) o.toArray();
            A = (T[]) a.toArray();
            B = (T[]) b.toArray();
            mapOA = oa;
            mapOB = ob;
            mapAB = ab;
            alignment = new ArrayList[]{new ArrayList<T>(), new ArrayList<T>(), new ArrayList<T>()};
            diff = null;
            this.mssm = mssm;
        }

        private void add(Integer o, Integer a, Integer b) {
            //System.out.println(String.format("ADD: %s %s %s",o,a,b));
            alignment[0].add((o != null) ? O[o] : null);
            alignment[1].add((a != null) ? A[a] : null);
            alignment[2].add((b != null) ? B[b] : null);
        }

        private Tuple3<T, T, T> get(Integer i) {
            return new Tuple3<T, T, T>(alignment[0].get(i), alignment[1].get(i), alignment[2].get(i));
        }

        public void compute() {
            arrange(0, 0, 0);
            extractDiff();
        }

        public List<DiffEntry<T>> getDiff3() {
            if (diff == null) {
                compute();
            }
            return diff;
        }

        private void arrange(Integer opos, Integer apos, Integer bpos) {
            Boolean stop = false;
            //System.out.println(String.format("arrange(%s, %s, %s)",opos,apos,bpos));
            if (opos < O.length && !mapOA.containsKey(opos) && !mapOB.containsKey(opos)) {
                add(opos, null, null);
                opos = opos + 1;
            } else if (apos < A.length
                    && (!mapOA.containsValue(apos) || getOrigin(mapOA, apos) < opos)
                    && !mapAB.containsKey(apos)) {
                add(null, apos, null);
                apos = apos + 1;
            } else if (bpos < B.length
                    && (!mapOB.containsValue(bpos) || getOrigin(mapOB, bpos) < opos)
                    && !mapAB.containsValue(bpos)) {
                add(null, null, bpos);
                bpos = bpos + 1;
            } else if (inMap(mapOA, opos, apos) && inMap(mapOB, opos, bpos) && inMap(mapAB, apos, bpos)) {
                add(opos, apos, bpos);
                opos = opos + 1;
                apos = apos + 1;
                bpos = bpos + 1;
            } else if (inMap(mapOA, opos, apos) && !mapAB.containsKey(apos)) {
                add(opos, apos, null);
                opos = opos + 1;
                apos = apos + 1;
            } else if (inMap(mapOB, opos, bpos) && !mapAB.containsValue(bpos)) {
                add(opos, null, bpos);
                opos = opos + 1;
                bpos = bpos + 1;
            } else if (inMap(mapAB, apos, bpos) && !mapOA.containsValue(apos) && !mapOB.containsValue(bpos)) {
                add(null, apos, bpos);
                apos = apos + 1;
                bpos = bpos + 1;
            } else if (!(opos == O.length && apos == A.length && bpos == B.length)) {
                System.err.println(String.format("No case matched for arrange(opos = %s,apos = %s, bpos = %s)\nmapAB = %s\nmapOA = %s\nmapOB = %s",
                        opos, apos, bpos, mapAB, mapOA, mapOB));
                stop = true;
            }

            if (!stop && (opos < O.length || apos < A.length || bpos < B.length)) {
                arrange(opos, apos, bpos);
            }
        }

        private Integer getOrigin(Map<Integer, Integer> m, Integer v) {
            Integer result = null;
            for (Integer key : m.keySet()) {
                if (m.get(key).equals(v)) result = key;
            }
            return result;
        }

        private void extractDiff() {
            diff = new ArrayList<DiffEntry<T>>();
            ArrayList<DiffEntry<T>> tempdiff = new ArrayList<DiffEntry<T>>();
            DiffEntry<T> last = null;
            for (int i = 0; i < alignment[0].size(); i++) {
                if (tempdiff.size() > 0) last = tempdiff.get(tempdiff.size() - 1);
                Tuple3<T, T, T> e = get(i);
                if (e._1() != null) {
                    if (e._2() == null || e._3() == null) {
                            if (e._2()!=null && !mssm.equals(e._1(),e._2())) {
                                // this is a conflict; one variant changed it, the other deleted it
                                tempdiff.add(new MostSimilarSubsequenceMapping.UpdateDeleteConflictEntry<T>(e._2(),null));
                            } else if (e._3()!=null && !mssm.equals(e._1(),e._3())) {
                                // this is a conflict; one variant changed it, the other deleted it
                                tempdiff.add(new MostSimilarSubsequenceMapping.UpdateDeleteConflictEntry<T>(null,e._3()));
                            } else tempdiff.add(new DiffEntry<T>(DiffType.REMOVE, e._1()));
                    } else {
                        if (mssm.equals(e._2(), e._3())) {
                            if (mssm.equals(e._1(), e._2()))
                                tempdiff.add(new DiffEntry<T>(DiffType.NONE, e._1()));
                            else
                                tempdiff.add(new DiffEntry<T>(DiffType.UPDATE, e._2()));
                        } else tempdiff.add(new DiffEntry<T>(DiffType.UPDATE, e._2(), e._3()));
                    }
                } else if (e._2() != null && e._3() != null) {
                    if (mssm.equals(e._2(), e._3()))
                        tempdiff.add(new DiffEntry<T>(DiffType.ADD, e._2()));
                    else
                        tempdiff.add(new DiffEntry<T>(DiffType.ADD, e._2(), e._3()));
                } else {
                    if (last instanceof ListConflictEntry) {
                        ListConflictEntry<T> conflictEntry = (ListConflictEntry<T>) last;
                        if (e._2() != null) conflictEntry.addLeft(e._2());
                        else conflictEntry.addRight(e._3());
                    } else {
                        if (last instanceof ListConflictEntry) {
                            ListConflictEntry<T> conflictEntry = (ListConflictEntry<T>) last;
                            if (e._2() != null) conflictEntry.addLeft(e._2());
                            else conflictEntry.addRight(e._3());
                        } else if (last==null || !last.getType().equals(DiffType.REMOVE)) {
                            if (e._2() != null) tempdiff.add(new DiffEntry<T>(DiffType.ADD, e._2()));
                            else tempdiff.add(new DiffEntry<T>(DiffType.ADD, e._3()));

                        } else tempdiff.add(new ListConflictEntry<T>(e._2(), e._3()));
                    }
                }
            }
            Iterator<DiffEntry<T>> it = tempdiff.iterator();
            while (it.hasNext()) {
                DiffEntry<T> e = it.next();
                if (e instanceof ListConflictEntry) {
                    ListConflictEntry<T> c = (ListConflictEntry<T>) e;
                    if (c.rightvalues.isEmpty()) for (T o : c.leftvalues) diff.add(new DiffEntry<T>(DiffType.ADD, o));
                    else if (c.leftvalues.isEmpty())
                        for (T o : c.rightvalues) diff.add(new DiffEntry<T>(DiffType.ADD, o));
                    else diff.add(c);
                } else diff.add(e);
            }
        }

        private Boolean inMap(Map<Integer, Integer> m, Integer k, Integer v) {
            //System.out.println(String.format("INMAP: %s %s %s",m,k,v));
            return m.containsKey(k) && m.get(k).equals(v);
        }

        private String fillup(String s) {
            int size = 1;
            for (T[] X : (T[][]) new Object[][]{A, O, B}) {
                for (T e : X) {
                    size = Math.max(size, e.toString().length());
                }
            }

            String r = String.format("%1$#" + (size + 1) + "s", s);
            return r.substring(r.length()-size);
        }

        private void printList(List<T> al, StringBuffer bw) {
            for (int i = 0; i < al.size(); i++) {
                if (al.get(i) == null) bw.append(fillup("-"));
                else bw.append(fillup(al.get(i).toString()));
            }
        }

        private String printArray(T[] al) {
            StringBuffer bw = new StringBuffer();
            bw.append("[");
            for (int i = 0; i < al.length; i++) {
                if (al[i] == null) bw.append(fillup("-"));
                else bw.append(fillup(al[i].toString()));
            }
            bw.append("]");
            return bw.toString();
        }

        public String toString() {
            StringBuffer buf = new StringBuffer();
            buf.append("A = " + printArray(A) + "\n");
            buf.append("O = " + printArray(O) + "\n");
            buf.append("B = " + printArray(B) + "\n");
            buf.append("\nAlignment:\n----------------------------------\n");
            buf.append("A : ");
            printList(alignment[1], buf);
            buf.append("\n");
            buf.append("O : ");
            printList(alignment[0], buf);
            buf.append("\n");
            buf.append("B : ");
            printList(alignment[2], buf);
            buf.append("\n");
            return buf.toString();
        }
    }
}
