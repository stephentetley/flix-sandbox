package flix.runtime.spt.sandbox.system.marshal;

import java.util.LinkedList;

public class MarshalStringArray {

    private LinkedList<String> cells;

    public MarshalStringArray() {
        this.cells = new LinkedList<String>();
    }

    public void snoc(String s) {
        this.cells.addLast(s);
    }

    public void cons(String s) {
        this.cells.addFirst(s);
    }

    public String[] toArray() {
        return this.cells.toArray(new String[this.cells.size()]);
    }

}
