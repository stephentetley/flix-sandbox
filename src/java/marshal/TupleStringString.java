package flix.runtime.library.system.marshal;

import java.nio.file.Files;

public class PairStringString {
    private String s1;
    private String s2;

    public PairStringString(String str1, String str2) {
        this.s1 = str1;
        this.s2 = str2;
    }

    public String get0() {
        return s1;
    }

    public String get1() {
        return s2;
    }
}
