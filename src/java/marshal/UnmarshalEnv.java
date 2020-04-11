package flix.runtime.library.system.marshal;

import flix.runtime.library.system.marshal.PairStringString;

import java.util.Iterator;
import java.util.Map;

public class UnmarshalEnv {
    private Iterator<Map.Entry<String, String>> iter;

    public UnmarshalEnv() throws Exception {
        Map<String, String> env = System.getenv();
        this.iter = env.entrySet().iterator();
    }

    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public PairStringString next() {
        Map.Entry<String,String> item = this.iter.next();
        return new PairStringString(item.getKey(), item.getValue());
    }
}
