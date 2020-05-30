package flix.runtime.spt.sandbox.system;

import flix.runtime.spt.sandbox.system.marshal.MarshalStringArray;

import java.io.InputStream;
import java.nio.charset.Charset;

public class ProcessWrapper {

    public static ProcessBuilder create(String cmd, MarshalStringArray args) {
        args.cons(cmd);
        return new ProcessBuilder(args.toArray());
    }

    public static String getInput(Process proc, Charset cs) throws Exception {
        InputStream ins = proc.getInputStream();
        return new String(ins.readAllBytes(), cs);
    }

    public static String getError(Process proc, Charset cs) throws Exception {
        InputStream ins = proc.getErrorStream();
        return new String(ins.readAllBytes(), cs);
    }

}
