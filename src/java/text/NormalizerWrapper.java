package flix.runtime.spt.sandbox.text;

import java.text.Normalizer;

public class NormalizerWrapper {

    /// Sub function for `Paths.get` as it is "varargs" in Java.
    public static String normalize(String src, NormalizerForm form) {

        switch(form) {
            case NFC:
                return Normalizer.normalize(src, Normalizer.Form.NFC);
            case NFD:
                return Normalizer.normalize(src, Normalizer.Form.NFD);
            case NFKC:
                return Normalizer.normalize(src, Normalizer.Form.NFKC);
            default:
                return Normalizer.normalize(src, Normalizer.Form.NFKD);
        }
    }

}
