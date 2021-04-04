package flix.runtime.spt.sandbox.text;

import java.text.Normalizer;

public class NormalizerWrapper {

    /// Use our own `NormalizerFormEnum` because Java's `Normalizer.Form` is
    /// and inner class (enum) and is not accessible to Flix.
    public static String normalize(String src, NormalizerFormEnum form) {

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
