package flix.runtime.spt.sandbox.text;

import java.text.Normalizer;

public class NormalizerWrapper {

    /// Use our own `NormalizerFormEnum` because Java's `Normalizer.Form` is
    /// and inner class (enum) and is not accessible to Flix.
    public static String normalize(String src, NormalizerFormEnum normalForm) {
        Normalizer.Form nf = NormalizerFormEnum.convertToInner(normalForm);
        return Normalizer.normalize(src, nf);
    }
}
