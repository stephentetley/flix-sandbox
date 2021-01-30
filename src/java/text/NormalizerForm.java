package flix.runtime.spt.sandbox.text;

/// Provide our own enum rather because Normalizer.Form is an inner class
public enum NormalizerForm {
    NFC,
    NFD,
    NFKC,
    NFKD,
}
