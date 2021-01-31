package flix.runtime.spt.sandbox.i18n;

import java.util.Locale;

    public class LocaleWrapper {

    // Wrapper function for `getDefault(Locale.Category.DISPLAY)` so we can work around
    // the nested enum
    public static Locale getDefaultDISPLAY() throws Exception {
        return Locale.getDefault(Locale.Category.DISPLAY);
    }

    // Wrapper function for `getDefault(Locale.Category.FORMAT)` so we can work around
    // the nested enum
    public static Locale getDefaultFORMAT() throws Exception {
        return Locale.getDefault(Locale.Category.FORMAT);
    }

}
