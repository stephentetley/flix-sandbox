/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
    // Wrapper function for `setDefault(Locale.Category.DISPLAY, newLocale)` so we can work around
    // the nested enum
    public static void setDefaultDISPLAY(Locale newLocale) throws Exception {
        Locale.setDefault(Locale.Category.DISPLAY, newLocale);
    }

    // Wrapper function for `setDefault(Locale.Category.FORMAT, newLocale)` so we can work around
    // the nested enum
    public static void setDefaultFORMAT(Locale newLocale) throws Exception {
        Locale.setDefault(Locale.Category.FORMAT, newLocale);
    }

}
