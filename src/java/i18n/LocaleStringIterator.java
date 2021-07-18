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



import java.util.Arrays;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

public class LocaleStringIterator {
    private Iterator<String> iter;

    protected LocaleStringIterator(Iterator<String> iter1) {
        iter = iter1;
    }

    public static LocaleStringIterator newGetISOCountriesIterator() throws Exception {
        String[] codes = Locale.getISOCountries();
        return new LocaleStringIterator(Arrays.stream(codes).iterator());
    }

    public static LocaleStringIterator newGetISOCountriesWithICCIterator(IsoCountryCodeEnum icc) throws Exception {
        Set<String> codes = Locale.getISOCountries(IsoCountryCodeEnum.convertToInner(icc));
        return new LocaleStringIterator(codes.iterator());
    }

    public static LocaleStringIterator newGetISOLanguagesIterator() throws Exception {
        String[] langs = Locale.getISOLanguages();
        return new LocaleStringIterator(Arrays.stream(langs).iterator());
    }

    public static LocaleStringIterator newGetUnicodeLocaleAttributesIterator(Locale locale) throws Exception {
        Set<String> attrs = locale.getUnicodeLocaleAttributes();
        return new LocaleStringIterator(attrs.iterator());
    }

    public static LocaleStringIterator newGetUnicodeLocaleKeysIterator(Locale locale) throws Exception {
        Set<String> keys = locale.getUnicodeLocaleKeys();
        return new LocaleStringIterator(keys.iterator());
    }


    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public String next() { return this.iter.next(); }

}
