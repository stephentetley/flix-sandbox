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



import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

public class LocaleStringCursor {
    private Iterator<String> iter;

    protected LocaleStringCursor(Iterator<String> iter1) {
        iter = iter1;
    }

    public static LocaleStringCursor createGetISOCountriesCursor(IsoCountryCodeEnum icc) throws Exception {
        Set<String> codes = Locale.getISOCountries(IsoCountryCodeEnum.convertToInner(icc));
        return new LocaleStringCursor(codes.iterator());
    }

    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public String next() { return this.iter.next(); }

}
