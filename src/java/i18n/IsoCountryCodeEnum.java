/*
 * Copyright 2020 Stephen Tetley
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

/// Provide our own enum rather because Locale.IsoCountryCode is an inner class


import java.util.Locale;

public enum IsoCountryCodeEnum {
    FLIX_PART1_ALPHA2,
    FLIX_PART1_ALPHA3,
    FLIX_PART3;

    public static Locale.IsoCountryCode convertToInner(IsoCountryCodeEnum form) {
        switch(form) {
            case FLIX_PART1_ALPHA2:
                return Locale.IsoCountryCode.PART1_ALPHA2;
            case FLIX_PART1_ALPHA3:
                return Locale.IsoCountryCode.PART1_ALPHA3;
            default:
                return Locale.IsoCountryCode.PART3;
        }
    }

}
