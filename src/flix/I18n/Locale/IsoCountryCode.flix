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

use I18n/Locale.IsoCountryCode;
use I18n/Locale.IsoCountryCode.{Part1_Alpha2, Part1_Alpha3, Part3};

namespace I18n/Locale {

    enum IsoCountryCode {
        case Part1_Alpha2,
        case Part1_Alpha3,
        case Part3
    }
}

// WARNING - Currently this doesn't compile:
// "Cannot find "##java.util.Locale.IsoCountryCode"

namespace I18n/Locale/IsoCountryCode {

    pub def marshal(x: IsoCountryCode): ##java.util.Locale.IsoCountryCode =
        import get java.util.Locale.IsoCountryCode:PART1_ALPHA2 as getPART1_ALPHA2;
        import get java.util.Locale.IsoCountryCode:PART1_ALPHA2 as getPART1_ALPHA3;
        import get java.util.Locale.IsoCountryCode:PART1_ALPHA2 as getPART3;
        match x {
            case Part1_Alpha2   => getPART1_ALPHA2()
            case Part1_Alpha3   => getPART1_ALPHA3()
            case Part3          => getPART3()
        } as & Pure

    pub def unmarshal(x: ##java.util.Locale.IsoCountryCode): IsoCountryCode =
        import get java.util.Locale.IsoCountryCode:PART1_ALPHA2 as getPART1_ALPHA2;
        import get java.util.Locale.IsoCountryCode:PART1_ALPHA2 as getPART1_ALPHA3;
        import get java.util.Locale.IsoCountryCode:PART1_ALPHA2 as getPART3;
        match x {
            case x1 if x1 == getPART1_ALPHA2()      => Part1_Alpha2
            case x1 if x1 == getPART1_ALPHA3()      => Part1_Alpha3
            case x1 if x1 == getPART3()             => Part3
        } as & Pure

        
}