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



namespace I18n {

    pub opaque type Locale = ##java.util.Locale

    instance ToString[Locale] {
        pub def toString(x: Locale): String = I18n/Locale.toString(x)
    }

    instance FromString[Locale] {
        pub def fromString(x: String): Option[Locale] =
            import new java.util.Locale(String): ##java.util.Locale & Pure as newLocale;
            try {
                Some(Locale(newLocale(x)))
            } catch {
                case _: ##java.lang.Exception => None
            }
    }

    instance Eq[Locale] {
        pub def eq(x: Locale, y: Locale): Bool = I18n/Locale.equals(x, y)
    }

    instance Hash[Locale] {
        pub def hash(x: Locale): Int32 = I18n/Locale.hashCode(x)
    }

    /// No Order, so no Boxable

}

namespace I18n/Locale {

    use X.Cursor;
    use X.Cursor.{Cursor};
    use I18n.Locale;
    use I18n.Locale.{Locale};

    /// `new` functions should not return Result as there isn't a tangible notion of failure.
    /// Unrecognized locales can be built - operations on them just don't produce results.

    pub def new(language: String): Locale =
        import new java.util.Locale(String): ##java.util.Locale & Pure as newLocale;
        Locale(newLocale(language))

    pub def newWithCountry(language: String, country: String): Locale =
        import new java.util.Locale(String, String): ##java.util.Locale & Pure as newLocale;
        Locale(newLocale(language, country))

    pub def newWithVariant(language: String, country: String, variant: String): Locale =
        import new java.util.Locale(String, String, String): ##java.util.Locale & Pure as newLocale;
        Locale(newLocale(language, country, variant))

    pub def getDefault(): Locale & Impure =
        import static java.util.Locale.getDefault(): ##java.util.Locale & Impure;
        Locale(getDefault())

    pub def setJvmDefaultLocale!(x: Locale): Result[Unit, String] & Impure =
        import static java.util.Locale.setDefault(##java.util.Locale): Unit & Impure;
        try {
            let Locale(x1) = x;
            setDefault(x1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("setJvmDefaultLocale! failed")
        }


    pub enum Category with Eq, Order, ToString {
        case Display
        case Format
    }


    pub def getDefaultWithCategory(x: Category): Locale & Impure =
        import static java.util.Locale.getDefault(##java.util.Locale$Category): ##java.util.Locale & Impure;
        import static get java.util.Locale$Category.DISPLAY: ##java.util.Locale$Category & Pure as getDisplay;
        import static get java.util.Locale$Category.FORMAT: ##java.util.Locale$Category & Pure as getFormat;
        match x {
            case Display => getDisplay() |> getDefault |> Locale
            case Format  => getFormat() |> getDefault |> Locale
        }

    pub def setJvmDefaultLocaleWithCategory!(category: Category, x: Locale): Result[Unit, String] & Impure =
        import static java.util.Locale.setDefault(##java.util.Locale$Category, ##java.util.Locale): Unit & Impure;
        import static get java.util.Locale$Category.DISPLAY: ##java.util.Locale$Category & Pure as getDisplay;
        import static get java.util.Locale$Category.FORMAT: ##java.util.Locale$Category & Pure as getFormat;
        try {
            let Locale(x1) = x;
            let category = match category {
                case Display => getDisplay()
                case Format => getFormat()
            };
            setDefault(category, x1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("setDefaultWithCategory! failed")
        }

    pub def equals(x: Locale, y: Locale): Bool =
        import java.util.Locale.equals(##java.lang.Object): Bool & Pure;
        let Locale(x1) = x;
        let Locale(y1) = y;
        equals(x1, y1 as ##java.lang.Object)

    pub def toString(x: Locale): String =
        import java.util.Locale.toString(): String & Pure;
        let Locale(x1) = x;
        toString(x1)

    pub def hashCode(x: Locale): Int32 =
        import java.util.Locale.hashCode(): Int32 & Pure;
        let Locale(x1) = x;
        hashCode(x1)

    pub def canada(): Locale =
        import static get java.util.Locale.CANADA: ##java.util.Locale & Pure as getCANADA;
        Locale(getCANADA())

    pub def canada_french(): Locale =
        import static get java.util.Locale.CANADA_FRENCH: ##java.util.Locale & Pure as getCANADA_FRENCH;
        Locale(getCANADA_FRENCH())

    pub def china(): Locale =
        import static get java.util.Locale.CHINA: ##java.util.Locale & Pure as getCHINA;
        Locale(getCHINA())

    pub def chinese(): Locale =
        import static get java.util.Locale.CHINESE: ##java.util.Locale & Pure as getCHINESE;
        Locale(getCHINESE())

    pub def english(): Locale =
        import static get java.util.Locale.ENGLISH: ##java.util.Locale & Pure as getENGLISH;
        Locale(getENGLISH())

    pub def france(): Locale =
        import static get java.util.Locale.FRANCE: ##java.util.Locale & Pure as getFRANCE;
        Locale(getFRANCE())

    pub def french(): Locale =
        import static get java.util.Locale.FRENCH: ##java.util.Locale & Pure as getFRENCH;
        Locale(getFRENCH())

    pub def german(): Locale =
        import static get java.util.Locale.GERMAN: ##java.util.Locale & Pure as getGERMAN;
        Locale(getGERMAN())

    pub def germany(): Locale =
        import static get java.util.Locale.GERMANY: ##java.util.Locale & Pure as getGERMANY;
        Locale(getGERMANY())

    pub def italian(): Locale =
        import static get java.util.Locale.ITALIAN: ##java.util.Locale & Pure as getITALIAN;
        Locale(getITALIAN())

    pub def italy(): Locale =
        import static get java.util.Locale.ITALY: ##java.util.Locale & Pure as getITALY;
        Locale(getITALY())

    pub def japan(): Locale =
        import static get java.util.Locale.JAPAN: ##java.util.Locale & Pure as getJAPAN;
        Locale(getJAPAN())

    pub def japanese(): Locale =
        import static get java.util.Locale.JAPANESE: ##java.util.Locale & Pure as getJAPANESE;
        Locale(getJAPANESE())

    pub def korea(): Locale =
        import static get java.util.Locale.KOREA: ##java.util.Locale & Pure as getKOREA;
        Locale(getKOREA())

    pub def korean(): Locale =
        import static get java.util.Locale.KOREAN: ##java.util.Locale & Pure as getKOREAN;
        Locale(getKOREAN())

    pub def prc(): Locale =
        import static get java.util.Locale.PRC: ##java.util.Locale & Pure as getPRC;
        Locale(getPRC())

    pub def root(): Locale =
        import static get java.util.Locale.ROOT: ##java.util.Locale & Pure as getROOT;
        Locale(getROOT())

    pub def simplified_chinese(): Locale =
        import static get java.util.Locale.SIMPLIFIED_CHINESE: ##java.util.Locale & Pure as getSIMPLIFIED_CHINESE;
        Locale(getSIMPLIFIED_CHINESE())

    pub def taiwan(): Locale =
        import static get java.util.Locale.TAIWAN: ##java.util.Locale & Pure as getTAIWAN;
        Locale(getTAIWAN())

    pub def traditional_chinese(): Locale =
        import static get java.util.Locale.TRADITIONAL_CHINESE: ##java.util.Locale & Pure as getTRADITIONAL_CHINESE;
        Locale(getTRADITIONAL_CHINESE())

    pub def uk(): Locale =
        import static get java.util.Locale.UK: ##java.util.Locale & Pure as getUK;
        Locale(getUK())

    pub def us(): Locale =
        import static get java.util.Locale.US: ##java.util.Locale & Pure as getUS;
        Locale(getUS())


    pub def getCountry(x: Locale): String =
        import java.util.Locale.getCountry(): String & Pure;
        let Locale(x1) = x;
        getCountry(x1)

    pub def getLanguage(x: Locale): String =
        import java.util.Locale.getLanguage(): String & Pure;
        let Locale(x1) = x;
        getLanguage(x1)

    pub def getScript(x: Locale): String =
        import java.util.Locale.getScript(): String & Pure;
        let Locale(x1) = x;
        getScript(x1)

    pub def getISO3Country(x: Locale): Option[String] =
        import java.util.Locale.getISO3Country(): String & Pure;
        try {
            let Locale(x1) = x;
            Some(getISO3Country(x1))
        } catch {
            case _: ##java.util.MissingResourceException => None
        }

    pub def getISO3Language(x: Locale): Option[String] =
        import java.util.Locale.getISO3Language(): String & Pure;
        try {
            let Locale(x1) = x;
            Some(getISO3Language(x1))
        } catch {
            case _: ##java.util.MissingResourceException => None
        }

    pub def getDisplayLanguage(x: Locale): String =
        import java.util.Locale.getDisplayLanguage(): String & Pure;
        let Locale(x1) = x;
        getDisplayLanguage(x1)

    pub def getDisplayLanguageInLocale(x: Locale, inLocale: Locale): String =
        import java.util.Locale.getDisplayLanguage(##java.util.Locale): String & Pure;
        let Locale(x1) = x;
        let Locale(inLocale1) = inLocale;
        getDisplayLanguage(x1, inLocale1)


    pub def hasExtensions(x: Locale): Bool =
        import java.util.Locale.hasExtensions(): Bool & Pure;
        let Locale(x1) = x;
        hasExtensions(x1)

    pub def stripExtensions(x: Locale): Locale =
        import java.util.Locale.stripExtensions(): ##java.util.Locale & Pure;
        let Locale(x1) = x;
        Locale(stripExtensions(x1))


    pub def toLanguageTag(x: Locale): String =
        import java.util.Locale.toLanguageTag(): String & Pure;
        let Locale(x1) = x;
        toLanguageTag(x1)

    pub def forLanguageTag(tag: String): Locale =
        import static java.util.Locale.forLanguageTag(String): ##java.util.Locale & Pure;
        Locale(forLanguageTag(tag))



    /// This marshals okay...
    pub def getAvailableLocales(): Array[Locale] & Impure =
        import static java.util.Locale.getAvailableLocales(): Array[##java.util.Locale] & Impure;
        let arr = getAvailableLocales();
        Array.map(Locale, arr)

    pub enum IsoCountryCode with Eq, Order, Boxable, Hash, ToString {
        case Part1Alpha2
        case Part1Alpha3
        case Part3
    }

    pub def isoCountryCodeToNative(x: IsoCountryCode): ##java.util.Locale$IsoCountryCode =
        import static get java.util.Locale$IsoCountryCode.PART1_ALPHA2: ##java.util.Locale$IsoCountryCode & Pure as getPart1Alpha2;
        import static get java.util.Locale$IsoCountryCode.PART1_ALPHA3: ##java.util.Locale$IsoCountryCode & Pure as getPart1Alpha3;
        import static get java.util.Locale$IsoCountryCode.PART3: ##java.util.Locale$IsoCountryCode & Pure as getPart3;
        match x {
            case Part1Alpha2    => getPart1Alpha2()
            case Part1Alpha3    => getPart1Alpha3()
            case Part3          => getPart3()
        }


    pub def getIsoCountries(): Result[Array[String], String] & Impure =
        import static java.util.Locale.getISOCountries(): Array[String] & Impure;
        try {
            let arr = getISOCountries();
            Ok(arr)
        } catch {
            case _: ##java.lang.Exception => Err("getIsoCountries")
        }

    pub def getIsoCountriesWithIsoCountryCode(icc: IsoCountryCode): Result[Set[String], String] & Impure =
        import static java.util.Locale.getISOCountries(##java.util.Locale$IsoCountryCode): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        try {
            let icc1 = isoCountryCodeToNative(icc);
            let sjava = getISOCountries(icc1);
            let iter = iterator(sjava);
            let xs = iterateListOfString(iter, ks -> ks);
            Ok(List.toSet(xs))
        } catch {
            case _: ##java.lang.Exception => Err("getIsoCountriesWithIsoCountryCode")
        }


    def iterateListOfString(iter: ##java.util.Iterator, k: List[String] -> List[String]): List[String] & Impure =
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        if (hasNext(iter))
            let entry = next(iter) as String;
            iterateListOfString(iter, ks -> k(entry :: ks))
        else
            k(Nil)


    pub def getIsoLanguages(): Result[List[String], String] & Impure =
        import static java.util.Locale.getISOLanguages(): Array[String] & Impure;
        try {
            let arr = getISOLanguages();
            Ok(Array.toList(arr))
        } catch {
            case _: ##java.lang.Exception => Err("getIsoLanguages")
        }

    pub def getUnicodeLocaleAttributes(x: Locale): Result[Set[String], String] & Impure =
        import java.util.Locale.getUnicodeLocaleAttributes(): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        try {
            let Locale(x1) = x;
            let sjava = getUnicodeLocaleAttributes(x1);
            let iter = iterator(sjava);
            let xs = iterateListOfString(iter, ks -> ks);
            Ok(List.toSet(xs))
        } catch {
            case _: ##java.lang.Exception => Err("getUnicodeLocaleAttributes")
        }

    pub def getUnicodeLocaleKeys(x: Locale): Result[Set[String], String] & Impure =
        import java.util.Locale.getUnicodeLocaleKeys(): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        try {
            let Locale(x1) = x;
            let sjava = getUnicodeLocaleKeys(x1);
            let iter = iterator(sjava);
            let xs = iterateListOfString(iter, ks -> ks);
            Ok(List.toSet(xs))
        } catch {
            case _: ##java.lang.Exception => Err("getUnicodeLocaleKeys")
        }

}
