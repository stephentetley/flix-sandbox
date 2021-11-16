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
                import new java.util.Locale(String) as newLocale;
        try {
            Some(Locale(newLocale(x))) as & Pure
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
        import new java.util.Locale(String) as newLocale;
        Locale(newLocale(language)) as & Pure

    pub def newWithCountry(language: String, country: String): Locale =
        import new java.util.Locale(String, String) as newLocale;
        Locale(newLocale(language, country)) as & Pure

    pub def newWithVariant(language: String, country: String, variant: String): Locale =
        import new java.util.Locale(String, String, String) as newLocale;
        Locale(newLocale(language, country, variant)) as & Pure

    pub def getDefault(): Locale & Impure = 
        import java.util.Locale:getDefault();
        Locale(getDefault())

    pub def setJvmDefaultLocale!(x: Locale): Result[Unit, String] & Impure =
        import java.util.Locale:setDefault(##java.util.Locale);
        try { 
            let Locale(x1) = x;
            setDefault(x1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("setDefault! failed")
        }


    pub enum Category with Eq, Order, ToString {
        case Display
        case Format
    }


    pub def getDefaultWithCategory(x: Category): Locale & Impure =
        import java.util.Locale:getDefault(##java.util.Locale$Category);
        import get java.util.Locale$Category:DISPLAY as getDisplay;
        import get java.util.Locale$Category:FORMAT as getFormat;
        match x { 
            case Display => getDisplay() |> getDefault |> Locale
            case Format  => getFormat() |> getDefault |> Locale
        }

    pub def setJvmDefaultLocaleWithCategory!(category: Category, x: Locale): Result[Unit, String] & Impure =
        import java.util.Locale:setDefault(##java.util.Locale$Category, ##java.util.Locale);
        import get java.util.Locale$Category:DISPLAY as getDisplay;
        import get java.util.Locale$Category:FORMAT as getFormat;
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
        import java.util.Locale.equals(##java.lang.Object);
        let Locale(x1) = x;
        let Locale(y1) = y;
        equals(x1, y1 as ##java.lang.Object) as & Pure

    pub def toString(x: Locale): String =
        import java.util.Locale.toString();
        let Locale(x1) = x;
        toString(x1) as & Pure

    pub def hashCode(x: Locale): Int32 =
        import java.util.Locale.hashCode();
        let Locale(x1) = x;
        hashCode(x1) as & Pure

    pub def canada(): Locale = 
        import get java.util.Locale:CANADA as getCANADA;
        Locale(getCANADA()) as & Pure

    pub def canada_french(): Locale = 
        import get java.util.Locale:CANADA_FRENCH as getCANADA_FRENCH;
        Locale(getCANADA_FRENCH()) as & Pure

    pub def china(): Locale = 
        import get java.util.Locale:CHINA as getCHINA;
        Locale(getCHINA()) as & Pure

    pub def chinese(): Locale = 
        import get java.util.Locale:CHINESE as getCHINESE;
        Locale(getCHINESE()) as & Pure

    pub def english(): Locale = 
        import get java.util.Locale:ENGLISH as getENGLISH;
        Locale(getENGLISH()) as & Pure

    pub def france(): Locale = 
        import get java.util.Locale:FRANCE as getFrance;
        Locale(getFrance()) as & Pure
    
    pub def french(): Locale = 
        import get java.util.Locale:FRENCH as getFRENCH;
        Locale(getFRENCH()) as & Pure
    
    pub def german(): Locale = 
        import get java.util.Locale:GERMAN as getGERMAN;
        Locale(getGERMAN()) as & Pure
 
    pub def germany(): Locale = 
        import get java.util.Locale:GERMANY as getGERMANY;
        Locale(getGERMANY()) as & Pure
 
    pub def italian(): Locale = 
        import get java.util.Locale:ITALIAN as getITALIAN;
        Locale(getITALIAN()) as & Pure
 
    pub def italy(): Locale = 
        import get java.util.Locale:ITALY as getITALY;
        Locale(getITALY()) as & Pure
 
    pub def japan(): Locale = 
        import get java.util.Locale:JAPAN as getJAPAN;
        Locale(getJAPAN()) as & Pure
    
    pub def japanese(): Locale = 
        import get java.util.Locale:JAPANESE as getJAPANESE;
        Locale(getJAPANESE()) as & Pure

    pub def korea(): Locale = 
        import get java.util.Locale:KOREA as getKOREA;
        Locale(getKOREA()) as & Pure

    pub def korean(): Locale = 
        import get java.util.Locale:KOREAN as getKOREAN;
        Locale(getKOREAN()) as & Pure

    pub def prc(): Locale = 
        import get java.util.Locale:PRC as getPRC;
        Locale(getPRC()) as & Pure

    pub def root(): Locale = 
        import get java.util.Locale:ROOT as getROOT;
        Locale(getROOT()) as & Pure

    pub def simplified_chinese(): Locale = 
        import get java.util.Locale:SIMPLIFIED_CHINESE as getSIMPLIFIED_CHINESE;
        Locale(getSIMPLIFIED_CHINESE()) as & Pure

    pub def taiwan(): Locale = 
        import get java.util.Locale:TAIWAN as getTAIWAN;
        Locale(getTAIWAN()) as & Pure

    pub def traditional_chinese(): Locale = 
        import get java.util.Locale:TRADITIONAL_CHINESE as getTRADITIONAL_CHINESE;
        Locale(getTRADITIONAL_CHINESE()) as & Pure

    pub def uk(): Locale = 
        import get java.util.Locale:UK as getUK;
        Locale(getUK()) as & Pure

    pub def us(): Locale = 
        import get java.util.Locale:US as getUS;
        Locale(getUS()) as & Pure


    pub def getCountry(x: Locale): String = 
        import java.util.Locale.getCountry();
        let Locale(x1) = x;
        getCountry(x1) as & Pure

    pub def getLanguage(x: Locale): String = 
        import java.util.Locale.getLanguage();
        let Locale(x1) = x;
        getLanguage(x1) as & Pure

    pub def getScript(x: Locale): String = 
        import java.util.Locale.getScript();
        let Locale(x1) = x;
        getScript(x1) as & Pure

    pub def getISO3Country(x: Locale): Option[String] = 
        import java.util.Locale.getISO3Country();
        try {
            let Locale(x1) = x;
            Some(getISO3Country(x1) as & Pure)
        } catch { 
            case _: ##java.util.MissingResourceException => None
        }

    pub def getISO3Language(x: Locale): Option[String] = 
        import java.util.Locale.getISO3Language();
        try {
            let Locale(x1) = x;
            Some(getISO3Language(x1) as & Pure)
        } catch { 
            case _: ##java.util.MissingResourceException => None
        }

    pub def getDisplayLanguage(x: Locale): String = 
        import java.util.Locale.getDisplayLanguage();
        let Locale(x1) = x;
        getDisplayLanguage(x1) as & Pure

    pub def getDisplayLanguageInLocale(x: Locale, inLocale: Locale): String = 
        import java.util.Locale.getDisplayLanguage(##java.util.Locale);
        let Locale(x1) = x;
        let Locale(inLocale1) = inLocale;
        getDisplayLanguage(x1, inLocale1) as & Pure        


    pub def hasExtensions(x: Locale): Bool = 
        import java.util.Locale.hasExtensions();
        let Locale(x1) = x;
        hasExtensions(x1) as & Pure

    pub def stripExtensions(x: Locale): Locale = 
        import java.util.Locale.stripExtensions();
        let Locale(x1) = x;
        Locale(stripExtensions(x1) as & Pure)


    pub def toLanguageTag(x: Locale): String =
        import java.util.Locale.toLanguageTag();
        let Locale(x1) = x;
        toLanguageTag(x1) as & Pure

    pub def forLanguageTag(tag: String): Locale =
        import java.util.Locale:forLanguageTag(String);
        Locale(forLanguageTag(tag)) as & Pure



    /// This marshals okay...
    pub def getAvailableLocales(): Array[Locale] & Impure = 
        import java.util.Locale:getAvailableLocales();
        let arr = getAvailableLocales();
        Array.map(Locale, arr)

    pub enum IsoCountryCode with Eq, Order, Boxable, Hash, ToString {
        case Part1Alpha2
        case Part1Alpha3
        case Part3
    }

    pub def isoCountryCodeToNative(x: IsoCountryCode): ##java.util.Locale$IsoCountryCode =
        import get java.util.Locale$IsoCountryCode:PART1_ALPHA2 as getPart1Alpha2;
        import get java.util.Locale$IsoCountryCode:PART1_ALPHA3 as getPart1Alpha3;
        import get java.util.Locale$IsoCountryCode:PART3 as getPart3;
        match x {
            case Part1Alpha2    => getPart1Alpha2()
            case Part1Alpha3    => getPart1Alpha3()
            case Part3          => getPart3()
        } as & Pure


    pub def getIsoCountries(): Result[List[String], String] & Impure =
        import java.util.Locale:getISOCountries();
        try {
            let arr = getISOCountries();
            Ok(Array.toList(arr))
        } catch {
            case _: ##java.lang.Exception => Err("getIsoCountries")
        }

    pub def getIsoCountriesWithIsoCountryCode(icc: IsoCountryCode): Result[Set[String], String] & Impure =
        import java.util.Locale:getISOCountries(##java.util.Locale$IsoCountryCode);
        import java.util.Set.iterator();
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
        import java.util.Iterator.hasNext();
        import java.util.Iterator.next();
        if (hasNext(iter))
            let entry = next(iter) as String;
            iterateListOfString(iter, ks -> k(entry :: ks))
        else
            k(Nil)


    pub def getIsoLanguages(): Result[List[String], String] & Impure =
        import java.util.Locale:getISOLanguages();
        try {
            let arr = getISOLanguages();
            Ok(Array.toList(arr))
        } catch {
            case _: ##java.lang.Exception => Err("getIsoLanguages")
        }

    pub def getUnicodeLocaleAttributes(x: Locale): Result[Set[String], String] & Impure =
        import java.util.Locale.getUnicodeLocaleAttributes();
        import java.util.Set.iterator();
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
        import java.util.Locale.getUnicodeLocaleKeys();
        import java.util.Set.iterator();
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
