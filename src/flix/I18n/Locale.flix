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

    use I18n.Locale;
    use I18n.Locale.{Locale};

    /// TODO - show return Result...
    pub def new(x: {language: String, country: String, variant: String}): Locale =
        import new java.util.Locale(String, String, String) as newLocale;
        Locale(newLocale(x.language, x.country, x.variant)) as & Pure

    pub def getDefault(): Locale & Impure = 
        import java.util.Locale:getDefault();
        Locale(getDefault())

    pub def setDefault(x: Locale): Result[Unit, String] & Impure = 
        import java.util.Locale:setDefault(##java.util.Locale);
        try { 
            let Locale(x1) = x;
            setDefault(x1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("setDefault failed")
        }


    pub enum Category {
        case Display
        case Format
    }

    pub def getDefaultWithCategory(x: Category): Locale & Impure = 
        import flix.runtime.spt.sandbox.i18n.LocaleWrapper:getDefaultDISPLAY();
        import flix.runtime.spt.sandbox.i18n.LocaleWrapper:getDefaultFORMAT();
        match x { 
            case Display => Locale(getDefaultDISPLAY())
            case Format => Locale(getDefaultFORMAT())
        }

    pub def setDefaultWithCategory (category: Category, x: Locale): Result[Unit, String] & Impure = 
        import flix.runtime.spt.sandbox.i18n.LocaleWrapper:setDefaultDISPLAY(##java.util.Locale);
        import flix.runtime.spt.sandbox.i18n.LocaleWrapper:setDefaultFORMAT(##java.util.Locale);
        try { 
            let Locale(x1) = x;
            match category { 
                case Display => setDefaultDISPLAY(x1)
                case Format => setDefaultFORMAT(x1)
            };
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("setDefault failed")
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

    pub def forLanguageTag(tag: String): Option[Locale] =
        import java.util.Locale:forLanguageTag(String);
        match forLanguageTag(tag) {
            case s if Object.isNull(s) => None
            case s => Some(Locale(s))
        } as & Pure



    /// This marshals okay...
    pub def getAvailableLocales(): Array[Locale] & Impure = 
        import java.util.Locale:getAvailableLocales();
        let arr = getAvailableLocales();
        Array.map(Locale, arr)

    pub enum IsoCountryCode { 
        case Part1Alpha2
        case Part1Alpha3
        case Part3
    }

    pub def marshal(x: IsoCountryCode): ##flix.runtime.spt.sandbox.i18n.IsoCountryCodeEnum =
        import get flix.runtime.spt.sandbox.i18n.IsoCountryCodeEnum:FLIX_PART1_ALPHA2   as getFLIX_PART1_ALPHA2;
        import get flix.runtime.spt.sandbox.i18n.IsoCountryCodeEnum:FLIX_PART1_ALPHA3   as getFLIX_PART1_ALPHA3;
        import get flix.runtime.spt.sandbox.i18n.IsoCountryCodeEnum:FLIX_PART3          as getFLIX_PART3;
        match x {
            case Part1Alpha2    => getFLIX_PART1_ALPHA2()
            case Part1Alpha3    => getFLIX_PART1_ALPHA3()
            case Part3          => getFLIX_PART3()
        } as & Pure


    /// Cursors differ only on the create function and what they are marshalled to, so make making them common...
    def makeLocaleStringCursor(stringCursor: ##flix.runtime.spt.sandbox.i18n.LocaleStringCursor, failMsg: String): Cursor[String, String] =
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor.hasNext() as ffiHasNext;
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor.next() as ffiNext;
        try {
            let cursorAlg: CursorAlg[String, String] =  {
                  hasNext = () -> ffiHasNext(stringCursor)
                , next = Cursor.guardThunk(_ -> ffiNext(stringCursor), "next failed")
                , finalize = () -> () as & Impure
            };
            Cursor.makeCursor(cursorAlg)
        } catch {
            case _: ##java.lang.Exception => Cursor.failure(failMsg)
        }

    pub def getIsoCountries(): List[String] & Impure =
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor:createGetISOCountriesCursor() as newCursor;
        let iter = newCursor();
        makeLocaleStringCursor(iter, "getIsoCountryCodes failed")
            |> Cursor.toList!
            |> flip(Result.getWithDefault, Nil)


    pub def getIsoCountriesWithIsoCountryCode(icc: IsoCountryCode): Set[String] & Impure =
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor:createGetISOCountriesWithICCCursor(##flix.runtime.spt.sandbox.i18n.IsoCountryCodeEnum) as newCursor;
        let icc1 = marshal(icc);
        let iter = newCursor(icc1);
        makeLocaleStringCursor(iter, "getIsoCountryCodesWithIsoCountryCode failed")
            |> Cursor.toSet!
            |> flip(Result.getWithDefault, Set#{})

    pub def getIsoLanguages(): List[String] & Impure =
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor:createGetISOLanguagesCursor() as newCursor;
        let iter = newCursor();
        makeLocaleStringCursor(iter, "getIsoLanguages failed")
            |> Cursor.toList!
            |> flip(Result.getWithDefault, Nil)


    pub def getUnicodeLocaleAttributes(locale: Locale): Set[String] & Impure =
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor:createGetUnicodeLocaleAttributesCursor(##java.util.Locale) as newCursor;
        let Locale(locale1) = locale;
        let iter = newCursor(locale1);
        makeLocaleStringCursor(iter, "getUnicodeLocaleAttributes failed")
            |> Cursor.toSet!
            |> flip(Result.getWithDefault, Set#{})


    pub def getUnicodeLocaleKeys(locale: Locale): Set[String] & Impure =
        import flix.runtime.spt.sandbox.i18n.LocaleStringCursor:createGetUnicodeLocaleKeysCursor(##java.util.Locale) as newCursor;
        let Locale(locale1) = locale;
        let iter = newCursor(locale1);
        makeLocaleStringCursor(iter, "getUnicodeLocaleKeys failed")
            |> Cursor.toSet!
            |> flip(Result.getWithDefault, Set#{})

}
