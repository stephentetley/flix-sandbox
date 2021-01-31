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
}

namespace I18n/Locale {

    use I18n.Locale;
    use I18n.Locale.{Locale};

    pub def new(x: { language: String, country: String, variant: String }): Locale = 
        import new java.util.Locale(String, String, String) as newLocale;
        Locale(newLocale(x.language, x.country, x.variant)) as & Pure

    pub def getDefault(): Locale & Impure = 
        import java.util.Locale:getDefault();
        Locale(getDefault())

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
        x1.getCountry() as & Pure

    pub def getLanguage(x: Locale): String = 
        import java.util.Locale.getLanguage();
        let Locale(x1) = x;
        x1.getLanguage() as & Pure

    pub def hasExtensions(x: Locale): Bool = 
        import java.util.Locale.hasExtensions();
        let Locale(x1) = x;
        x1.hasExtensions() as & Pure

    pub def toString(x: Locale): String = 
        import java.util.Locale.toString();
        let Locale(x1) = x;
        x1.toString() as & Pure

    pub def getAvailableLocales(): Array[Locale] & Impure = 
        import java.util.Locale:getAvailableLocales();
        let arr = getAvailableLocales();
        Array.map(Locale, arr)

    pub enum IsoCountryCode { 
        case Part1Alpha2
        case Part1Alpha3
        case Part3
    }

    pub def get3IsoCountryCodes(x: IsoCountryCode): Option[Set[String]] & Impure = match x {
        case Part1Alpha2 => getPart1Alpha2IsoCountryCodes()
        case Part1Alpha3 => getPart1Alpha3IsoCountryCodes()
        case Part3 => getPart3IsoCountryCodes()
    }

    def getPart1Alpha3IsoCountryCodes(): Option[Set[String]] & Impure =
        import new flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart1Alpha3IsoCountryCodes() as newIter;
        import flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart1Alpha3IsoCountryCodes.hasNext();
        import flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart1Alpha3IsoCountryCodes.next();
        try {
            let iter = newIter();
            let step = () ->
                if (iter.hasNext() == false)
                    None
                else {
                    let s1 = iter.next();
                    Some(s1)
                };
            List.unfoldWithIter(step) |> List.toSet |> Some
        } catch {
            case _: ##java.lang.Exception => None
        }

    def getPart1Alpha2IsoCountryCodes(): Option[Set[String]] & Impure =
        import new flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart1Alpha2IsoCountryCodes() as newIter;
        import flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart1Alpha2IsoCountryCodes.hasNext();
        import flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart1Alpha2IsoCountryCodes.next();
        try {
            let iter = newIter();
            let step = () ->
                if (iter.hasNext() == false)
                    None
                else {
                    let s1 = iter.next();
                    Some(s1)
                };
            List.unfoldWithIter(step) |> List.toSet |> Some
        } catch {
            case _: ##java.lang.Exception => None
        }

    def getPart3IsoCountryCodes(): Option[Set[String]] & Impure =
        import new flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart3IsoCountryCodes() as newIter;
        import flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart3IsoCountryCodes.hasNext();
        import flix.runtime.spt.sandbox.i18n.marshal.UnmarshalPart3IsoCountryCodes.next();
        try {
            let iter = newIter();
            let step = () ->
                if (iter.hasNext() == false)
                    None
                else {
                    let s1 = iter.next();
                    Some(s1)
                };
            List.unfoldWithIter(step) |> List.toSet |> Some
        } catch {
            case _: ##java.lang.Exception => None
        }




}