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

use I18n.Locale;
use I18n.Locale.{Locale};

namespace I18n {

    pub opaque type Locale = ##java.util.Locale
}

namespace I18n/Locale {

    pub def new(x: { language: String, country: String, variant: String }): Locale = 
        import new java.util.Locale(String, String, String) as newLocale;
        Locale(newLocale(x.language, x.country, x.variant)) as & Pure

    pub def getDefault(): Locale = 
        import java.util.Locale:getDefault();
        Locale(getDefault()) as & Pure


    pub def canada(): Locale = 
        import get java.util.Locale:CANADA as getCanada;
        Locale(getCanada()) as & Pure

    pub def canada_french(): Locale = 
        import get java.util.Locale:CANADA_FRENCH as getCanadaFrench;
        Locale(getCanadaFrench()) as & Pure

    pub def china(): Locale = 
        import get java.util.Locale:CHINA as getChina;
        Locale(getChina()) as & Pure

    pub def chinese(): Locale = 
        import get java.util.Locale:CHINESE as getChinese;
        Locale(getChinese()) as & Pure

    pub def english(): Locale = 
        import get java.util.Locale:ENGLISH as getEnglish;
        Locale(getEnglish()) as & Pure

    pub def france(): Locale = 
        import get java.util.Locale:FRANCE as getFrance;
        Locale(getFrance()) as & Pure
    
    pub def french(): Locale = 
        import get java.util.Locale:FRENCH as getFrench;
        Locale(getFrench()) as & Pure
    
    pub def german(): Locale = 
        import get java.util.Locale:GERMAN as getGerman;
        Locale(getGerman()) as & Pure
 
    pub def germany(): Locale = 
        import get java.util.Locale:GERMANY as getGermany;
        Locale(getGermany()) as & Pure
 
    pub def italian(): Locale = 
        import get java.util.Locale:ITALIAN as getItalian;
        Locale(getItalian()) as & Pure
 
    pub def italy(): Locale = 
        import get java.util.Locale:ITALY as getItaly;
        Locale(getItaly()) as & Pure
 
    pub def japan(): Locale = 
        import get java.util.Locale:JAPAN as getJapan;
        Locale(getJapan()) as & Pure
    
    pub def japanese(): Locale = 
        import get java.util.Locale:JAPANESE as getJapanese;
        Locale(getJapanese()) as & Pure

    pub def korea(): Locale = 
        import get java.util.Locale:KOREA as getKorea;
        Locale(getKorea()) as & Pure

    pub def korean(): Locale = 
        import get java.util.Locale:KOREAN as getKorean;
        Locale(getKorean()) as & Pure

    pub def prc(): Locale = 
        import get java.util.Locale:PRC as getPRC;
        Locale(getPRC()) as & Pure

    pub def root(): Locale = 
        import get java.util.Locale:ROOT as getRoot;
        Locale(getRoot()) as & Pure

    pub def simplified_chinese(): Locale = 
        import get java.util.Locale:SIMPLIFIED_CHINESE as getSimplifiedChinese;
        Locale(getSimplifiedChinese()) as & Pure

    pub def taiwan(): Locale = 
        import get java.util.Locale:TAIWAN as getTaiwan;
        Locale(getTaiwan()) as & Pure

    pub def traditional_chinese(): Locale = 
        import get java.util.Locale:TRADITIONAL_CHINESE as getTraditionalChinese;
        Locale(getTraditionalChinese()) as & Pure

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

}