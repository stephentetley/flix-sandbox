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

namespace Text {
    ///
    /// Represents a Collator.
    ///
    pub opaque type Collator = ##java.text.Collator
}

namespace Text/Collator {

    use Text.Collator;
    use Text.Collator.{Collator};


    pub enum DecompositionMode {
        case NoDecomposition
        case CanonicalDecomposition
        case FullDecomposition
    }

    instance ToString[DecompositionMode] {
        pub def toString(x: DecompositionMode): String = match x {
            case NoDecomposition => "NoDecomposition"
            case CanonicalDecomposition => "CanonicalDecomposition"
            case FullDecomposition => "FullDecomposition"
        }
    }

    instance Eq[DecompositionMode] {
        pub def eq(x: DecompositionMode, y: DecompositionMode): Bool = match (x, y) {
            case (NoDecomposition, NoDecomposition) => true
            case (CanonicalDecomposition, CanonicalDecomposition) => true
            case (FullDecomposition, FullDecomposition) => true
            case (_, _) => false
        }
    }

    pub enum CollatorStrength {
        case Identical
        case Primary
        case Secondary
        case Tertiary
    }

    instance ToString[CollatorStrength] {
        pub def toString(x: CollatorStrength): String = match x {
            case Identical => "Identical"
            case Primary => "Primary"
            case Secondary => "Secondary"
            case Tertiary => "Tertiary"
        }
    }

    instance Eq[CollatorStrength] {
        pub def eq(x: CollatorStrength, y: CollatorStrength): Bool = match (x, y) {
            case (Identical, Identical) => true
            case (Primary, Primary) => true
            case (Secondary, Secondary) => true
            case (Tertiary, Tertiary) => true
            case (_, _) => false
        }
    }

    def marshallDecompositionMode(x: DecompositionMode): Int32 =
        import get java.text.Collator:NO_DECOMPOSITION          as getNO_DECOMPOSITION;
        import get java.text.Collator:CANONICAL_DECOMPOSITION   as getCANONICAL_DECOMPOSITION;
        import get java.text.Collator:FULL_DECOMPOSITION        as getFULL_DECOMPOSITION;
        match x {
            case NoDecomposition => getNO_DECOMPOSITION()
            case CanonicalDecomposition => getCANONICAL_DECOMPOSITION()
            case FullDecomposition => getFULL_DECOMPOSITION()
        } as & Pure

   def unmarshallDecompositionMode(i: Int32): DecompositionMode =
        import get java.text.Collator:NO_DECOMPOSITION          as getNO_DECOMPOSITION;
        import get java.text.Collator:CANONICAL_DECOMPOSITION   as getCANONICAL_DECOMPOSITION;
        import get java.text.Collator:FULL_DECOMPOSITION        as getFULL_DECOMPOSITION;
        match i {
            case n if n == getNO_DECOMPOSITION() => NoDecomposition
            case n if n == getCANONICAL_DECOMPOSITION() => CanonicalDecomposition
            case n if n == getFULL_DECOMPOSITION() => FullDecomposition
        } as & Pure

    def marshallCollatorStrength(x: CollatorStrength): Int32 =
        import get java.text.Collator:IDENTICAL as getIDENTICAL;
        import get java.text.Collator:PRIMARY as getPRIMARY;
        import get java.text.Collator:SECONDARY as getSECONDARY;
        import get java.text.Collator:TERTIARY as getTERTIARY;
        match x {
            case Identical => getIDENTICAL()
            case Primary => getPRIMARY()
            case Secondary => getSECONDARY()
            case Tertiary => getTERTIARY()
        } as & Pure

    /// Warning - I am not sure each enum has a distinct int value in Java land...
    def unmarshallCollatorStrength(i: Int32): CollatorStrength =
        import get java.text.Collator:IDENTICAL as getIDENTICAL;
        import get java.text.Collator:PRIMARY as getPRIMARY;
        import get java.text.Collator:SECONDARY as getSECONDARY;
        import get java.text.Collator:TERTIARY as getTERTIARY;
        match i {
            case n if n == getIDENTICAL() => Identical
            case n if n == getPRIMARY() => Primary
            case n if n == getSECONDARY() => Secondary
            case n if n == getTERTIARY() => Tertiary
        } as & Pure

    ///
    /// Uses a Collator to be accurate with the locale of the JVM.
    ///
    pub def getInstance(): Collator & Impure =
        import java.text.Collator:getInstance();
        Collator(getInstance())


    pub def getInstanceWithLocale(locale: I18n.Locale): Collator =
        import java.text.Collator:getInstance(##java.util.Locale);
        use I18n.Locale.{Locale};
        let Locale(locale1) = locale;
        Collator(getInstance(locale1)) as & Pure

    ///
    /// Test equality of strings `s1` and `s2` using Collator `x``.
    ///
    pub def equals(x: Collator, s1: String, s2: String): Bool =
        import java.text.Collator.equals(String, String);
        let Collator(x1) = x;
        equals(x1, s1, s2) as & Pure

    ///
    /// Compare strings `s1` and `s2` using Collator `x``.
    ///
    pub def compare(x: Collator, s1: String, s2: String): Int32 =
        import java.text.Collator.compare(String, String);
        let Collator(x1) = x;
        compare(x1, s1, s2) as & Pure



    pub def getDecomposition(x: Collator): DecompositionMode & Impure =
        import java.text.Collator.getDecomposition();
        let Collator(x1) = x;
        unmarshallDecompositionMode(getDecomposition(x1))

    pub def setDecomposition!(x: Collator, mode: DecompositionMode): Unit & Impure =
        import java.text.Collator.setDecomposition(Int32);
        let Collator(x1) = x;
        let mode1 = marshallDecompositionMode(mode);
        setDecomposition(x1, mode1)


    pub def getStrength(x: Collator): CollatorStrength & Impure =
        import java.text.Collator.getStrength();
        let Collator(x1) = x;
        unmarshallCollatorStrength(getStrength(x1))

    pub def setStrength!(x: Collator, strength: CollatorStrength): Unit & Impure =
        import java.text.Collator.setStrength(Int32);
        let Collator(x1) = x;
        let strength1 = marshallCollatorStrength(strength);
        setStrength(x1, strength1)

}
