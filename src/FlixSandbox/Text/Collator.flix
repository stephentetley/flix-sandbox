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

namespace FlixSandbox/Text {
    
    ///
    /// Represents a Collator.
    ///
    pub enum Collator(##java.text.Collator)
}

namespace FlixSandbox/Text/Collator {

    use FlixSandbox/Text.Collator;
    use FlixSandbox/Text.Collator.{Collator};

    pub enum DecompositionMode with Eq, Order, ToString {
        case NoDecomposition
        case CanonicalDecomposition
        case FullDecomposition
    }

    pub enum CollatorStrength with Eq, Order, ToString {
        case Identical
        case Primary
        case Secondary
        case Tertiary
    }


    def marshallDecompositionMode(x: DecompositionMode): Int32 =
        import static get java.text.Collator.NO_DECOMPOSITION: Int32 & Pure as getNoDecomposition;
        import static get java.text.Collator.CANONICAL_DECOMPOSITION: Int32 & Pure as getCanonicalDecomposition;
        import static get java.text.Collator.FULL_DECOMPOSITION: Int32 & Pure as getFullDecomposition;
        match x {
            case NoDecomposition        => getNoDecomposition()
            case CanonicalDecomposition => getCanonicalDecomposition()
            case FullDecomposition      => getFullDecomposition()
        }

   def unmarshallDecompositionMode(i: Int32): DecompositionMode =
        import static get java.text.Collator.CANONICAL_DECOMPOSITION: Int32 & Pure as getCanonicalDecomposition;
        import static get java.text.Collator.FULL_DECOMPOSITION: Int32 & Pure as getFullDecomposition;
        match i {
            case n if n == getCanonicalDecomposition() => CanonicalDecomposition
            case n if n == getFullDecomposition() => FullDecomposition
            case _ => NoDecomposition
        }

    def marshallCollatorStrength(x: CollatorStrength): Int32 =
        import static get java.text.Collator.IDENTICAL: Int32 & Pure as getIdentical;
        import static get java.text.Collator.PRIMARY: Int32 & Pure as getPrimary;
        import static get java.text.Collator.SECONDARY: Int32 & Pure as getSecondary;
        import static get java.text.Collator.TERTIARY: Int32 & Pure as getTertiary;
        match x {
            case Identical => getIdentical()
            case Primary   => getPrimary()
            case Secondary => getSecondary()
            case Tertiary  => getTertiary()
        }

    /// Warning - I am not sure each enum has a distinct int value in Java land...
    def unmarshallCollatorStrength(i: Int32): CollatorStrength =
        import static get java.text.Collator.IDENTICAL: Int32 & Pure as getIdentical;
        import static get java.text.Collator.PRIMARY: Int32 & Pure as getPrimary;
        import static get java.text.Collator.SECONDARY: Int32 & Pure as getSecondary;
        import static get java.text.Collator.TERTIARY: Int32 & Pure as getTertiary;
        match i {
            case n if n == getIdentical() => Identical
            case n if n == getPrimary() => Primary
            case n if n == getSecondary() => Secondary
            case n if n == getTertiary() => Tertiary
        }

    ///
    /// Uses a Collator to be accurate with the locale of the JVM.
    ///
    pub def getInstance(): Collator & Impure =
        import static java.text.Collator.getInstance(): ##java.text.Collator & Impure;
        Collator(getInstance())

    /// TODO: Move to Locale?
    pub def getInstanceWithLocale(locale: I18n.Locale): Collator =
        import static java.text.Collator.getInstance(##java.util.Locale): ##java.text.Collator & Pure;
        use I18n.Locale.{Locale};
        let Locale(locale1) = locale;
        Collator(getInstance(locale1))

    /// TODO - instance with decomposition and strength?


    ///
    /// Test equality of strings `s1` and `s2` with the collator `x``.
    ///
    pub def equalsWith(x: Collator, s1: String, s2: String): Bool =
        import java.text.Collator.equals(String, String): Bool & Pure;
        let Collator(x1) = x;
        equals(x1, s1, s2)

    ///
    /// Compare strings `s1` and `s2` with the collator `x``.
    ///
    pub def compareWith(x: Collator, s1: String, s2: String): Comparison =
        import java.text.Collator.compare(String, String): Int32 & Impure;
        let Collator(x1) = x;
        (compare(x1, s1, s2) as & Pure) |> Comparison.fromInt32



    pub def getDecomposition(x: Collator): DecompositionMode & Impure =
        import java.text.Collator.getDecomposition(): Int32 & Impure;
        let Collator(x1) = x;
        unmarshallDecompositionMode(getDecomposition(x1))

    /// TODO - test cloning the Java objects and then get a pure API...

    def clone(x: Collator): ##java.text.Collator & Impure =
        import java.text.Collator.clone(): ##java.text.Collator & Impure;
        let Collator(x1) = x;
        clone(x1) as ##java.text.Collator

    pub def setDecomposition(x: Collator, mode: DecompositionMode): Collator =
        import java.text.Collator.setDecomposition(Int32): Unit & Impure;
        let body = () -> {
            let x1 = clone(x);
            let mode1 = marshallDecompositionMode(mode);
            setDecomposition(x1, mode1);
            x1
        };
        Collator(body() as & Pure)


    pub def getStrength(x: Collator): CollatorStrength & Impure =
        import java.text.Collator.getStrength(): Int32 & Impure;
        let Collator(x1) = x;
        unmarshallCollatorStrength(getStrength(x1))

    pub def setStrength(x: Collator, strength: CollatorStrength): Collator =
        import java.text.Collator.setStrength(Int32): Unit & Impure;
        let body = () -> {
            let x1 = clone(x);
            let strength1 = marshallCollatorStrength(strength);
            setStrength(x1, strength1);
            x1
        };
        Collator(body() as & Pure)

}
