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
    pub enum NormalizerForm with Eq, Order, ToString {
        case Nfc
        case Nfd
        case Nfkc
        case Nfkd
    }

}


namespace Text/Normalizer {

    use Text.NormalizerForm;
    use Text.NormalizerForm.{Nfc, Nfd, Nfkc, Nfkd};

    pub def marshal(x: NormalizerForm): ##java.text.Normalizer$Form =
        import get java.text.Normalizer$Form:NFC as getNFC;
        import get java.text.Normalizer$Form:NFD as getNFD;
        import get java.text.Normalizer$Form:NFKC as getNFKC;
        import get java.text.Normalizer$Form:NFKD as getNFKD;
        match x {
            case Nfc    => getNFC()
            case Nfd    => getNFD()
            case Nfkc   => getNFKC()
            case Nfkd   => getNFKD()
        } as & Pure

    pub def unmarshal(x: ##java.text.Normalizer$Form): NormalizerForm =
        import get java.text.Normalizer$Form:NFC as getNFC;
        import get java.text.Normalizer$Form:NFD as getNFD;
        import get java.text.Normalizer$Form:NFKC as getNFKC;
        import get java.text.Normalizer$Form:NFKD as getNFKD;
        match x {
            case x1 if primEquals(x1, getNFC())    => Nfc
            case x1 if primEquals(x1, getNFD())    => Nfd
            case x1 if primEquals(x1, getNFKC())   => Nfkc
            case x1 if primEquals(x1, getNFKD())   => Nfkd
        } as & Pure

    /// Warning - is this reference equality or value equality?
    def primEquals(x: ##java.text.Normalizer$Form, y: ##java.text.Normalizer$Form): Bool =
        import java.lang.Object.equals(##java.lang.Object);
        let x1 = x as ##java.lang.Object;
        let y1 = y as ##java.lang.Object;
        equals(x1, y1) as & Pure


    pub def normalize(nform: NormalizerForm, src: String): String =
        import java.text.Normalizer:normalize(##java.lang.CharSequence, ##java.text.Normalizer$Form);
        try {
            let nform1 = marshal(nform);
            normalize(src as ##java.lang.CharSequence, nform1) as & Pure
        } catch {
            case _:##java.lang.Exception => ""
        }

    pub def isNormalized(nform: NormalizerForm, src: String): Bool =
        import java.text.Normalizer:isNormalized(##java.lang.CharSequence, ##java.text.Normalizer$Form);
        try {
            let nform1 = marshal(nform);
            isNormalized(src as ##java.lang.CharSequence, nform1) as & Pure
        } catch {
            case _:##java.lang.Exception => false
        }

}
