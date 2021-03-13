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
    pub enum NormalizerForm {
        case Nfc
        case Nfd
        case Nfkc
        case Nfkd
    }

    instance ToString[NormalizerForm] {
        pub def toString(x: NormalizerForm): String = match x {
            case Nfc    => "Nfc"
            case Nfd    => "Nfd"
            case Nfkc   => "Nfkc"
            case Nfkd   => "Nfkd"
        }
    }

    instance Eq[NormalizerForm] {
        pub def eq(x: NormalizerForm, y: NormalizerForm): Bool = match (x,y) {
            case (Nfc, Nfc)     => true
            case (Nfd, Nfd)     => true
            case (Nfkc, Nfkc)   => true
            case (Nfkd, Nfkd)   => true
            case (_,_)          => false
        }

        pub def neq(x: NormalizerForm, y: NormalizerForm): Bool = not (x == y)
    }
}


namespace Text/Normalizer {

    use Text.NormalizerForm;
    use Text.NormalizerForm.{Nfc, Nfd, Nfkc, Nfkd};

    pub def marshal(x: NormalizerForm): ##flix.runtime.spt.sandbox.text.NormalizerForm =
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFC        as getNFC;
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFD        as getNFD;
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFKC       as getNFKC;
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFKD       as getNFKD;
        match x {
            case Nfc    => getNFC()
            case Nfd    => getNFD()
            case Nfkc   => getNFKC()
            case Nfkd   => getNFKD()
        } as & Pure

    pub def unmarshal(x: ##flix.runtime.spt.sandbox.text.NormalizerForm): NormalizerForm =
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFC        as getNFC;
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFD        as getNFD;
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFKC       as getNFKC;
        import get flix.runtime.spt.sandbox.text.NormalizerForm:NFKD       as getNFKD;
        match x {
            case x1 if primEquals(x1, getNFC())     => Nfc
            case x1 if primEquals(x1, getNFD())     => Nfd
            case x1 if primEquals(x1, getNFKC())    => Nfkc
            case x1 if primEquals(x1, getNFKD())    => Nfkd
        } as & Pure


    def primEquals(x: ##flix.runtime.spt.sandbox.text.NormalizerForm, y: ##flix.runtime.spt.sandbox.text.NormalizerForm): Bool =
        import java.lang.Object.equals(##java.lang.Object);
        let x1 = x as ##java.lang.Object;
        let y1 = y as ##java.lang.Object;
        equals(x1, y1) as & Pure


    pub def normalize(src: String, form: NormalizerForm): String =
        import flix.runtime.spt.sandbox.text.NormalizerWrapper:normalize(String, ##flix.runtime.spt.sandbox.text.NormalizerForm);
        try {
            normalize(src, marshal(form)) as & Pure
        } catch {
            case _:##java.lang.Exception => ""
        }
}