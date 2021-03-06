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

package flix.runtime.spt.sandbox.text;

/// Provide our own enum rather because Normalizer.Form is an inner class

import java.text.Normalizer;

public enum NormalizerFormEnum {
    FLIX_NFC,
    FLIX_NFD,
    FLIX_NFKC,
    FLIX_NFKD;

    public static Normalizer.Form convertToInner(NormalizerFormEnum form) {
        switch(form) {
            case FLIX_NFC:
                return Normalizer.Form.NFC;
            case FLIX_NFD:
                return Normalizer.Form.NFD;
            case FLIX_NFKC:
                return Normalizer.Form.NFKC;
            default:
                return Normalizer.Form.NFKD;
        }
    }

}

