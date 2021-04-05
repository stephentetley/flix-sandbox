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

import java.text.Normalizer;

public class NormalizerWrapper {

    /// Use our own `NormalizerFormEnum` because Java's `Normalizer.Form` is
    /// and inner class (enum) and is not accessible to Flix.
    public static String normalize(String src, NormalizerFormEnum normalForm) {
        Normalizer.Form nf = NormalizerFormEnum.convertToInner(normalForm);
        return Normalizer.normalize(src, nf);
    }

    public static boolean isNormalized(String src, NormalizerFormEnum normalForm) {
        Normalizer.Form nf = NormalizerFormEnum.convertToInner(normalForm);
        return Normalizer.isNormalized(src, nf);
    }

}
