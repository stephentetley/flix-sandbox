/*
 * Copyright 2021 Stephen Tetley
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


namespace ApplicativeExtras {

    use FunctorExtras.{<&>};
    use Applicative.{point};

    ///  Applicative cons.
    def consA(f: m[a], fs: m[List[a]]): m[List[a]] with Applicative[m] =
        (((x, xs) -> x :: xs) <&> f) <*> fs


    pub def seqLeft(ma: m[a], mb : m[b]): m[a] with Applicative[m] =
        Applicative.liftA2((a, _) -> a, ma, mb)

    pub def seqRight(ma: m[a], mb : m[b]): m[b] with Applicative[m] =
        Applicative.liftA2((_, b) -> b, ma, mb)

    pub def <*>(mf: m[a -> b & ef], ma: m[a]): m[b] & ef with Applicative[m] = Applicative.ap(mf, ma)

    pub def <**>(ma : m[a], mf: m[a -> b & ef]): m[b] & ef with Applicative[m] = Applicative.liftA2((a, f) -> f(a), ma, mf)

    pub def <*(ma: m[a], mb : m[b]): m[a] with Applicative[m] = seqLeft(ma, mb)

    pub def *>(ma: m[a], mb : m[b]): m[b] with Applicative[m] = seqRight(ma, mb)

    pub def replicateA(ntimes: Int32, mx: m[a]): m[List[a]]  with Applicative[m] =
        replicateAHelper(ntimes, mx, point(Nil))

    def replicateAHelper(ntimes: Int32, mx: m[a], ac: m[List[a]]): m[List[a]] with Applicative[m] =
        if (ntimes <= 0)
            ac
        else
            replicateAHelper(ntimes - 1, mx, consA(mx, ac))

}
