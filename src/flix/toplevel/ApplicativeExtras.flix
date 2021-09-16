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


namespace FunctorExtras {

    pub def ignore(ma: f[a]): f[Unit] with Functor[f] = ma `Functor.as` ()

    pub def <&>(f: a -> b & ef, ma: f[a]): f[b] & ef with Functor[f] = Functor.map(f, ma)

    pub def <&(x: a, ma: f[b]): f[a] with Functor[f] = ma `Functor.as` x

    pub def &>(ma: f[a], x: b): f[b] with Functor[f] = ma `Functor.as` x

}


namespace ApplicativeExtras {

    pub def seqLeft(ma: m[a], mb : m[b]): m[a] with Applicative[m] =
        Applicative.liftA2((a, _) -> a, ma, mb)

    pub def seqRight(ma: m[a], mb : m[b]): m[b] with Applicative[m] =
        Applicative.liftA2((_, b) -> b, ma, mb)

    pub def <*>(mf: m[a -> b & ef], ma: m[a]): m[b] & ef with Applicative[m] = Applicative.ap(mf, ma)

    pub def <**>(ma : m[a], mf: m[a -> b & ef]): m[b] & ef with Applicative[m] = Applicative.liftA2((a, f) -> f(a), ma, mf)

    pub def <*(ma: m[a], mb : m[b]): m[a] with Applicative[m] = seqLeft(ma, mb)

    pub def *>(ma: m[a], mb : m[b]): m[b] with Applicative[m] = seqRight(ma, mb)

}

namespace FoldableExtras {

    pub def sequenceX(c: t[m[a]]): m[Unit] with Applicative[m], Foldable[t] =
        use ApplicativeExtras.{*>};
        Foldable.foldLeft((mb, ma) -> ma *> mb, Applicative.point(()), c)
}