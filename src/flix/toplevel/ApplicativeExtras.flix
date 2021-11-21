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

/// Temp
/// See: https://hackage.haskell.org/package/semigroups-0.8.4.1/docs/Data-Semigroup.html#t:Option

namespace ApplicativeExtras {

    use FunctorExtras.{<&>};
    use Applicative.{point};

    ///  Applicative cons.
    def consA(x: m[a], l: m[List[a]]): m[List[a]] with Applicative[m] =
        (((x1, xs1) -> x1 :: xs1) <&> x) <*> l


    /// def constantA(x: m[a], y: m[b]): m[a] with Applicative[m] =
    ///     (((x1, _) -> x1) <&> x) <*> y

    pub def seqLeft(ma: m[a], mb : m[b]): m[a] with Applicative[m] =
        Applicative.liftA2((a, _) -> a, ma, mb)

    pub def seqRight(ma: m[a], mb : m[b]): m[b] with Applicative[m] =
        Applicative.liftA2((_, b) -> b, ma, mb)

    pub def <*>(mf: m[a -> b & ef], ma: m[a]): m[b] & ef with Applicative[m] = Applicative.ap(mf, ma)

    pub def <**>(ma : m[a], mf: m[a -> b & ef]): m[b] & ef with Applicative[m] = Applicative.liftA2((a, f) -> f(a), ma, mf)

    pub def <*(ma: m[a], mb : m[b]): m[a] with Applicative[m] = seqLeft(ma, mb)

    pub def *>(ma: m[a], mb : m[b]): m[b] with Applicative[m] = seqRight(ma, mb)

    /// Should this be in List namespace?
    pub def replicateA(i: Int32, mx: m[a]): m[List[a]]  with Applicative[m] =
        replicateAHelper(i, mx, point(Nil))

    def replicateAHelper(i: Int32, mx: m[a], ac: m[List[a]]): m[List[a]] with Applicative[m] =
        if (i <= 0)
            ac
        else
            replicateAHelper(i - 1, mx, consA(mx, ac))

    /// aka `replicateM_`
    pub def ntimes(i: Int32, mx: m[a]): m[Unit] with Applicative[m] =
        ntimesHelper(i, mx, point(()))

    /// Helper for `ntimes`.
    ///
    /// Must ensure that `mx` is evaluated at each step and the function is tail recursive,
    /// so we use an accumulator (even though it doesn't change).
    ///
    def ntimesHelper(i: Int32, mx: m[a], ac: m[Unit]): m[Unit] with Applicative[m] =
        if (i <= 0)
            ac
        else
            ntimesHelper(i - 1, mx, seqLeft(ac, mx))


    /// Should this be in the List namespace?
    pub def filterA(f: a -> m[Bool], l: List[a]): m[List[a]]  with Applicative[m] =
        filterAHelper(f, l, mx -> mx)

    ///
    /// Helper function for `filterA`.
    ///
    /// Implementation is quite tricky due to not using bind/flatMap (so we can Applicative)
    /// and also being CPS/tail recursive.
    ///
    def filterAHelper(f: a -> m[Bool], l: List[a], k: m[List[a]] -> m[List[a]]): m[List[a]] with Applicative[m] =
        match l {
            case Nil => k(point(Nil))
            case x :: rs => filterAHelper(f, rs, ks -> k(consWhenTrue(f(x), x, ks)))
        }

    ///
    /// Helper function for `filterA`.
    /// Conditional cons - cons `x` only if `test` is true.
    ///
    def consWhenTrue(test: m[Bool], x: a, l: m[List[a]]): m[List[a]] with Applicative[m] =
        (((ans, xs1) -> if (ans) (x :: xs1) else xs1) <&> test) <*> l


    pub def whenTrue(x: Bool, f: m[Unit]): m[Unit] with Applicative[m] =
        if (x) f else point(())

    pub def whenFalse(x: Bool, f: m[Unit]): m[Unit] with Applicative[m] =
        if (not x) f else point(())

}
