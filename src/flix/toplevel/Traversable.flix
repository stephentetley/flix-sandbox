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

pub lawless class Traversable[t : Type -> Type] with Functor[t] {

    pub def traverse(f: a -> m[b] & ef, c: t[a]): m[t[b]] & ef with Applicative[m]

    pub def sequence(c: t[m[a]]): m[t[a]] with Applicative[m]

}

instance Traversable[List] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Applicative[m] = ListHelper.traverse(f, t)

    pub def sequence(t: List[m[a]]): m[List[a]] with Applicative[m] = ListHelper.sequence(t)

}

instance Traversable[Option] {
    pub def traverse(f: a -> m[b] & ef, t: Option[a]): m[Option[b]] & ef with Applicative[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match t {
            case None => point(None)
            case Some(a) => Some <&> f(a)
        }

    pub def sequence(t: Option[m[a]]): m[Option[a]] with Applicative[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match t {
            case Some(ma) => Some <&> ma
            case None => point(None)
        }
}

instance Traversable[Either[e]] {
    pub def traverse(f: a -> m[b] & ef, t: Either[e, a]): m[Either[e, b]] & ef with Applicative[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match t {
            case Left(e) => point(Left(e))
            case Right(a) => Right <&> f(a)
        }

    pub def sequence(t: Either[e, m[a]]): m[Either[e, a]] with Applicative[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match t {
            case Left(e) => point(Left(e))
            case Right(ma) => Right <&> ma
        }
}

namespace ListHelper {
    use ApplicativeExtras.{<*>};
    use FunctorExtras.{<&>};

    pub def traverse(f: a -> m[b] & ef, xs: List[a]): m[List[b]] & ef with Applicative[m] =
        traverseHelper(f, xs, ks -> ks as & ef)

    pub def traverseHelper(f: a -> m[b] & ef, xs: List[a], k: m[List[b]] -> m[List[b]] & ef): m[List[b]] & ef with Applicative[m] =
        match xs {
            case Nil => k(Applicative.point(Nil)) as & ef
            case x :: rs => traverseHelper(f, rs, ks -> k(consA(f(x), ks)))
        }

    def consA(mx: f[a], mxs: f[List[a]]): f[List[a]] with Applicative[f] =
        ((x, xs) -> x :: xs) <&> mx <*> mxs


    pub def sequence(xs: List[m[a]]): m[List[a]] with Applicative[m] = sequenceHelper(xs, ks -> ks)

    pub def sequenceHelper(xs: List[m[a]], k: m[List[a]] -> m[List[a]]): m[List[a]] with Applicative[m] =
        match xs {
            case Nil => k(Applicative.point(Nil))
            case mx :: rs => sequenceHelper(rs, ks -> k(consA(mx, ks)))
        }

    pub def sequenceX(xs: List[m[a]]): m[Unit] with Applicative[m] = sequenceXHelper(xs, k -> k)

    pub def sequenceXHelper(xs: List[m[a]], k: m[Unit] -> m[Unit]): m[Unit] with Applicative[m] =
        use ApplicativeExtras.{seqRight};
        match xs {
            case Nil => k(Applicative.point(()))
            case mx :: rs => sequenceXHelper(rs, ka -> k(seqRight(mx, ka)))
        }

}

