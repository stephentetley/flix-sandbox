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

}

instance Traversable[List] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, c: List[a]): m[List[b]] & ef with Applicative[m] = ListHelper.traverse(f, c)
}

instance Traversable[Option] {
    pub def traverse(f: a -> m[b] & ef, c: Option[a]): m[Option[b]] & ef with Applicative[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match c {
            case None => point(None)
            case Some(a) => Some <&> f(a)
        }
}

instance Traversable[Either[e]] {
    pub def traverse(f: a -> m[b] & ef, c: Either[e, a]): m[Either[e, b]] & ef with Applicative[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match c {
            case Left(e) => point(Left(e))
            case Right(a) => Right <&> f(a)
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

}

