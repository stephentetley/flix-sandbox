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

    pub def mapM(f: a -> m[b] & ef, c: t[a]): m[t[b]] & ef with Monad[m]

    /// This would be better placed in Foldable...
    pub def foreachM(f: a -> m[b] & ef, c: t[a]): m[Unit] & ef with Monad[m]

}

instance Traversable[List] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Applicative[m] = ListHelper.traverse(f, t)

    pub def sequence(t: List[m[a]]): m[List[a]] with Applicative[m] = ListHelper.sequence(t)

    pub def mapM(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Monad[m] = ListHelper.mapM(f, t)

    pub def foreachM(f: a -> m[b] & ef, t: List[a]): m[Unit] & ef with Monad[m] = ListHelper.foreachM(f, t)

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

    pub def mapM(f: a -> m[b] & ef, t: Option[a]): m[Option[b]] & ef with Monad[m] =
        use Applicative.{point};
        use FunctorExtras.{<&>};
        match t {
            case None => point(None)
            case Some(a) => Some <&> f(a)
        }

    pub def foreachM(f: a -> m[b] & ef, t: Option[a]): m[Unit] & ef with Monad[m] =
        use Applicative.{point};
        match t {
            case None => point(())
            case Some(a) => Monad.flatMap(_ -> point(()), f(a))
        }
}

namespace Traversable {

    pub def mapAccumLeft(f: acc -> a -> (acc, b), start: acc, t: t[a]): (acc, t[b]) with Traversable[t] =
        runStateL(traverse(x1 -> StateL(flip(f, x1)), t), start)

    opaque type StateL[s, a] = s -> (s, a)

    instance Functor[StateL[s]] {
        pub def map(f: a -> b & ef, x: StateL[s, a]): StateL[s, b] & ef=
            let StateL(x1) = x;
            StateL(s ->
                let (s1, v1) = x1(s);
                (s1, f(v1) as & Pure)
            ) as & ef
    }

    instance Applicative[StateL[s]] {
        pub def point(x: a): StateL[s, a] = StateL(s -> (s, x))

        pub def ap(f: StateL[s, a -> b & ef], x: StateL[s, a]): StateL[s, b] & ef =
            let StateL(f1) = f;
            let StateL(x1) = x;
            StateL(s ->
                let (s1, f2) = f1(s);
                let (s2, v) = x1(s1);
                (s2, f2(v) as & Pure)
            ) as & ef
    }

    def runStateL(x: StateL[s, a], state: s): (s, a) =
        let StateL(x1) = x;
        x1(state)

    pub def mapAccumRight(f: acc -> a -> (acc, b), start: acc, t: t[a]): (acc, t[b]) with Traversable[t] =
        runStateR(traverse(x1 -> StateR(flip(f, x1)), t), start)

    opaque type StateR[s, a] = s -> (s, a)

    instance Functor[StateR[s]] {
        pub def map(f: a -> b & ef, x: StateR[s, a]): StateR[s, b] & ef =
            let StateR(x1) = x;
            StateR(s ->
                let (s1, v1) = x1(s);
                (s1, f(v1) as & Pure)
            ) as & ef
    }

    instance Applicative[StateR[s]] {
        pub def point(x: a): StateL[s, a] = StateL(s -> (s, x))

        pub def ap(f: StateR[s, a -> b & ef], x: StateR[s, a]): StateR[s, b] & ef=
            let StateR(f1) = f;
            let StateR(x1) = x;
            StateR(s ->
                /// Eval order is reverse of StateL...
                let (s1, v) = x1(s);
                let (s2, f2) = f1(s1);
                (s2, f2(v) as & Pure)
            ) as & ef
    }

    def runStateR(x: StateR[s, a], state: s): (s, a) =
        let StateR(x1) = x;
        x1(state)

}

/// To be added to List...
namespace ListHelper {
    use ApplicativeExtras.{<*>};
    use FunctorExtras.{<&>};

    pub def traverse(f: a -> m[b] & ef, xs: List[a]): m[List[b]] & ef with Applicative[m] =
        traverseHelper(f, xs, ks -> ks as & ef)

    def traverseHelper(f: a -> m[b] & ef, xs: List[a], k: m[List[b]] -> m[List[b]] & ef): m[List[b]] & ef with Applicative[m] =
        match xs {
            case Nil => k(Applicative.point(Nil)) as & ef
            case x :: rs => traverseHelper(f, rs, ks -> k(consA(f(x), ks)))
        }

    def consA(mx: f[a], mxs: f[List[a]]): f[List[a]] with Applicative[f] =
        ((x, xs) -> x :: xs) <&> mx <*> mxs


    pub def sequence(xs: List[m[a]]): m[List[a]] with Applicative[m] = sequenceHelper(xs, ks -> ks)

    def sequenceHelper(xs: List[m[a]], k: m[List[a]] -> m[List[a]]): m[List[a]] with Applicative[m] =
        match xs {
            case Nil => k(Applicative.point(Nil))
            case mx :: rs => sequenceHelper(rs, ks -> k(consA(mx, ks)))
        }

    pub def sequenceX(xs: List[m[a]]): m[Unit] with Applicative[m] = sequenceXHelper(xs, k -> k)

    def sequenceXHelper(xs: List[m[a]], k: m[Unit] -> m[Unit]): m[Unit] with Applicative[m] =
        use ApplicativeExtras.{seqRight};
        match xs {
            case Nil => k(Applicative.point(()))
            case mx :: rs => sequenceXHelper(rs, ka -> k(seqRight(mx, ka)))
        }

    pub def mapM(f: a -> m[b] & ef, xs: List[a]): m[List[b]] & ef with Monad[m] =
        mapMHelper(f, xs, ks -> ks as & ef)

    def mapMHelper(f: a -> m[b] & ef, xs: List[a], k: m[List[b]] -> m[List[b]] & ef): m[List[b]] & ef with Monad[m] =
        match xs {
            case Nil => k(Applicative.point(Nil)) as & ef
            case x :: rs => mapMHelper(f, rs, ks -> k(consA(f(x), ks)))
        }

    pub def foreachM(f: a -> m[b] & ef, xs: List[a]): m[Unit] & ef with Monad[m] =
        foreachMHelper(f, xs, ku -> ku as & ef)

    pub def foreachMHelper(f: a -> m[b] & ef, xs: List[a], k: m[Unit] -> m[Unit] & ef): m[Unit] & ef with Monad[m] =
        match xs {
            case Nil => k(Applicative.point(())) as & ef
            case x :: rs => foreachMHelper(f, rs, ku -> k(Monad.flatMap(_ -> ku, f(x))))
        }

}

