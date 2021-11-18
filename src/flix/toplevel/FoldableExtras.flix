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


namespace FoldableExtras {


    pub def sequenceX(t: t[m[a]]): m[Unit] with Applicative[m], Foldable[t] =
        use ApplicativeExtras.{*>};
        Foldable.foldLeft((mb, ma) -> ma *> mb, Applicative.point(()), t)

    pub def traverseX(f: a -> m[b] & ef, t: t[a]): m[Unit] & ef with Foldable[t], Applicative[m] =
        use ApplicativeExtras.{*>};
        Foldable.foldLeft((mb, a) -> f(a) *> mb, Applicative.point(()), t)

    pub def forX(t: t[a], f: a -> m[b] & ef): m[Unit] & ef with Foldable[t], Applicative[m] = traverseX(f, t)

    pub def asum(t: t[m[a]]): m[a] with Foldable[t], Alternative[m] =
        use Alternative.{<|>};
        Foldable.foldLeft((mb, ma) -> mb <|> ma, Alternative.zero(), t)

    pub def concat(t: t[List[a]]): List[a] with Foldable[t] =
        let makeDList = xs -> (ys -> List.append(xs, ys));
        let appendDList = (f, g) -> xs -> f(g(xs));
        let unDList = f -> f(Nil);
        Foldable.foldLeft((ac, xs) -> appendDList(ac, makeDList(xs)), xs -> xs, t) |> unDList

}