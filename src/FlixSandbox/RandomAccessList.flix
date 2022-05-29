/*
 * Copyright 2022 Stephen Tetley
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


///
/// The RandomAccessList type.
///


namespace FlixSandbox/RandomAccessList {

    enum Tree[a] with Eq, Order, ToString {
        case Leaf(a)
        case Node(a, Tree[a], Tree[a])
    }

    pub opaque type RandomAccessList[a] = List[(Int32, Tree[a])]

    type alias RandomAccessListBody[a] = List[(Int32, Tree[a])]

    def getRandomAccessList(l: RandomAccessList[a]): List[(Int32, Tree[a])] = 
        let RandomAccessList(l1) = l;
        l1

    instance Functor[Tree] {
        pub def map(f: a -> b & ef, t: Tree[a]): Tree[b] & ef = 
            def loop(tree, k) = match tree {
                case Leaf(a)         => {let ans = Leaf(f(a)); k(ans)}
                case Node(a, t1, t2) => {
                    let b = f(a);
                    loop(t1, v1 ->
                        loop(t2, v2 ->
                            k(Node(b, v1, v2))))
                }
            };
            loop(t, k -> k as & ef)
    }

    instance Functor[RandomAccessList] {
        pub def map(f: a -> b & ef, l: RandomAccessList[a]): RandomAccessList[b] & ef = 
            let RandomAccessList(l1) = l;
            Functor.map(x -> {let (i, t) = x; (i, Functor.map(f, t))}, l1) |> RandomAccessList
    }

    pub def cons(x: a, l: RandomAccessList[a]): RandomAccessList[a] = match getRandomAccessList(l) {
        case (w1, t1) :: (w2, t2) :: rs if (w1 == w2) => RandomAccessList((1 + w1 + w2, Node(x, t1, t2)) :: rs)
        case ts                                       => RandomAccessList((1, Leaf(x)) :: ts)
    }

    pub def head(l: RandomAccessList[a]): Option[a] = match getRandomAccessList(l) {
        case (1, Leaf(x)) :: _          => Some(x)
        case (_, Node(x, _, _)) :: _    => Some(x)
        case _                          => None
    }

    pub def tail(l: RandomAccessList[a]): RandomAccessList[a] = match getRandomAccessList(l) {
        case (1, Leaf(_)) :: ts         => RandomAccessList(ts)
        case (w, Node(_, t1, t2)) :: ts => RandomAccessList((w / 2, t1) :: (w / 2, t2) :: ts)
        case _                          => RandomAccessList(Nil)
    }

    /// Should be tail recursive...
    def lookupTree(weight: Int32, idx: Int32, tree: Tree[a]): Option[a] = match (weight, idx, tree) {
        case (1, 0, Leaf(x))         => Some(x)
        case (_, _, Leaf(_))         => None
        case (_, 0, Node(x, _, _))   => Some(x)
        case (w, i, Node(_, t1, t2)) => 
            if (i <= w/2) 
                lookupTree(w/2, i-1, t1)
            else
                lookupTree(w/2, i - 1 - w / 2, t2)
    }

    pub def lookup(i: Int32, l: RandomAccessList[a]): Option[a] = match getRandomAccessList(l) {
        case Nil          => None
        case (w, t) :: ts => 
            if (i < w)
                lookupTree(w, i, t)
            else
                lookup(i - w, RandomAccessList(ts))
    }

    /// CPS converted
    def updateTree(weight: Int32, idx: Int32, y: a, tree: Tree[a], k: Tree[a] -> Tree[a]): Tree[a] = match (weight, idx, tree) {
        case (1, 0, Leaf(_))         => k(Leaf(y))
        case (_, _, Leaf(x))         => k(Leaf(x))
        case (_, 0, Node(_, t1, t2)) => k(Node(y, t1, t2))
        case (w, i, Node(x, t1, t2)) => 
            if (i <= w/2) 
                updateTree(w/2, i-1, y, t1, kt -> k(Node(x, kt, t2)))
            else
                updateTree(w/2, i - 1 - w / 2, y, t2, kt -> k(Node(x, t1, kt)))
    }

    def updateHelper(i: Int32, y: a, l: RandomAccessListBody[a], 
                        k: RandomAccessListBody[a] -> RandomAccessListBody[a]): RandomAccessListBody[a] = match l {
        case Nil          => k(Nil)
        case (w, t) :: ts => 
            if (i < w) {
                let t1 = updateTree(w, i, y, t, identity);
                k((w, t1) :: ts)
            } else
                updateHelper(i - w, y, ts, ks -> k((w, t) :: ks))
    }


    ///
    /// Updates `l` at index `i` with `y`. if `i` is out-of-bounds, returns `l`.
    ///
    pub def update(i: Int32, y: a, l: RandomAccessList[a]): RandomAccessList[a] = 
        let RandomAccessList(l1) = l;
        updateHelper(i, y, l1, identity) |> RandomAccessList

}