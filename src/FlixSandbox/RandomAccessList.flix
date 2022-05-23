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

    enum Tree[a] {
        case Leaf(a)
        case Node(a, Tree[a], Tree[a])
    }

    pub opaque type RandomAccessList[a] = List[(Int32, Tree[a])]

    def getRandomAccessList(l: RandomAccessList[a]): List[(Int32, Tree[a])] = 
        let RandomAccessList(l1) = l;
        l1

    pub def cons(x: a, l: RandomAccessList[a]): RandomAccessList[a] = match getRandomAccessList(l) {
        case (w1, t1) :: (w2, t2) :: rs if (w1 == w2) => RandomAccessList((1 + w1 + w2, Node(x, t1, t2)) :: rs)
        case ts                                       => RandomAccessList((1, Leaf(x)) :: ts)
    }

}