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


namespace Doodle {

    pub enum ImmArray[a: Type](Array[a, Static])

    instance ToString [ImmArray[a]] with ToString[a] {
        pub def toString(arr: ImmArray[a]): String = 
            let ImmArray(arr1) = arr;
            Array.toString(arr1) as \ {}
    }

}

namespace Doodle/ImmArray {

    use Doodle.ImmArray;
    use Doodle.ImmArray.{ImmArray};

    pub def fromArray(arr: Array[a, r]): ImmArray[a] \ Read(r) = fromArrayHelper(arr) as \ { Read(r) }

    def fromArrayHelper(arr: Array[a, r]): ImmArray[a] \ IO = 
        let len = Array.length(arr);
        if (len < 1)
            ImmArray([] @ Static)
        else {
            let first = arr[0];
            let arr1 = [first; len] @ Static;
            Array.foreachWithIndex((x, i) -> arr1[i] = x, arr);
            ImmArray(arr1)
        }

    pub def get(i: Int32, ia: ImmArray[a]): a = 
        let ImmArray(ia1) = ia;
        ia1[i] as \ {}


    pub def length(ia: ImmArray[a]): Int32 = 
        let ImmArray(ia1) = ia;
        Array.length(ia1)

    pub def map(f: a -> b \ ef, ia: ImmArray[a]): ImmArray[b] \ ef =  mapHelper(f, ia) as \ ef
    
    def mapHelper(f: a -> b \ ef, ia: ImmArray[a]): ImmArray[b] \ { IO, ef } = 
        let len = length(ia);
        if (len < 1)
            ImmArray([] @ Static)
        else {
            let first = f(get(0, ia));
            let arr1 = [first; len] @ Static;
            foreachWithIndex((x, i) -> arr1[i] = f(x), ia);
            ImmArray(arr1)
        }

    pub def foreachWithIndex(f: (a, Int32) -> Unit \ ef, ia: ImmArray[a]): Unit \ ef =
        let ImmArray(ia1) = ia;
        Array.foreachWithIndex(f, ia1) as \ ef

}
