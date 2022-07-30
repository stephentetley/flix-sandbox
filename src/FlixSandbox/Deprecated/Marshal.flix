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

// DEPRECATED - use `collection-interop` instead

namespace FlixSandbox/Deprecated/Marshal {

    pub def marshalInt8(i: Int8): ##java.lang.Byte = 
        import static java.lang.Byte.valueOf(Int8): ##java.lang.Byte & Pure;
        valueOf(i)

    pub def unmarshalInt8(i: ##java.lang.Byte): Int8 = 
        import java.lang.Byte.byteValue(): Int8 & Pure;
        byteValue(i)

    pub def marshalInt16(i: Int16): ##java.lang.Short = 
        import static java.lang.Short.valueOf(Int16): ##java.lang.Short & Pure;
        valueOf(i)

    pub def unmarshalInt16(i: ##java.lang.Short): Int16 = 
        import java.lang.Short.shortValue(): Int16 & Pure;
        shortValue(i)


    pub def marshalInt32(i: Int32): ##java.lang.Integer = 
        import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer & Pure;
        valueOf(i)

    pub def unmarshalInt32(i: ##java.lang.Integer): Int32 = 
        import java.lang.Integer.intValue(): Int32 & Pure;
        intValue(i)


    pub def marshalInt64(i: Int64): ##java.lang.Long = 
        import static java.lang.Long.valueOf(Int64): ##java.lang.Long & Pure;
        valueOf(i)

    pub def unmarshalInt64(i: ##java.lang.Long): Int64 = 
        import java.lang.Long.longValue(): Int64 & Pure;
        longValue(i)


    pub def marshalFloat32(d: Float32): ##java.lang.Float = 
        import static java.lang.Float.valueOf(Float32): ##java.lang.Float & Pure;
        valueOf(d)

    pub def unmarshalFloat32(d: ##java.lang.Float): Float32 = 
        import java.lang.Float.floatValue(): Float32 & Pure;
        floatValue(d)

    pub def marshalFloat64(d: Float64): ##java.lang.Double = 
        import static java.lang.Double.valueOf(Float64): ##java.lang.Double & Pure;
        valueOf(d)

    pub def unmarshalFloat64(d: ##java.lang.Double): Float64 = 
        import java.lang.Double.doubleValue(): Float64 & Pure;
        doubleValue(d)

    pub def marshalChar(c: Char): ##java.lang.Character = 
        import static java.lang.Character.valueOf(Char): ##java.lang.Character & Pure;
        valueOf(c)

    pub def unmarshalChar(c: ##java.lang.Character): Char = 
        import java.lang.Character.charValue(): Char & Pure;
        charValue(c)


    /// This is useful but (a) what to call it? (b) are there similar functions
    /// to make including it worthwile? Otherwise it doesn't really pass the Fairbairn 
    /// threshold.
    ///
    /// pub def unmarshalString(o: ##java.lang.Object): String = 
    ///     if (Object.isNull(o))
    ///         ""
    ///     else
    ///         o as String


    pub def unmarshalOptional(downcast: ##java.lang.Object -> a & ef, opt: ##java.util.Optional): Option[a] & ef = 
        import java.util.Optional.isPresent(): Bool & Pure;
        import java.util.Optional.get(): ##java.lang.Object & Pure;
        if (isPresent(opt))
            get(opt) |> downcast |> Some
        else 
            None

    pub def marshalOptional(upcast: a -> ##java.lang.Object & ef, o: Option[a]): ##java.util.Optional & ef =
        import static java.util.Optional.of(##java.lang.Object): ##java.util.Optional & Pure;
        import static java.util.Optional.empty(): ##java.util.Optional & Pure;
        match o { 
            case Some(a) => of(upcast(a))
            case None    => empty()
        }

        
    /// NOTE caller decides whether to wrap with Err...
    
    pub def unmarshalExceptionMessage(ex: ##java.lang.Exception): String & Impure = 
        import java.lang.Throwable.getMessage(): String & Impure;
        getMessage(ex as ##java.lang.Throwable)

    pub def unmarshalIOExceptionMessage(ex: ##java.io.IOException): String & Impure = 
        import java.lang.Throwable.getMessage(): String & Impure;
        getMessage(ex as ##java.lang.Throwable)


    pub def marshalArray(upcast: a -> ##java.lang.Object & ef, arr: Array[a, r]): Array[##java.lang.Object, r] \ {Read(r), Write(r), ef}=
        import java.util.ArrayList.toArray(): Array[##java.lang.Object, r] & r;    
        let step = iter -> match Iterator.next(iter) {
            case Some(x)    => Some((upcast(x), iter))
            case None       => None
        };
        unfoldArrayList(step, Array.iterator(arr)) |> toArray
        

    pub def marshalSet(upcast: a -> ##java.lang.Object & ef, s: Set[a]): ##java.util.Set & ef =
        region r1 {
            let step = iter -> match Iterator.next(iter) {
                case Some(x)    => Some((upcast(x), iter))
                case None       => None
            };
            unfoldHashSet(step, Set.iterator(r1, s)) as ##java.util.Set & ef
        }

    pub def unmarshalSet(downcast: ##java.lang.Object -> a & ef, javas: ##java.util.Set): Set[a] & ef with Order[a] =
        let step = (acc, obj) -> {
            let x1 = downcast(obj);
            Set.insert(x1, acc)
        };
        foldSet(step, Set.empty(), javas)

    pub def marshalList(upcast: a -> ##java.lang.Object & ef, l: List[a]): ##java.util.List & ef =
        let step = xs -> match xs {
            case x :: rs    => Some((upcast(x), rs))
            case Nil        => None
        };
        unfoldArrayList(step, l) as ##java.util.List

    pub def unmarshalList(downcast: ##java.lang.Object -> a & ef, javal: ##java.util.List): List[a] & ef =
        let step = (k, obj) -> {
            let x1 = downcast(obj);
            (ks -> k(x1 :: ks))
        };
        foldList(step, ks -> ks, javal)  |> f -> f(Nil)


    pub def marshalMap(upcastKey: k -> ##java.lang.Object & ef1, 
                        upcastValue: v -> ##java.lang.Object & ef2,  
                        m: Map[k, v]): ##java.util.Map & (ef1 and ef2) =
        region r1 {
            let step = iter -> match Iterator.next(iter) {
                case Some(k, v)   => Some((upcastKey(k), upcastValue(v), iter))
                case None         => None
            };
            unfoldHashMap(step, Map.iterator(r1, m)) as ##java.util.Map & (ef1 and ef2)
        }

    ///
    /// We don't need the inverse function `marshalMapEntry` because Java's API for building maps is `m.put(k, v)`
    /// so we never need to create a MapEntry.
    ///
    pub def unmarshalMapEntry(downcastKey: ##java.lang.Object -> k & ef1, 
                                downcastValue: ##java.lang.Object -> v & ef2, 
                                entry: ##java.util.Map$Entry): (k, v) & (ef1 and ef2) =
        import java.util.Map$Entry.getKey(): ##java.lang.Object & Pure;
        import java.util.Map$Entry.getValue(): ##java.lang.Object & Pure;        
        let k = getKey(entry);
        let v = getValue(entry);                
        let k1 = downcastKey(k);
        let v1 = downcastValue(v);
        (k1, v1)

    pub def unmarshalMap(downcastKey: ##java.lang.Object -> k & ef1, 
                            downcastValue: ##java.lang.Object -> v & ef2, 
                            javam: ##java.util.Map): Map[k, v] & (ef1 and ef2) with Order[k] =
        let step = (acc, entry) -> {
            let (k, v) = unmarshalMapEntry(downcastKey, downcastValue, entry);
            Map.insert(k, v, acc)
        };
        foldMap(step, Map.empty(), javam as ##java.util.Map) 

    pub def marshalSortedMap(upcastKey: k -> ##java.lang.Object & ef1,
                                upcastValue: v -> ##java.lang.Object & ef2, 
                                m: Map[k, v]): ##java.util.SortedMap & (ef1 and ef2) =
        region r1 {
            let step = iter -> match Iterator.next(iter) {
                case Some(k, v)   => Some((upcastKey(k), upcastValue(v), iter))
                case None         => None
            };
            unfoldTreeMap(step, Map.iterator(r1, m)) as ##java.util.SortedMap & (ef1 and ef2)
        }


    pub def unmarshalSortedMap(downcastKey: ##java.lang.Object -> k & ef, 
                                downcastValue: ##java.lang.Object -> v & ef, 
                                javam: ##java.util.SortedMap): Map[k, v] & ef with Order[k] =
        let step = (acc, entry) -> {
            let (k, v) = unmarshalMapEntry(downcastKey, downcastValue, entry);
            Map.insert(k, v, acc)
        };
        foldMap(step, Map.empty(), javam as ##java.util.Map)



    pub def unmarshalIterator(_: Region[r], 
                                downcast: ##java.lang.Object -> a & ef, 
                                javaIter: ##java.util.Iterator): Iterator[a, r] \ { Read(r), ef}  =
        import java.util.Iterator.hasNext(): Bool & r;
        import java.util.Iterator.next(): ##java.lang.Object & r;
        let isDone = () -> { 
            let ans = hasNext(javaIter);
            not ans
        };
        let getNext = () -> {
            let a1 = next(javaIter);
            downcast(a1) as \ Read(r)
        };
        Iterator(isDone, getNext) as \ {Read(r), ef}


    pub def unmarshalStream(r: Region[r], downcast: ##java.lang.Object -> a & ef, strm: ##java.util.stream.Stream): Iterator[a, r] \ { Read(r), ef }  =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & r;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        unmarshalIterator(r, downcast, javaIter)

    pub def unmarshalIntStream(r: Region[r], strm: ##java.util.stream.IntStream): Iterator[Int32, r] \ Read(r)  =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & r;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        let downcast = obj -> unmarshalInt32(obj as ##java.lang.Integer);
        unmarshalIterator(r, downcast, javaIter)

    pub def unmarshalLongStream(r: Region[r], strm: ##java.util.stream.LongStream): Iterator[Int64, r] \ Read(r)  =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & r;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        let downcast = obj -> unmarshalInt64(obj as ##java.lang.Long);
        unmarshalIterator(r, downcast, javaIter)

    pub def unmarshalDoubleStream(r: Region[r], strm: ##java.util.stream.DoubleStream): Iterator[Float64, r] \ Read(r) =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & r;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        let downcast = obj -> unmarshalFloat64(obj as ##java.lang.Double);
        unmarshalIterator(r, downcast, javaIter)



    pub def marshalListToSet(upcast: a -> ##java.lang.Object & ef, l: List[a]): ##java.util.Set & ef =
        let step = xs -> match xs {
            case x :: rs => Some((upcast(x), rs))
            case Nil     => None
        };
        unfoldHashSet(step, l) as ##java.util.Set


    /// Unmarshal a Java set to a Flix list - elements in the list will be unique (order is arbitrary).
    /// Unlike Flix Sets, Java Sets are not limited to ordered types, for unordered types we cannot unmarshal 
    /// Java.Set to Flix Set instead we unmarshal to a List.
    pub def unmarshalSetToList(downcast: ##java.lang.Object -> a & ef, javas: ##java.util.Set): List[a] & ef =
        let step = (k, obj) -> {
            let a1 = downcast(obj);
            (ks -> k(a1 :: ks))
        };
        foldSet(step, ks -> ks, javas) |> f -> f(Nil)

    pub def marshalAssocListToMap(upcastKey: k -> ##java.lang.Object & ef, 
                                    upcastValue: v -> ##java.lang.Object & ef, 
                                    l: List[(k, v)]): ##java.util.Map & ef =
        let step = xs -> match xs {
            case (k, v) :: rs => Some((upcastKey(k), upcastValue(v), rs))
            case Nil          => None
        };
        unfoldHashMap(step, l) as ##java.util.Map

    pub def unmarshalMapToAssocList(downcastEntry: ##java.util.Map$Entry -> (k, v) & ef, javam: ##java.util.Map): List[(k, v)] & ef =
        let step = (k, entry) -> {
            let kv = downcastEntry(entry);
            (ks -> k(kv :: ks))
        };
        foldMap(step, ks -> ks, javam) |> f -> f(Nil)


    pub def marshalListToArray(upcast: a -> ##java.lang.Object & ef, l: List[a]): Array[##java.lang.Object, r] & Impure =
        import java.util.ArrayList.toArray(): Array[##java.lang.Object, r] & r;
        let step = iter -> match Iterator.next(iter) {
            case Some(x)    => Some((upcast(x), iter))
            case None       => None
        };
        unfoldArrayList(step, List.iterator(Static, l)) |> toArray


    /// Due to change once Iterator uses regions...
    pub def marshalSetToArray(upcast: a -> ##java.lang.Object & ef, s: Set[a]): Array[##java.lang.Object, r] & Impure =
        import java.util.ArrayList.toArray(): Array[##java.lang.Object, r] & r;    
        let step = iter -> match Iterator.next(iter) {
            case Some(x)    => Some((upcast(x), iter))
            case None       => None
        };
        unfoldArrayList(step, Set.iterator(Static, s)) |> toArray

    /// Folds

    pub def foldIterator(f: (b, ##java.lang.Object) -> b & ef, s: b, javaIter: ##java.util.Iterator): b & Impure =
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        def loop(ac) = {
            if (hasNext(javaIter)) {
                let obj1 = next(javaIter);
                let ac1 = f(ac, obj1) as & Impure;
                loop(ac1)
            } else
                ac
        };
        loop(s)


    pub def foldList(f: (b, ##java.lang.Object) -> b & ef, s: b, javal: ##java.util.List): b & ef =
        foldListHelper(f, s, javal) as & ef

    def foldListHelper(f: (b, ##java.lang.Object) -> b & ef, s: b, javal: ##java.util.List): b & Impure =
        import java.util.List.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let iter = iterator(javal);
        def loop(ac) = {
            if (hasNext(iter)) {
                let obj1 = next(iter);
                let ac1 = f(ac, obj1) as & Impure;
                loop(ac1)
            } else
                ac
        };
        loop(s)


    pub def foldMap(f: (b, ##java.util.Map$Entry) -> b & ef, s: b, t: ##java.util.Map): b & ef = 
        foldMapHelper(f, s, t) as & ef
        
    def foldMapHelper(f: (b, ##java.util.Map$Entry) -> b & ef, s: b, t: ##java.util.Map): b & Impure = 
        import java.util.Map.entrySet(): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let set1 = entrySet(t);
        let iter = iterator(set1);
        def loop(ac) = {
            if (hasNext(iter)) {
                let entry = next(iter) as ##java.util.Map$Entry;
                let ac1 = f(ac, entry) as & Impure;
                loop(ac1)
            } else
                ac
        };
        loop(s)

    pub def foldSet(f: (b, ##java.lang.Object) -> b & ef, s: b, javas: ##java.util.Set): b & ef =
        foldSetHelper(f, s, javas) as & ef 

    def foldSetHelper(f: (b, ##java.lang.Object) -> b & ef, s: b, javas: ##java.util.Set): b & Impure =
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let iter = iterator(javas);                
        def loop(ac) = {
            if (hasNext(iter)) {
                let obj1  = next(iter);
                let ac1 = f(ac, obj1) as & Impure;
                loop(ac1)
            } else
                ac
        };
        loop(s)

    /// Unfolds (make specific Java object - client can cast)

    pub def unfoldHashSet(f: s -> Option[(##java.lang.Object, s)] & ef, st: s): ##java.util.HashSet & ef =
        unfoldHashSetHelper(f, st)

    def unfoldHashSetHelper(f: s -> Option[(##java.lang.Object, s)] & ef, st: s): ##java.util.HashSet & ef =
        import new java.util.HashSet(): ##java.util.HashSet & ef as newHashSet;
        import java.util.HashSet.add(##java.lang.Object): Bool & ef as add; 
        def loop(st1, ac) = match f(st1) {
            case None         => ac
            case Some(a, st2) => { let _ = add(ac, a); loop(st2, ac) }
        };
        loop(st, newHashSet())

    pub def unfoldLinkedList(f: s -> Option[(##java.lang.Object, s)] & ef, st: s): ##java.util.LinkedList & ef =
        unfoldLinkedListHelper(f, st)

    def unfoldLinkedListHelper(f: s -> Option[(##java.lang.Object, s)] & ef, st: s): ##java.util.LinkedList & ef =
        import new java.util.LinkedList(): ##java.util.LinkedList & ef as newLinkedList;
        import java.util.LinkedList.add(##java.lang.Object): Bool & ef as add;
        def loop(st1, ac) = match f(st1) {
            case None         => ac
            case Some(a, st2) => { let _ = add(ac, a); loop(st2, ac) }
        };
        loop(st, newLinkedList())

    pub def unfoldArrayList(f: s -> Option[(##java.lang.Object, s)] & ef, st: s): ##java.util.ArrayList & ef =
        unfoldArrayListHelper(f, st)

    def unfoldArrayListHelper(f: s -> Option[(##java.lang.Object, s)] & ef, st: s): ##java.util.ArrayList & ef =
        import new java.util.ArrayList(): ##java.util.ArrayList & ef as newArrayList;
        import java.util.ArrayList.add(##java.lang.Object): Bool & ef as add;
        def loop(st1, ac) = match f(st1) {
            case None         => ac
            case Some(a, st2) => { let _ = add(ac, a); loop(st2, ac) }
        };
        loop(st, newArrayList())

    pub def unfoldHashMap(f: s -> Option[(##java.lang.Object, ##java.lang.Object, s)] & ef, st: s): ##java.util.HashMap & ef =
        unfoldHashMapHelper(f, st)

    def unfoldHashMapHelper(f: s -> Option[(##java.lang.Object, ##java.lang.Object, s)] & ef, st: s): ##java.util.HashMap & ef =
        import new java.util.HashMap(): ##java.util.HashMap & ef as newHashMap;
        import java.util.HashMap.put(##java.lang.Object, ##java.lang.Object): ##java.lang.Object & ef as put; 
        def loop(st1, ac) = match f(st1) {
            case None         => ac
            case Some(k, v, st2) => { let _ = put(ac, k, v); loop(st2, ac) }
        };
        loop(st, newHashMap())

    pub def unfoldTreeMap(f: s -> Option[(##java.lang.Object, ##java.lang.Object, s)] & ef, st: s): ##java.util.TreeMap & ef =
        unfoldTreeMapHelper(f, st)

    def unfoldTreeMapHelper(f: s -> Option[(##java.lang.Object, ##java.lang.Object, s)] & ef, st: s): ##java.util.TreeMap & ef =
        import new java.util.TreeMap(): ##java.util.TreeMap & ef as newTreeMap;
        import java.util.TreeMap.put(##java.lang.Object, ##java.lang.Object): ##java.lang.Object & ef as put; 
        def loop(st1, ac) = match f(st1) {
            case None         => ac
            case Some(k, v, st2) => { let _ = put(ac, k, v); loop(st2, ac) }
        };
        loop(st, newTreeMap())

    /// Special function to avoid Flix marshal bug:
    /// https://github.com/flix/flix/issues/3332

    pub def runVaragsOfString(l: List[String], k: Array[String, r] -> a \ Read(r), _: Region[r]): a & Impure =
        import static flixspt.sandbox.MarshalArrayList.arrayListOfStringToStringArray(##java.util.ArrayList): Array[String, r] \ Read(r);
        let step = iter -> match Iterator.next(iter) {
            case Some(x)    => Some((x as ##java.lang.Object, iter))
            case None       => None
        };
        let arrlist = unfoldArrayList(step, List.iterator(Static, l));
        k(arrayListOfStringToStringArray(arrlist))
        

}
