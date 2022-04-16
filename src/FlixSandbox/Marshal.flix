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

// NOTE - keep reconciled with the version in `charset-locale`

// BIG TODO - give proper thought to Pure/Impure...

namespace FlixSandbox/Marshal {

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


    /// NOTE caller decides whether to wrap with Err...
    
    pub def unmarshalExceptionMessage(ex: ##java.lang.Exception): String & Impure = 
        import java.lang.Throwable.getMessage(): String & Impure;
        getMessage(ex as ##java.lang.Throwable)

    pub def unmarshalIOExceptionMessage(ex: ##java.io.IOException): String & Impure = 
        import java.lang.Throwable.getMessage(): String & Impure;
        getMessage(ex as ##java.lang.Throwable)


    pub def marshalArray(upcast: a -> ##java.lang.Object & ef, arr: Array[a]): Array[##java.lang.Object] & Impure =
        import new java.util.ArrayList(): ##java.util.ArrayList & Impure as new1;
        import java.util.ArrayList.add(##java.lang.Object): Bool & Impure; 
        import java.util.ArrayList.toArray(): Array[##java.lang.Object] & Impure; 
        let javaarr = new1();
        let addElem = x -> {
            let x1 = upcast(x) as & Impure;
            let _ = add(javaarr, x1);
            ()
        };
        Array.foreach(addElem, arr);
        toArray(javaarr)      

    pub def marshalSet(upcast: a -> ##java.lang.Object & ef, s: Set[a]): ##java.util.Set & Impure =
        import new java.util.HashSet(): ##java.util.HashSet & Impure as new1;
        import java.util.HashSet.add(##java.lang.Object): Bool & Impure; 
        let javas = new1();
        let addElem = x -> {
            let x1 = upcast(x) as & Impure;
            let _ = add(javas, x1);
            ()
        };
        Set.foreach(addElem, s);
        javas as ##java.util.Set

    pub def unmarshalSet(downcast: ##java.lang.Object -> a & ef, javas: ##java.util.Set): Set[a] & Impure with Order[a] =
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let iter = iterator(javas);                
        def loop(acc: Set[a]) = {
            if (hasNext(iter)) {
                let jObj  = next(iter);
                let x1 = downcast(jObj) as & Impure;
                loop(Set.insert(x1, acc))
            } else
                acc
        };
        loop(Set.empty())

    pub def marshalList(upcast: a -> ##java.lang.Object & ef, l: List[a]): ##java.util.List & Impure =
        import new java.util.LinkedList(): ##java.util.LinkedList & Impure as new1;
        import java.util.LinkedList.add(##java.lang.Object): Bool & Impure; 
        let javal = new1();
        let addElem = x -> {
            let x1 = upcast(x) as & Impure;
            let _ = add(javal, x1);
            ()
        };
        List.foreach(addElem, l);
        javal as ##java.util.List

    pub def unmarshalList(downcast: ##java.lang.Object -> a & ef, javal: ##java.util.List): List[a] & Impure =
        import java.util.List.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let iter = iterator(javal);                
        def loop(k: List[a] -> List[a]) = {
            if (hasNext(iter)) {
                let jObj  = next(iter);
                let x1 = downcast(jObj) as & Impure;
                loop(ks -> k(x1 :: ks))
            } else
                k(Nil)
        };
        loop(ks -> ks)

    pub def marshalMap(upcastKey: k -> ##java.lang.Object & ef, upcastValue: v -> ##java.lang.Object & ef,  m: Map[k, v]): ##java.util.Map & Impure =
        import new java.util.HashMap(): ##java.util.HashMap & Impure as new1;
        import java.util.HashMap.put(##java.lang.Object, ##java.lang.Object): ##java.lang.Object & Impure; 
        let javam = new1();
        let addElem = (k, v) -> {
            let k1 = upcastKey(k) as & Impure;
            let v1 = upcastValue(v) as & Impure;
            let _ = put(javam, k1, v1);
            ()
        };
        Map.foreach(addElem, m);
        javam as ##java.util.Map

    pub def unmarshalMapEntry(downcastKey: ##java.lang.Object -> k & ef, downcastValue: ##java.lang.Object -> v & ef, entry: ##java.util.Map$Entry): (k, v) & Impure =
        import java.util.Map$Entry.getKey(): ##java.lang.Object & Impure;
        import java.util.Map$Entry.getValue(): ##java.lang.Object & Impure;        
        let k = getKey(entry);
        let v = getValue(entry);                
        let k1 = downcastKey(k) as & Impure;
        let v1 = downcastValue(v) as & Impure;
        (k1, v1)

    pub def unmarshalMap(downcastEntry: ##java.util.Map$Entry -> (k, v) & ef, javam: ##java.util.Map): Map[k, v] & Impure with Order[k] =
        import java.util.Map.entrySet(): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let set1 = entrySet(javam);
        let iter = iterator(set1);
        def loop(acc: Map[k, v]) = {
            if (hasNext(iter)) {
                let entry = next(iter) as ##java.util.Map$Entry;
                let (k, v) = downcastEntry(entry) as & Impure;
                loop(Map.insert(k, v, acc))
            } else
                acc
        };
        loop(Map.empty())


    pub def marshalSortedMap(upcastKey: k -> ##java.lang.Object & ef, upcastValue: v -> ##java.lang.Object & ef,  m: Map[k, v]): ##java.util.SortedMap & Impure =
        import new java.util.TreeMap(): ##java.util.TreeMap & Impure as new1;
        import java.util.TreeMap.put(##java.lang.Object, ##java.lang.Object): ##java.lang.Object & Impure; 
        let javam = new1();
        let addElem = (k, v) -> {
            let k1 = upcastKey(k) as & Impure;
            let v1 = upcastValue(v) as & Impure;
            let _ = put(javam, k1, v1);
            ()
        };
        Map.foreach(addElem, m);
        javam as ##java.util.SortedMap


    pub def unmarshalSortedMap(downcastKey: ##java.lang.Object -> k & ef, downcastValue: ##java.lang.Object -> v & ef, javam: ##java.util.SortedMap): Map[k, v] & Impure with Order[k] =
        import java.util.SortedMap.entrySet(): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        import java.util.Map$Entry.getKey(): ##java.lang.Object & Impure;
        import java.util.Map$Entry.getValue(): ##java.lang.Object & Impure;        
        let set1 = entrySet(javam);
        let iter = iterator(set1);
        def loop(acc: Map[k, v]) = {
            if (hasNext(iter)) {
                let entry = next(iter) as ##java.util.Map$Entry;
                let k = getKey(entry);
                let v = getValue(entry);                
                let k1 = downcastKey(k) as & Impure;
                let v1 = downcastValue(v) as & Impure;
                loop(Map.insert(k1, v1, acc))
            } else
                acc
        };
        loop(Map.empty())


    pub def unmarshalIterator(downcast: ##java.lang.Object -> a & ef, javaIter: ##java.util.Iterator): Iterator[a] =
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let isDone = () -> { 
            let ans = hasNext(javaIter);
            not ans
        };
        let getNext = () -> {
            let a1 = next(javaIter);
            downcast(a1) as & Impure
        };
        Iterator(isDone, getNext)



    pub def unmarshalStream(downcast: ##java.lang.Object -> a & ef, strm: ##java.util.stream.Stream): Iterator[a] =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & Pure;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        unmarshalIterator(downcast, javaIter)

    pub def unmarshalIntStream(strm: ##java.util.stream.IntStream): Iterator[Int32] =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & Pure;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        let downcast = obj -> unmarshalInt32(obj as ##java.lang.Integer);
        unmarshalIterator(downcast, javaIter)

    pub def unmarshalLongStream(strm: ##java.util.stream.LongStream): Iterator[Int64] =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & Pure;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        let downcast = obj -> unmarshalInt64(obj as ##java.lang.Long);
        unmarshalIterator(downcast, javaIter)

    pub def unmarshalDoubleStream(strm: ##java.util.stream.DoubleStream): Iterator[Float64] =
        import java.util.stream.BaseStream.iterator(): ##java.util.Iterator & Pure;
        let bs = strm as ##java.util.stream.BaseStream;
        let javaIter = iterator(bs);
        let downcast = obj -> unmarshalFloat64(obj as ##java.lang.Double);
        unmarshalIterator(downcast, javaIter)


    pub def marshalListToSet(upcast: a -> ##java.lang.Object & ef, l: List[a]): ##java.util.Set & Impure =
        import new java.util.HashSet(): ##java.util.HashSet & Impure as new1;
        import java.util.HashSet.add(##java.lang.Object): Bool & Impure; 
        let javas = new1();
        let addElem = x -> {
            let x1 = upcast(x) as & Impure;
            let _ = add(javas, x1);
            ()
        };
        List.foreach(addElem, l);
        javas as ##java.util.Set


    /// Unmarshal a Java set to a Flix list - elements in the list will be unique (order is arbitrary).
    /// Unlike Flix Sets, Java Sets are not limited to ordered types, for unordered types we cannot unmarshal 
    /// Java.Set to Flix Set instead we unmarshal to a List.
    pub def unmarshalSetToList(downcast: ##java.lang.Object -> a & ef, javas: ##java.util.Set): List[a] & Impure =
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        let iter = iterator(javas);                
        def loop(k: List[a] -> List[a]) = {
            if (hasNext(iter)) {
                let jObj  = next(iter);
                let x1 = downcast(jObj) as & Impure;
                loop(ks -> k(x1 :: ks))
            } else
                k(Nil)
        };
        loop(ks -> ks)

}
