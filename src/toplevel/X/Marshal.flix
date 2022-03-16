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


namespace X/Marshal {


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

    pub def unmarshalMap(downcastKey: ##java.lang.Object -> k & ef, downcastValue: ##java.lang.Object -> v & ef, javam: ##java.util.Map): Map[k, v] & Impure with Order[k] =
        import java.util.Map.entrySet(): ##java.util.Set & Impure;
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




}
