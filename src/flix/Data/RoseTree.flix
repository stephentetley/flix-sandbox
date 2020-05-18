/*
 * Copyright 2020 Stephen Tetley
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

// Note - not in the Data namespace.

pub enum RoseTree[a] {
    case RoseTree(a, List[RoseTree[a]])
}

type alias Forest[a] = List[RoseTree[a]]

namespace RoseTree {

    def shift1(first: String, other: String, xs: List[String], k: List[String] -> List[String]): List[String] = 
        match xs { 
            case Nil => k(Nil)
            case x :: rs => { 
                let s1 = first + x;
                shift2(other, rs, ss -> k(s1 :: ss))
            }
        }
    
    def shift2(other: String, xs: List[String], k: List[String] -> List[String]): List[String] = 
        match xs {
            case Nil => k(Nil)
            case x :: rs => {
                let s1 = other + x;
                shift2(other, rs, ss -> k(s1 :: ss))
            }
        }            


    def drawTree1(x: RoseTree[String], k: List[String] -> List[String]): List[String] = 
        match x {
            case RoseTree(label, kids) => {
                let xs = String.lines(label);
                drawKids(kids, ys -> k(xs ::: ys))
            }
        }

    def drawKids(xs: List[RoseTree[String]], k: List[String] -> List[String]): List[String] = 
        match xs {
            case Nil => k(Nil)
            case t1 :: Nil => drawTree1(t1, ac1 -> shift1("`- ", "   ", ac1, ac2 -> k("|" :: ac2)))
            case t1 :: rs =>  drawTree1(t1, ac1 -> shift1("+- ", "|  ", ac1, ac2 -> drawKids(rs, ac3 -> k("|" :: (ac2 ::: ac3)))))
        }


    pub def drawTree (x: RoseTree[String]): String = drawTree1(x, xs -> xs) |> String.unlines

    pub def drawForest (x: Forest[String]): String = String.unlines(List.map(drawTree, x))
    
}