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

package flixspt.sandbox;

import java.util.ArrayList;
import java.util.List;

/**
 * Helper for array marshalling bug:
 * 
 * https://github.com/flix/flix/issues/3332
 * 
 */
public class MarshalArrayList {

    // see: https://stackoverflow.com/questions/4042434/converting-arrayliststring-to-string-in-java
    
    public static String[] arrayListOfStringToStringArray(ArrayList<String> arrlist) throws Exception {
        String[] arr = arrlist.toArray(new String[0]);
        return arr;
    }

    public static String[] listOfStringToStringArray(List<String> list) throws Exception {
        String[] arr = list.toArray(new String[0]);
        return arr;
    }
    
}
