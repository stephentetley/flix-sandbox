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


namespace Text/SimpleFormat {

    ///
    /// Represents a FilePath.
    ///
    pub opaque type Doc = Data.FunStringBuilder


    ///
    /// Return `sb` converted  to a string.
    ///
    pub def toString(d: Doc): String =
        let Doc(sb) = d;
        Data/FunStringBuilder.toString(sb)
    
    pub def empty(): Doc =         
        Doc(Data/FunStringBuilder.new())


    pub def text(s: String): Doc = 
        use Data/FunStringBuilder.appendString;
        let sb = Data/FunStringBuilder.new();
        Doc(appendString(s, sb))

    pub def char(c: Char): Doc = 
        use Data/FunStringBuilder.appendChar;
        let sb = Data/FunStringBuilder.new();
        Doc(appendChar(c, sb))

    ///
    /// Concatenate two documents horizontally (no separating space).
    ///
    pub def beside(d1: Doc, d2: Doc): Doc = 
        use Data/FunStringBuilder.append;
        let Doc(sb1) = d1;
        let Doc(sb2) = d2;
        Doc(append(sb1, sb2))

    pub def line(): Doc = 
        use Data/FunStringBuilder.appendLineSeparator;
        let sb = Data/FunStringBuilder.new();
        Doc(appendLineSeparator(sb))

    ///
    /// Concatenate two documents horizontally (no separating space).
    ///
    pub def <>(x: Doc, y: Doc): Doc = beside(x,y)

    ///
    /// Concatenate two documents horizontally with a separating space.
    ///
    pub def besideSpace(x: Doc, y: Doc): Doc = x <> char(' ') <> y

    ///
    /// Concatenate two documents horizontally with a separating space.
    ///
    pub def <<>>(x: Doc, y:Doc): Doc = besideSpace(x,y)

    ///
    /// Concatenate two documents with a line break.
    ///
    pub def below(x: Doc, y: Doc): Doc = x <> line() <> y

    ///
    /// Concatenate two documents with a line break.
    ///
    pub def <!>(x: Doc, y: Doc): Doc = below(x, y)

    // ************************************************************************
    // List concatenation

    pub def foldDocs(op: (Doc, Doc) -> Doc, docs: List[Doc]): Doc =
        match docs {
            case Nil => empty()
            case x :: rs => foldDocsHelper(op, x, rs, identity)
        }

    def foldDocsHelper(op: (Doc, Doc) -> Doc, acc: Doc, docs:List[Doc], cont: Doc -> Doc): Doc =
        match docs {
            case Nil => cont(acc)
            case x :: rs => foldDocsHelper(op, op(acc,x), rs, cont)
        }

    pub def punctuate(sep: Doc, docs:List[Doc]): List[Doc] = punctuateHelper(sep, docs, xs -> xs)
    

    pub def punctuateHelper(sep: Doc, ds:List[Doc], k: List[Doc] -> List[Doc]): List[Doc] = match ds {
        case Nil => k(Nil)
        case d :: Nil => k(d :: Nil)
        case d :: rs    => punctuateHelper(sep, rs, xs -> k((d <> sep) :: xs))
    }

    ///
    /// Punctuate the list of documents with the separator `sep` and bookend them
    /// with `left` and `right`.
    ///
    pub def encloseSep(left:Doc, right:Doc, sep: Doc, docs: List[Doc]): Doc = match docs {
        case Nil => left <> right
        case d1 :: Nil => left <> d1 <> right
        case d1 :: rs => encloseSepHelper(sep, rs, left <> d1, d -> d <> right)
    }
        
    ///
    /// Helper function for `encloseSep`.
    ///
    def encloseSepHelper(sep: Doc, docs: List[Doc], acc: Doc, cont: Doc -> Doc): Doc = 
        match docs {
            case Nil => cont(acc)
            case d1 :: rs => {
                let acc1 = acc <> sep <> d1;
                encloseSepHelper(sep, rs, acc1, cont)
            }
        }

    ///
    /// Enclose in parens and separate with comma (a,b,c,...)
    ///
    pub def tupled(docs: List[Doc]) : Doc = encloseSep(lparen(), rparen(), comma(), docs)

    ///
    /// Separate documents horizontally with a space.
    ///
    /// Note - seems to be a problem using (<+>)...
    ///
    pub def hsep(docs: List[Doc]): Doc = foldDocs((x,y) -> x <<>> y, docs)

    ///
    /// Returns the docs concatenated together as lines.
    ///
    pub def vcat(docs: List[Doc]): Doc = foldDocs((x,y) -> x <!> y, docs)

    ///
    /// Concatenate documents horizontally (no space).
    ///
    pub def hcat(docs: List[Doc]): Doc= foldDocs((x,y) -> x <> y, docs)

    ///
    /// Print an Int8 literal.
    ///
    pub def int8(i: Int8): Doc = Int8.toString(i) |> text

    ///
    /// Print an Int16 literal.
    ///
    pub def int16(i: Int16): Doc = Int16.toString(i) |> text

    ///
    /// Print an Int32 literal.
    ///
    pub def int32(i: Int32): Doc = Int32.toString(i) |> text

    ///
    /// Print an Int64 literal.
    ///
    pub def int64(i: Int64): Doc = Int64.toString(i) |> text

    ///
    /// Print an float32 literal.
    ///
    pub def float32(d: Float32): Doc = Float32.toString(d) |> text

    ///
    /// Print an float64 literal.
    ///
    pub def float64(d: Float64): Doc = Float64.toString(d) |> text

    ///
    /// Print an BigInt literal.
    ///
    pub def bigInt(i: BigInt): Doc = BigInt.toString(i) |> text

    pub def repeatString(s: String, n: Int32): Doc = 
        String.repeat(n, s) |> text

    pub def repeat(d: Doc, n: Int32): Doc = 
        List.repeat(n, d) |> hcat

    // ************************************************************************
    // Character printers

    ///
    /// Tab.
    ///
    pub def tab() : Doc = char('\t')

    ///
    /// Single left parenthesis: '('.
    ///
    pub def lparen() : Doc = char('(')

    ///
    /// Single right parenthesis: ')'
    ///
    pub def rparen() : Doc = char(')')

    ///
    /// Single left angle: '<'
    ///
    pub def langle() : Doc = char('<')

    ///
    /// Single right angle: '>'
    ///
    pub def rangle() : Doc = char('>')

    ///
    /// Single left brace: '{'
    ///
    pub def lbrace(): Doc = char('{')
    
    ///
    /// Single right brace: '}'
    ///
    pub def rbrace(): Doc= char('}')
    
    ///
    /// Single left square bracket: '['
    ///
    pub def lbracket(): Doc = char('[')
    
    ///
    /// Single right square bracket: ']'
    ///
    pub def rbracket(): Doc = char(']')

    ///
    /// Single quote
    ///
    pub def squote(): Doc= char('\'')

    ///
    /// The document @dquote@ contains a double quote
    ///
    pub def dquote(): Doc = char('"')

    ///
    /// The document @semi@ contains a semi colon
    ///
    pub def semi(): Doc = char(';')

    ///
    /// The document @colon@ contains a colon
    ///
    pub def colon(): Doc = char(':')

    ///
    /// The document @comma@ contains a comma
    ///
    pub def comma() : Doc = char(',')

    ///
    /// The document @space@ contains a single space
    ///
    pub def space() : Doc = char(' ')

    ///
    /// The document @dot@ contains a single dot
    ///
    pub def dot(): Doc = char('.')

    ///
    /// The document @backslash@ contains a back slash
    ///
    pub def backslash(): Doc = char('\\')

    ///
    /// The document @equals@ contains an equal sign
    ///
    pub def equals(): Doc = char('=')

    ///
    /// Generate a document of n spaces
    ///
    pub def spaces(n: Int32): Doc = repeatString(" ", n)

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    ///
    pub def enclose(l: Doc, r: Doc, body: Doc): Doc = l <> body <> r

    ///
    /// Enclose in single quotes '...'
    ///
    pub def squotes(x: Doc): Doc = enclose(squote(), squote(), x)
    
    ///
    /// Enclose in double quotes "..."
    ///
    pub def dquotes(x: Doc): Doc = enclose(dquote(), dquote(), x)
    
    ///
    /// Enclose in angle braces {...}
    ///
    pub def braces(x: Doc): Doc = enclose(lbrace(), rbrace(), x)
    
    ///
    /// Enclose in square brackets (...)
    ///
    pub def parens(x: Doc): Doc = enclose(lparen(), rparen(), x)
    
    ///
    /// Enclose in angle brackets <...>
    ///
    pub def angles(x: Doc): Doc = enclose(langle(), rangle(), x)
    
    ///
    /// Enclose in square brackets [...]
    ///
    pub def brackets (x: Doc): Doc = enclose(lbracket(), rbracket(), x)
    
}
