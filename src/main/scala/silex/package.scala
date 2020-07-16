/* Copyright 2020 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/** This package is used to write lexers.
  *
  * =Lexer=
  *
  * To define a lexer, mix-in the [[silex.Lexers]] trait.
  *
  * {{{
  * object MyLexer extends Lexers {
  *
  *   // Type of characters consumed.
  *   type Character = Char
  *
  *   // Type of positions.
  *   type Position = MyPosition
  *
  *   // Type of tokens produced.
  *   type Token = MyToken
  *
  *   // Then define your lexing rules using regular expressions.
  *   val lex = Lexer(...)
  *
  *   // Then, you can query the lexer.
  *   def apply(source: Source[Character, Position]): Iterator[Token] =
  *     lex(source)
  * }
  * }}}
  *
  * Additional traits can be mixed-in.
  * See for instance [[silex.CharRegExps]]
  * for regular expressions on `Char`.
  *
  * =Data sources=
  *
  * The input of lexers is in a form of a data source.
  *
  * {{{
  * // Building a source from a string:
  * val stringSource = Source.fromString("[1, 2, 3]")
  *
  * // Or from a file:
  * val fileSource = Source.fromFile("data/clients.json")
  *
  * // With a custom positioner:
  * val customSource = Source.fromFile("data/clients.json", IndexPositioner)
  * }}}
  *
  *
  * @groupprio abstract 0
  * @groupname abstract Abstract
  *
  * @groupprio lexer 1
  * @groupname lexer Lexers
  *
  * @groupprio producer 2
  * @groupname producer Producers
  *
  * @groupname regexp Regular Expressions
  * @groupprio regexp 11
  *
  * @groupname combinator Combinators
  * @groupprio combinator 12
  *
  * @groupprio source 21
  * @groupname source Sources
  *
  * @groupprio position 22
  * @groupname position Positions
  *
  * @groupprio positioner 23
  * @groupname positioner Positioners
  */
package object silex {

}