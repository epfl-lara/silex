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

package silex

/** Contains definitions relating to regular expressions.
  *
  * @see See [[silex.CharLexers]] for useful regular expressions on `Char`.
  *
  * @group lexer
  */
trait RegExps {

  /** Type of characters.
    *
    * @group abstract
    */
  type Character

  import RegExp._

  /** Regular expressions over characters.
    *
    * @group regexp
    */
  sealed abstract class RegExp {

    /** Indicates if this regular expression accepts the empty word. */
    val acceptsEmpty: Boolean

    /** Indicates if the regular expression is productive. */
    val isProductive: Boolean

    /** Indicates if the regular expressions accepts at least a non-empty word. */
    val hasNext: Boolean

    /** Union of `this` and `that` regular expression.
      *
      * Performs surface-level simplifications.
      *
      * @return A regular expression that accepts words if either `this` or `that` accepts it.
      *
      * @group combinator
      */
    def |(that: RegExp): RegExp = (this, that) match {
      case (EmptySet, _) => that
      case (_, EmptySet) => this
      case _ => Union(this, that)
    }

    /** Concatenation of `this` and `that` regular expression.
      *
      * Performs surface-level simplifications.
      *
      * @return A regular expression that accepts words
      *         if `this` accepts a prefix and `that` accepts the suffix.
      *
      * @group combinator
      */
    def ~(that: RegExp): RegExp = (this, that) match {
      case (EmptySet, _) => EmptySet
      case (_, EmptySet) => EmptySet
      case (EmptyStr, _) => that
      case (_, EmptyStr) => this
      case _ => Concat(this, that)
    }

    /** Exactly `n` instances of `this` regular expression.
      *
      * @group combinator
      */
    def times(n: Int): RegExp = {
      require(n >= 0)

      if (n == 0) {
        EmptyStr
      }
      else {
        this ~ this.times(n - 1)
      }
    }

    /** Zero or one instances of `this` regular expression.
      *
      * @group combinator
      */
    def opt: RegExp = this | EmptyStr
  }

  /** Contains primitive constructors for regular expressions.
    *
    * @group regexp
    */
  object RegExp {

    /** Accepts only the empty word. */
    case object EmptyStr extends RegExp {
      override val acceptsEmpty: Boolean = true
      override val isProductive: Boolean = true
      override val hasNext: Boolean = false
    }

    /** Never accepts. */
    case object EmptySet extends RegExp {
      override val acceptsEmpty: Boolean = false
      override val isProductive: Boolean = false
      override val hasNext: Boolean = false
    }

    /** Accepts single characters that satisfy a predicate. */
    case class Elem(predicate: Character => Boolean) extends RegExp {
      override val acceptsEmpty: Boolean = false
      override val isProductive: Boolean = true
      override val hasNext: Boolean = true
    }

    /** Union of two regular expressions. */
    case class Union(left: RegExp, right: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = left.acceptsEmpty || right.acceptsEmpty
      override val isProductive: Boolean = left.isProductive || right.isProductive
      override val hasNext: Boolean = left.hasNext || right.hasNext
    }

    /** Concatenation of two regular expressions. */
    case class Concat(first: RegExp, second: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = first.acceptsEmpty && second.acceptsEmpty
      override val isProductive: Boolean = first.isProductive && second.isProductive
      override val hasNext: Boolean =
        (first.hasNext && second.isProductive) || (first.acceptsEmpty && second.hasNext)
    }

    /** Kleene star of a regular expressions. */
    case class Star(regExp: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = true
      override val isProductive: Boolean = true
      override val hasNext: Boolean = regExp.hasNext
    }
  }

  //---- Combinators ----//

  /** Regular expression that accepts any of the characters in `chars`.
    *
    * @group combinator
    */
  def oneOf(chars: Seq[Character]): RegExp = {
    val set = Set(chars*)
    elem((c: Character) => set.contains(c))
  }

  /** Regular expression that accepts single characters based on a `predicate`.
    *
    * @group combinator
    */
  def elem(predicate: Character => Boolean): RegExp = Elem(predicate)

  /** Regular expression that accepts only the single character `char`.
    *
    * @group combinator
    */
  def elem(char: Character): RegExp = Elem(_ == char)

  /** Regular expression that accepts only the sequence of characters `chars`.
    *
    * @group combinator
    */
  def word(chars: Seq[Character]): RegExp = {
    val empty: RegExp = EmptyStr
    chars.foldRight(empty) {
      case (char, rest) => elem(char) ~ rest
    }
  }

  /** Regular expression that accepts zero or more repetitions of `regExp`.
    *
    * @group combinator
    */
  def many(regExp: RegExp): RegExp = regExp match {
    case Star(_) => regExp
    case _ => Star(regExp)
  }

  /** Regular expression that accepts one or more repetitions of `regExp`.
    *
    * @group combinator
    */
  def many1(regExp: RegExp): RegExp = regExp ~ many(regExp)

  /** Regular expression that accepts zero or one instances of `regExp`.
    *
    * @group combinator
    */
  def opt(regExp: RegExp): RegExp = regExp | EmptyStr

  /** Regular expression that accepts any single character.
    *
    * @group combinator
    */
  val any: RegExp = Elem((c: Character) => true)
}
