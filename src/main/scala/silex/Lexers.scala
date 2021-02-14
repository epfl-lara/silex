/* Copyright 2021 EPFL, Lausanne
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

import scala.collection.mutable.ArrayBuffer

import silex.util.{Zippers, BufferedIterator}

/** Contains definitions for lexers.
  *
  * @group lexer
  */
trait Lexers extends RegExps with Zippers {

  /** Type of tokens.
    *
    * @group abstract
    */
  type Token

  /** Type of positions.
    *
    * @group abstract
    */
  type Position

  /** Type of functions that create tokens. */
  type TokenMaker = (Iterable[Character], (Position, Position)) => Token

  //---- Producers ----//

  /** Associates a regular expression with a token generator.
    *
    * @group producer
    */
  case class Producer(regExp: RegExp, makeToken: TokenMaker)

  /** Adds methods to build a `Producer` to a `RegExp`.
    *
    * @group producer
    */
  implicit class ProducerDecorator(regExp: RegExp) {

    /** Creates a `Producer`. */
    def |>(makeToken: (Iterable[Character], (Position, Position)) => Token) =
      Producer(regExp, makeToken)

    /** Creates a `Producer`. */
    def |>(makeToken: Iterable[Character] => Token) =
      Producer(regExp, (cs: Iterable[Character], _: (Position, Position)) => makeToken(cs))

    /** Creates a `Producer`. */
    def |>(token: Token) =
      Producer(regExp, (_: Iterable[Character], _: (Position, Position)) => token)
  }

  //---- Lexers ----//

  /** Tokenizes an input source with respect to a sequence of token producers.
    *
    * @group lexer
    */
  class Lexer private(producers: List[Producer],
                      errorToken: Option[TokenMaker],
                      endToken: Option[Position => Token]) {

    /** Specifies what token to generate in case no regular expressions match. */
    def onError(handler: TokenMaker): Lexer = {
      new Lexer(producers, Some(handler), endToken)
    }

    /** Specifies what token to generate at the end of input. */
    def onEnd(handler: Position => Token): Lexer = {
      new Lexer(producers, errorToken, Some(handler))
    }

    /** Returns an iterator that produces tokens from the `source`.
      *
      * @param source      The input source.
      * @param stopOnError Indicates if the lexer should stop at the first error (`true`),
      *                    or continue producting tokens (`false`). `true` by default.
      */
    def apply(source: Source[Character, Position], stopOnError: Boolean = true): Iterator[Token] =
      new Iterator[Token] {

        // The automaton corresponding to the regular expressions.
        private val state = stateFactory(producers.map(p => (p.regExp, p.makeToken)))

        /** Indicates if the source has ended. */
        private var ended: Boolean = false

        /** Cache for the next. Computed by `hasNext` or `next()`. */
        private var cacheNext: Option[Token] = None

        /** Queries the source for the next token and update the state. */
        private def fetchNext(): Unit = {
          val startPos = source.currentPosition

          if (!source.hasNext) {
            ended = true
            cacheNext = endToken.map(_.apply(startPos))
            return
          }

          cacheNext = tokenizeOne(state, source).orElse {
            source.next() // Skip one char.
            source.mark()
            val (start, content, end) = source.commit()
            if (stopOnError) {
              ended = true
            }
            errorToken.map(_.apply(content, (start, end)))
          }
        }

        override def hasNext: Boolean = cacheNext match {
          case Some(_) => true
          case None => if (ended) false else {
            fetchNext()
            hasNext
          }
        }

        override def next(): Token = cacheNext match {
          case Some(token) => {
            cacheNext = None
            token
          }
          case None if ended => throw new NoSuchElementException("Token iterator ended.")
          case None => {
            fetchNext()
            next()
          }
        }
      }

    /** Spawn a thread that immediately start producing tokens from the `source`.
      *
      * @param source     The input source.
      * @param batchSize  Number of tokens to produce in a batch. By default `50`.
      */
    def spawn(
        source: Source[Character, Position],
        stopOnError: Boolean = true,
        batchSize: Int = 50): Iterator[Token] = {

      // The automaton corresponding to the regular expressions.
      val state = stateFactory(producers.map(p => (p.regExp, p.makeToken)))

      val buffer: ArrayBuffer[Token] = new ArrayBuffer(batchSize)

      val it = new BufferedIterator[Token]

      val thread = new Thread {
        override def run: Unit = {
          while (true) {

            if (!source.hasNext) {
              endToken.foreach { makeToken =>
                buffer += makeToken(source.currentPosition)
              }

              if (buffer.nonEmpty) {
                it.addAll(buffer)
              }

              it.end()

              return
            }

            tokenizeOne(state, source) match {
              case Some(token) => {
                buffer += token

                if (buffer.size >= batchSize) {
                  it.addAll(buffer)
                  buffer.clear()
                }
              }
              case None => {
                source.next()
                source.mark()
                val (start, content, end) = source.commit()

                errorToken.foreach { makeToken =>
                  buffer += makeToken(content, (start, end))
                }

                if (stopOnError) {
                  if (buffer.nonEmpty) {
                    it.addAll(buffer)
                  }

                  it.end()

                  return
                }
                else if (buffer.size >= batchSize) {
                  it.addAll(buffer)
                  buffer.clear()
                }
              }
            }
          }
        }
      }

      thread.start()

      it
    }

    /** Tries to produce a single token from the source. Uses automaton. */
    private def tokenizeOne(state: State[TokenMaker], source: Source[Character, Position]): Option[Token] = {
      source.mark()
      var current: State[TokenMaker] = state
      var lastAccepting: Option[TokenMaker] = None // We don't want to accept empty string.
      while (current.isLive && source.hasNext) {
        current = current.transition(source.next())
        val acceptedValue = current.isAccepting
        if (acceptedValue.nonEmpty) {
          source.mark()
          lastAccepting = acceptedValue
        }
      }
      source.reset()
      lastAccepting.map { (makeToken) =>
        val (start, content, end) = source.commit()
        makeToken(content, (start, end))
      }
    }
  }

  /** Contains utilities to build lexers.
    *
    * @group lexer
    */
  object Lexer {

    /** Creates a lexer for a sequence of producers.
      *
      * @param producers The producers, in decreasing priority.
      */
    def apply(producers: Producer*): Lexer = new Lexer(producers.toList, None, None)
  }

  private[silex] def stateFactory(exprs: List[(RegExp, TokenMaker)]): State[TokenMaker] =
    State(exprs)
}

/** Specialized lexers operating on `Char` characters.
  * Expected to be mixed-in with [[silex.Lexers]].
  *
  * @group lexer
  */
trait CharLexers { self: Lexers =>

  type Character = Char

  /** Single digit between 0 and 9. */
  val digit = elem(_.isDigit)

  /** Single digit between 1 and 9. */
  val nonZero = elem((c: Char) => c >= '1' && c <= '9')

  /** Single digit between 0 and 9 or A and F or a and f. */
  val hex = elem((c: Char) => c.isDigit || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))

  /** Single white space character. */
  val whiteSpace = elem(_.isWhitespace)

  override private[silex] def stateFactory(exprs: List[(RegExp, TokenMaker)]): State[TokenMaker] =
    State.specialized(exprs)
}
