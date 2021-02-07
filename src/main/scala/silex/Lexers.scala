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

  //---- Producers ----//

  /** Associates a regular expression with a token generator.
    *
    * @group producer
    */
  case class Producer(regExp: RegExp, makeToken: (Seq[Character], (Position, Position)) => Token)

  /** Adds methods to build a `Producer` to a `RegExp`.
    *
    * @group producer
    */
  implicit class ProducerDecorator(regExp: RegExp) {

    /** Creates a `Producer`. */
    def |>(makeToken: (Seq[Character], (Position, Position)) => Token) =
      Producer(regExp, makeToken)

    /** Creates a `Producer`. */
    def |>(makeToken: Seq[Character] => Token) =
      Producer(regExp, (cs: Seq[Character], _: (Position, Position)) => makeToken(cs))

    /** Creates a `Producer`. */
    def |>(token: Token) =
      Producer(regExp, (_: Seq[Character], _: (Position, Position)) => token)
  }

  //---- Lexers ----//

  /** Tokenizes an input source with respect to a sequence of token producers.
    *
    * @group lexer
    */
  class Lexer private(producers: List[Producer],
                      errorToken: Option[(Seq[Character], (Position, Position)) => Token],
                      endToken: Option[Position => Token]) {

    /** Specifies what token to generate in case no regular expressions match. */
    def onError(handler: (Seq[Character], (Position, Position)) => Token): Lexer = {
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

        /** Indicates if the source has ended. */
        private var ended: Boolean = false

        /** Cache for the next. Computed by `hasNext` or `next()`. */
        private var cacheNext: Option[Token] = None

        /** Queries the source for the next token and update the state. */
        private def fetchNext(): Unit = {
          val startPos = source.currentPosition

          if (source.atEnd) {
            ended = true
            cacheNext = endToken.map(_.apply(startPos))
            return
          }

          tokenizeOneZipper(source) match {
            case Some(token) => cacheNext = Some(token)
            case None => {
              val endPos = source.currentPosition
              val content = if (stopOnError) {
                ended = true
                source.backContent()
              }
              else {
                source.consume()
              }

              cacheNext = errorToken.map(_.apply(content, (startPos, endPos)))
            }
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

      var buffer: Vector[Token] = Vector()

      val it = new BufferedIterator[Token]

      val thread = new Thread {
        override def run: Unit = {
          while (true) {
            val startPos = source.currentPosition

            if (source.atEnd) {
              endToken.foreach { f =>
                buffer = buffer :+ f(startPos)
              }

              if (buffer.nonEmpty) {
                it.addAll(buffer)
              }

              it.end()

              return
            }

            tokenizeOneZipper(source) match {
              case Some(token) => {
                buffer = buffer :+ token

                if (buffer.size >= batchSize) {
                  it.addAll(buffer)
                  buffer = Vector()
                }
              }
              case None => {
                val endPos = source.currentPosition
                val content = if (stopOnError) {
                  source.backContent()
                }
                else {
                  source.consume()
                }

                errorToken.foreach { f =>
                  buffer = buffer :+ f(content, (startPos, endPos))
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
                  buffer = Vector()
                }
              }
            }
          }
        }
      }

      thread.start()

      it
    }

    // The makeToken functions, as an array for fast access.
    private val makeTokens = producers.map(_.makeToken).toArray

    // The automata corresponding to the regular expressions, as an array for fast access.
    private val machines = producers.map(p => new DFA(p.regExp)).toArray

    // The lists of start positions and indices. A list for fast flatMap.
    private val starts = machines.toList.zipWithIndex.map {
      case (m, n) => (m.initial, n)
    }

    /** Tries to produce a single token from the source. Uses automata. */
    private def tokenizeOneZipper(source: Source[Character, Position]): Option[Token] = {

      // The state and index of active automata.
      var states = starts

      // The buffer containing successfully consumed input.
      val buffer: ArrayBuffer[Character] = new ArrayBuffer()

      // Start position of the consumed input.
      val startPos = source.currentPosition

      // End position of the consumed input.
      var endPos = startPos

      // Index of the last successful state machine.
      var successful: Option[Int] = None

      while (states.nonEmpty && !source.atEnd) {
        val char = source.ahead()

        // Indicates if a machine was accepting
        // for this character already.
        var accepted = false

        // Updates the states of all machines and
        // filter out the ones which can no longer
        // reach an accepting state.
        states = states.flatMap {
          case (current, index) => {
            val currentMachine = machines(index)
            val next = currentMachine(current, char)

            // Also records the first accepting automaton.
            if (!accepted && next.isAccepting) {
              endPos = source.currentPosition
              buffer ++= source.consume()
              successful = Some(index)
              accepted = true
            }

            if (!next.isCompleted) {
              (next, index) :: Nil
            }
            else {
              Nil
            }
          }
        }
      }

      // Creates the token, if any.
      successful.map { (index: Int) =>
        // Resets the looked-ahead pointer in the source.
        // Only done in case of a successful tokenization.
        source.back()

        val range = (startPos, endPos)
        val token = makeTokens(index)(buffer.toSeq, range)

        token
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
}
