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
package util

import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, LongMap}

/** Implements conversion to DFA through Brzozowski derivation and zippers. */
trait Zippers { self: RegExps =>

  import RegExp._

  /** Contexts indicate paths from a subexpression to the expression's root
    * in a regular expression derivative.
    */
  private[silex] sealed trait Context
  private object Context {
    /** Empty context. */
    case object Empty extends Context

    /** Additional of layer of context. */
    case class FollowBy(right: RegExp, parents: Set[Context]) extends Context {
      require(right.hasNext)
    }
  }
  import Context._

  /** Add a focus to the left of the expression. */
  private def focus(expr: RegExp): Zipper = {
    Zipper(if (expr.hasNext) {
      Set(FollowBy(expr, Set(Empty)))
    }
    else if (expr.acceptsEmpty) {
      Set(Empty)
    }
    else {
      Set()
    })
  }

  /** Zipper-based representation of a regular expression. */
  private[silex] case class Zipper(contexts: Set[Context]) {

    /** Checks if a non-empty sting is accepted. */
    def hasNext: Boolean = {
      def up(context: Context): Boolean = context match {
        case Empty => false
        case FollowBy(_, _) => true
      }

      contexts.exists(up)
    }

    /** Checks if the empty string is accepted. */
    def acceptsEmpty: Boolean = {
      val seenUp: HashSet[Context] = new HashSet()

      def up(context: Context): Boolean = context match {
        case Empty => true
        case FollowBy(right, parents) => {
          if (seenUp.contains(context)) {
            false
          }
          else {
            seenUp += context

            if (right.acceptsEmpty) {
              parents.exists(up)
            }
            else {
              false
            }
          }
        }
      }

      contexts.exists(up)
    }

    /** Brzozowski's derivation. */
    def derive(char: Character): Zipper = {
      val downPoints: ArrayBuffer[(RegExp, Set[Context])] =
        new ArrayBuffer()

      val seenUp: HashSet[Context] =
        new HashSet()

      def up(context: Context): Unit = context match {
        case Empty => ()
        case FollowBy(right, parents) => {
          if (!seenUp.contains(context)) {
            seenUp += context
            downPoints += ((right, parents))
            if (right.acceptsEmpty) {
              parents.foreach(up(_))
            }
          }
        }
      }

      contexts.foreach(up)

      val resultsBuilder: ExtensibleSetBuilder =
        new ExtensibleSetBuilder()

      val seenDown: HashMap[RegExp, ExtensibleSetBuilder] =
        new HashMap()

      def down(expr: RegExp, builder: ContextSetBuilder): Unit =
        seenDown.get(expr) match {
          case Some(extensible) => extensible += builder
          case None => expr match {
            case Elem(predicate) if predicate(char) =>
              resultsBuilder += builder
            case Union(left, right) =>
              val childBuilder = new ExtensibleSetBuilder()
              childBuilder += builder
              seenDown += expr -> childBuilder
              down(left, childBuilder)
              down(right, childBuilder)
            case Concat(left, right) =>
              val childBuilder = new ExtensibleSetBuilder()
              childBuilder += builder
              seenDown += expr -> childBuilder
              if (right.isProductive) {
                val followBy = new FollowByBuilder(right, childBuilder)
                down(left, followBy)
              }
              if (left.acceptsEmpty) {
                down(right, childBuilder)
              }
            case Star(inner) =>
              val childBuilder = new ExtensibleSetBuilder()
              childBuilder += builder
              seenDown += expr -> childBuilder
              val followBy = new FollowByBuilder(expr, childBuilder)
              down(inner, followBy)
            case _ => ()
          }
        }

      downPoints.foreach {
        case (focus, contexts) => down(focus, ConstantSetBuilder(contexts))
      }

      Zipper(resultsBuilder.build)
    }
  }

  /** Mutable builder of contexts. */
  private sealed trait ContextSetBuilder {
    def build: Set[Context]
  }

  /** Constant builder. */
  private case class ConstantSetBuilder(val build: Set[Context])
      extends ContextSetBuilder

  /** Extensible builder. */
  private class ExtensibleSetBuilder extends ContextSetBuilder {

    protected val parts: ArrayBuffer[ContextSetBuilder] =
      new ArrayBuffer

    def +=(builder: ContextSetBuilder): Unit = {
      parts += builder
    }

    lazy val build: Set[Context] = {
      var result: Set[Context] = Set()
      for (part <- parts) {
        result ++= part.build
      }
      result
    }
  }

  /** Builder of FollowBy contexts. */
  private class FollowByBuilder(right: RegExp, builder: ContextSetBuilder)
      extends ContextSetBuilder {

    lazy val build: Set[Context] = {
      if (right.hasNext) {
        Set(FollowBy(right, builder.build))
      }
      else {
        builder.build
      }
    }
  }

  /** State of an automaton.
    *
    * @tparam S The type of values tagged to the accepting state.
    */
  private[silex] trait State[+S] {
    /** Next state, depending on next character. */
    def transition(char: Character): State[S]

    /** Indicates whether accepting states can be reached from this state,
      * taking at least one transition.
      */
    val isLive: Boolean

    /** Indicates whether the state is accepting, and in that case what
      * value is tagged to the state.
      */
    val isAccepting: Option[S]
  }

  /** Factory of state automatons. */
  private[silex] object State {

    /** Builds a `State` automaton from a list regular expressions, each tagged with a value. */
    def apply[A](exprs: List[(RegExp, A)]): State[A] = {
      val states: HashMap[List[Zipper], State[A]] = new HashMap()
      val values: List[A] = exprs.map(_._2)
      def newState(zippers: List[Zipper]): State[A] = new State[A] {
        private val transitions: HashMap[Character, State[A]] = new HashMap()
        override def transition(char: Character): State[A] =
          transitions.getOrElseUpdate(char, getState(zippers.map(_.derive(char))))
        override val isLive = zippers.exists(_.hasNext)
        override val isAccepting: Option[A] =
          zippers.zip(values).collectFirst {
            case (zipper, value) if zipper.acceptsEmpty => value
          }
      }
      def getState(zippers: List[Zipper]): State[A] =
        states.getOrElseUpdate(zippers, newState(zippers))
      getState(exprs.map(expr => focus(expr._1)))
    }

    /** Builds a `State` automaton from a list regular expressions, each tagged with a value.
      *
      * Specialized to `Char` characters.
      */
    def specialized[A](exprs: List[(RegExp, A)])(implicit ev: Character =:= Char): State[A] = {
      val states: HashMap[List[Zipper], State[A]] = new HashMap()
      val values: List[A] = exprs.map(_._2)
      def newState(zippers: List[Zipper]): State[A] = new State[A] {
        private val transitions: LongMap[State[A]] = new LongMap()
        override def transition(char: Character): State[A] =
          transitions.getOrElseUpdate(ev(char).toLong, getState(zippers.map(_.derive(char))))
        override val isLive = zippers.exists(_.hasNext)
        override val isAccepting: Option[A] =
          zippers.zip(values).collectFirst {
            case (zipper, value) if zipper.acceptsEmpty => value
          }
      }
      def getState(zippers: List[Zipper]): State[A] =
        states.getOrElseUpdate(zippers, newState(zippers))
      getState(exprs.map(expr => focus(expr._1)))
    }
  }
}