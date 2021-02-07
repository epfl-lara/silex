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

import scala.collection.mutable.{ArrayBuffer, Buffer, HashSet, HashMap}

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
    case class FollowBy(right: RegExp, parents: Set[Context]) extends Context
  }
  import Context._

  /** Add a focus to the left of the expression. */
  private def focus(expr: RegExp): Zipper = {
    Zipper(if (expr.isProductive) {
      Set(FollowBy(expr, Set(Empty)))
    }
    else {
      Set()
    })
  }

  /** Zipper-based representation of a regular expression. */
  private[silex] case class Zipper(contexts: Set[Context]) {

    /** Array of predicates appearing in front position. */
    def signature: Array[Character => Boolean] = {
      val downPoints: Buffer[RegExp] =
        new ArrayBuffer()

      val seenUp: HashSet[Context] =
        new HashSet()

      def up(context: Context): Unit = context match {
        case Empty => ()
        case FollowBy(right, parents) => {
          if (!seenUp.contains(context)) {
            seenUp += context
            downPoints += right
            if (right.acceptsEmpty) {
              parents.foreach(up)
            }
          }
        }
      }

      contexts.foreach(up)

      val resultsBuilder: HashSet[Elem] =
        new HashSet()

      val seenDown: HashSet[RegExp] =
        new HashSet()

      def down(expr: RegExp): Unit =
        if (!seenDown.contains(expr)) {
          expr match {
            case elem@Elem(_) =>
              resultsBuilder += elem
            case Union(left, right) =>
              down(left)
              down(right)
            case Concat(left, right) =>
              down(left)
              if (left.acceptsEmpty) {
                down(right)
              }
            case Star(inner) =>
              down(inner)
            case _ => ()
          }
        }

      downPoints.foreach(down)

      resultsBuilder.toSeq.map(_.predicate).toArray
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
      val downPoints: Buffer[(RegExp, Set[Context])] =
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

    protected val parts: Buffer[ContextSetBuilder] =
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
      Set(FollowBy(right, builder.build))
    }
  }

  /** State of the NFA.
    *
    * @param zipper The zipper corresponding to the state.
    */
  private[silex] class State(val zipper: Zipper) {

    /** Next state, based on next character. */
    val transitions: HashMap[Character, State] =
      new HashMap()

    /** Next state, based on fingerprint of next character. */
    val derivatives: HashMap[BigInt, State] =
      new HashMap()

    /** Signature of the zipper. */
    val signature: Array[Character => Boolean] =
      zipper.signature

    /** Checks if the state is accepting. */
    val isAccepting: Boolean =
      zipper.acceptsEmpty

    /** Checks if the state has outgoing transitions. */
    val isCompleted: Boolean =
      signature.isEmpty
  }

  /** Deterministic Finite Automaton. */
  private[silex] class DFA(regexp: RegExp) {
    private val states: HashMap[Zipper, State] =
      new HashMap()

    private def getState(zipper: Zipper): State =
      states.get(zipper).getOrElse {
        val res = new State(zipper)
        states += zipper -> res
        res
      }

    /** Initial state of the machine. */
    val initial = getState(focus(regexp))

    /** Transition function. */
    def apply(from: State, char: Character): State = {
      val transitions = from.transitions
      transitions.get(char).getOrElse {
        val signature = from.signature
        var i = 0
        val size = signature.length
        var fingerprint = BigInt(0)
        while(i < size) {
          if (signature(i)(char)) {
            fingerprint = fingerprint.setBit(i)
          }
          i += 1
        }
        val derivatives = from.derivatives
        val to = derivatives.get(fingerprint).getOrElse {
          val to = getState(from.zipper.derive(char))
          derivatives += fingerprint -> to
          to
        }
        transitions += char -> to
        to
      }
    }
  }
}