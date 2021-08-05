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

import java.io.{BufferedReader, InputStreamReader, Reader, FileInputStream}

import scala.collection.mutable.ArrayBuffer

/** Represents iterators with `Position`s and ability to `mark` points and `reset` to them.
  *
  * @group source
  */
trait Source[Character, Position] extends Iterator[Character] {

  /** Returns the marked content since last commit. */
  def commit(): (Position, Iterable[Character], Position)

  /** Marks the current point. */
  def mark(): Unit

  /** Reset to the last marked point. */
  def reset(): Unit

  /** Current position. */
  def currentPosition: Position

  /** Closes the source. */
  def close(): Unit = ()

  override def finalize(): Unit = close()
}

/** Source over an interator. */
class IteratorSource[Character, Position](
  iterator: Iterator[Character],
  positioner: Positioner[Character, Position]) extends Source[Character, Position] {

  private var queue: ArrayBuffer[Character] = new ArrayBuffer()
  private var accepted: ArrayBuffer[Character] = new ArrayBuffer()
  private val current: ArrayBuffer[Character] = new ArrayBuffer()
  private var commitPos: Position = positioner.start
  private var markPos: Position = positioner.start
  private var currentPos: Position = positioner.start

  override def currentPosition: Position = currentPos

  override def next(): Character = {
    val char: Character = if (queue.nonEmpty) {
      val res = queue.last
      queue.dropRightInPlace(1)
      res
    }
    else {
      iterator.next()
    }
    current += char
    currentPos = positioner.increment(currentPos, char)
    char
  }

  override def hasNext: Boolean = {
    queue.nonEmpty || iterator.hasNext
  }

  override def mark(): Unit = {
    accepted ++= current
    current.clear()
    markPos = currentPos
  }

  override def reset(): Unit = {
    queue ++= current.reverseIterator
    current.clear()
    currentPos = markPos
  }

  override def commit(): (Position, Iterable[Character], Position) = {
    val chars = accepted
    val startPos = commitPos
    val endPos = markPos
    accepted = new ArrayBuffer()
    commitPos = markPos
    (startPos, chars, endPos)
  }
}

/** Source over an array of character. */
class ArraySource[Character, Position](
  array: Array[Character],
  positioner: Positioner[Character, Position]) extends Source[Character, Position] {

  private var commitIndex: Int = 0
  private var markIndex: Int = 0
  private var currentIndex: Int = 0
  private var commitPos: Position = positioner.start
  private var markPos: Position = positioner.start
  private var currentPos: Position = positioner.start

  override def currentPosition: Position = currentPos

  override def next(): Character = {
    val char = array(currentIndex)
    currentIndex += 1
    currentPos = positioner.increment(currentPos, char)
    char
  }

  override def hasNext: Boolean = {
    currentIndex < array.size
  }

  override def mark(): Unit = {
    markIndex = currentIndex
    markPos = currentPos
  }

  override def reset(): Unit = {
    currentIndex = markIndex
    currentPos = markPos
  }

  override def commit(): (Position, Iterable[Character], Position) = {
    val chars = array.slice(commitIndex, markIndex)
    val startPos = commitPos
    val endPos = markPos
    commitIndex = markIndex
    commitPos = markPos
    (startPos, chars, endPos)
  }
}

/** Source over an array of character. */
class StringSource[Position](
  string: String,
  positioner: Positioner[Char, Position]) extends Source[Char, Position] {

  private var commitIndex: Int = 0
  private var markIndex: Int = 0
  private var currentIndex: Int = 0
  private var commitPos: Position = positioner.start
  private var markPos: Position = positioner.start
  private var currentPos: Position = positioner.start

  override def currentPosition: Position = currentPos

  override def next(): Char = {
    val char = string.charAt(currentIndex)
    currentIndex += 1
    currentPos = positioner.increment(currentPos, char)
    char
  }

  override def hasNext: Boolean = {
    currentIndex < string.length
  }

  override def mark(): Unit = {
    markIndex = currentIndex
    markPos = currentPos
  }

  override def reset(): Unit = {
    currentIndex = markIndex
    currentPos = markPos
  }

  override def commit(): (Position, Iterable[Char], Position) = {
    val chars = string.substring(commitIndex, markIndex)
    val startPos = commitPos
    val endPos = markPos
    commitIndex = markIndex
    commitPos = markPos
    (startPos, chars, endPos)
  }
}

/** Source over a `Reader`. */
class ReaderSource[Position](
  reader: Reader,
  positioner: Positioner[Char, Position],
  markLimit: Int = 1024) extends Source[Char, Position] {

  require(reader.markSupported)

  private var nextInt: Int = reader.read()
  reader.mark(markLimit)
  private var markedNextInt = nextInt
  private var accepted: ArrayBuffer[Char] = new ArrayBuffer()
  private val current: ArrayBuffer[Char] = new ArrayBuffer()
  private var commitPos: Position = positioner.start
  private var markPos: Position = positioner.start
  private var currentPos: Position = positioner.start

  override def currentPosition: Position = currentPos

  override def next(): Char = {
    val char: Char = nextInt.toChar
    current += char
    currentPos = positioner.increment(currentPos, char)
    nextInt = reader.read()
    char
  }

  override def hasNext: Boolean = {
    nextInt >= 0
  }

  override def mark(): Unit = {
    accepted ++= current
    current.clear()
    reader.mark(markLimit)
    markedNextInt = nextInt
    markPos = currentPos
  }

  override def reset(): Unit = {
    current.clear()
    reader.reset()
    nextInt = markedNextInt
    currentPos = markPos
  }

  override def commit(): (Position, Iterable[Char], Position) = {
    val chars = accepted
    val startPos = commitPos
    val endPos = markPos
    accepted = new ArrayBuffer()
    commitPos = markPos
    (startPos, chars, endPos)
  }

  override def close(): Unit = reader.close()
}

/** Builds sources.
  *
  * @group source
  */
object Source {

  /** Builds a source from a `file`.
    *
    * @param file       The path to the file.
    * @param positioner Generator of positions.
    * @param charset    The file's charset, by default the default charset of the virtual machine.
    * @param inMemory   Whether to load the entire file in memory as a string, by default `true`.
    */
  def fromFile[Position](
      file: String,
      positioner: Positioner[Char, Position],
      charset: java.nio.charset.Charset = java.nio.charset.Charset.defaultCharset(),
      inMemory: Boolean = true): Source[Char, Position] = {

    if (inMemory) {
      val string = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(file)), charset)
      new StringSource[Position](string, positioner)
    }
    else {
      val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), charset))
      new ReaderSource[Position](reader, positioner)
    }
  }

  /** Builds a source from a `string`. */
  def fromString(string: String): Source[Char, StringPosition] = {
    fromString(string, StringPositioner)
  }

  /** Builds a source from a `string` and a `positioner`. */
  def fromString[Position](
      string: String,
      positioner: Positioner[Char, Position]): Source[Char, Position] = {
    new StringSource[Position](string, positioner)
  }

  /** Builds a source from a `string` and a `positioner`. */
  def fromArray[Character, Position](
      array: Array[Character],
      positioner: Positioner[Character, Position]): Source[Character, Position] = {
    new ArraySource[Character, Position](array, positioner)
  }

  /** Builds a source from an `iterator` and a `positioner`. */
  def fromIterator[Character, Position](
      iterator: Iterator[Character],
      positioner: Positioner[Character, Position]): Source[Character, Position] = {

    new IteratorSource[Character, Position](iterator, positioner)
  }
}