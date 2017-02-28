/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Cobalt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package compiler.structure.parsers.primitives

import compiler.structure.blocks.primitives.BooleanBlock
import compiler.tokenizer.Tokenizer
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BooleanParserTest extends FunSuite with BeforeAndAfter {

  val parser = new BooleanParser

  val linesInitTrue = List(
    "val x = true",
    "val x:boolean = true",
    "var x = true",
    "var x:boolean = true"
  )

  val linesInitFalse = List(
    "val x = false",
    "val x:boolean = false",
    "var x = false",
    "var x:boolean = false"
  )

  val lines = List(
    "val x:boolean",
    "var x:boolean"
  )

  test("Should parse init true") {
    for (line <- linesInitTrue) {
      assert(parser.shouldParse(line))
    }
  }

  test("Should parse init false") {
    for (line <- linesInitFalse) {
      assert(parser.shouldParse(line))
    }
  }

  test("Should parse no init") {
    for (line <- lines) {
      assert(parser.shouldParse(line))
    }
  }

  test("Block creation init true") {
    for (line <- linesInitTrue) {
      val block = parser.parse(null, new Tokenizer(line))
      assert(block.getName == "x")
      assert(block.getValue == "true")
      assert(block.isInstanceOf[BooleanBlock])
    }
  }

  test("Block creation init false") {
    for (line <- linesInitFalse) {
      val block = parser.parse(null, new Tokenizer(line))
      assert(block.getName == "x")
      assert(block.getValue == "false")
      assert(block.isInstanceOf[BooleanBlock])
    }
  }
  test("Block creation uninitialized") {
    for (line <- lines) {
      val block = parser.parse(null, new Tokenizer(line))
      assert(block.getName == "x")
      assert(block.getValue == "false")
      assert(block.isInstanceOf[BooleanBlock])
    }
  }
}
