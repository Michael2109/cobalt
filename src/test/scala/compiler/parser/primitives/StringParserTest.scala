/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Michael Haywood
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

package compiler.parser.primitives

import compiler.block.primitives.StringBlock
import compiler.tokenizer.Tokenizer
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringParserTest extends FunSuite with BeforeAndAfter {

  val parser = new StringParser

  val linesInit = List(
    "val x = \"Test\"",
    "val x:String = \"Test\"",
    "var x = \"Test\"",
    "var x:String = \"Test\""
  )

  val lines = List(
    "val x:String",
    "var x:String"
  )

  test("Should parse init 10") {
    for (line <- linesInit) {
      assert(parser.shouldParse(line))
    }
  }

  test("Should parse no init") {
    for (line <- lines) {
      assert(parser.shouldParse(line))
    }
  }

  test("Block creation init 10") {

    for (line <- linesInit) {
      val block = parser.parse(null, new Tokenizer(line))
      assert(block.getName == "x")
      assert(block.getValue == "Test")
      assert(block.isInstanceOf[StringBlock])
    }
  }
  test("Block creation no init") {
    for (line <- lines) {
      val block = parser.parse(null, new Tokenizer(line))
      assert(block.getName == "x")
      assert(block.getValue == "")
      assert(block.isInstanceOf[StringBlock])
    }
  }
}
