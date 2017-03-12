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

package compiler.structure.parsers.constants

import compiler.structure.blocks.Block
import compiler.structure.blocks.constants.FloatConstantBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer
import compiler.utilities.Constants
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class FloatConstantParserTest() extends FunSuite with BeforeAndAfter {

  val parsers: List[Parser[_]] = Constants.parsers

  val lines: List[String] = List(
    "10f",
    "10F",
    "10.f",
    "10.F",
    "10.5f",
    "10.5F"
  )

  test("Block creation test") {
    for (line <- lines) {
      val parseable = parsers.filter(p => p.shouldParse(line))
      assert(!parseable.isEmpty)

      val block: Block = parseable.head.parse(null, new Tokenizer(line))
      assert(block.getValue == line)
      assert(block.isInstanceOf[FloatConstantBlock])
    }
  }

}