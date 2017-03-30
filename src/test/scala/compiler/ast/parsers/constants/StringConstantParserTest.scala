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

package compiler.ast.parsers.constants

import compiler.ast.blocks.Block
import compiler.ast.blocks.constants.StringConstantBlock
import compiler.ast.parsers.{Parser, Parsers}
import compiler.tokenizer.Tokenizer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class StringConstantParserTest() extends FunSuite with BeforeAndAfter {

  val parsers: List[Parser[_]] = Parsers.parsers

  val lines: List[String] = List(
    "\"Test1\""
  )

  test("Block creation test") {
    for (line <- lines) {
      val parseable = parsers.filter(p => p.shouldParse(line))
      assert(!parseable.isEmpty)

      val block: Block = parseable.head.parse(null, new Tokenizer(line))
      assert(block.getValue == line)
      assert(block.isInstanceOf[StringConstantBlock])
    }
  }

}