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

package compiler.structure.parsers.structures

import compiler.structure.blocks.structures.ObjectMethodCallBlock
import compiler.structure.parsers.Parsers
import compiler.tokenizer.Tokenizer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class ObjectMethodCallParserTest() extends FunSuite with BeforeAndAfter {

  val parsers = Parsers.parsers

  val lines = List(
    ".methodCall()"
  )

  test("Block creation test") {
    for (line <- lines) {
      val parseable = parsers.filter(p => p.shouldParse(line))
      assert(!parseable.isEmpty)
      assert(parseable.head.parse(null, new Tokenizer(line)).isInstanceOf[ObjectMethodCallBlock])
    }
  }

}