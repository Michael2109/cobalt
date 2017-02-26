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

package compiler.parser.structures.kinds

import compiler.block.Block
import compiler.block.structures.kinds.ObjectBlockTest
import compiler.parser.ParserTest
import compiler.structure.parameters.ParametersTest
import compiler.tokenizer.TokenizerTest

class ObjectParserTest extends ParserTest[ObjectBlockTest] {

  def shouldParse(line: String): Boolean = {
    line.matches("object[ ]+[a-zA-Z][a-zA-Z0-9]*:[ ]*")
  }

  def parse(superBlock: Block, tokenizer: TokenizerTest): ObjectBlockTest = {
    tokenizer.nextToken
    val objectName: String = tokenizer.nextToken.token

    val parameters = new ParametersTest().getParameters("")

    val parentClass = "java/lang/Object"


    val implementedClasses = ""


    return new ObjectBlockTest(superBlock, objectName, parameters.toArray, parentClass, implementedClasses)
  }
}
