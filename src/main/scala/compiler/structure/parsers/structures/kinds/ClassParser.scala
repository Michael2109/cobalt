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

package compiler.structure.parsers.structures.kinds

import compiler.data.parameters.Parameters
import compiler.structure.blocks.Block
import compiler.structure.blocks.structures.kinds.ClassBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

class ClassParser extends Parser[ClassBlock] {
  /**
    * A list of all regular expressions
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    "(open[ ]+)?class[ ]+[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)([ ]+extends[ ]+[a-zA-Z][a-zA-Z0-9]*)?:"
  )

  /**
    * Takes a line and checks to see ifs it is for this parsers by using regex.
    */
  override def shouldParse(line: String): Boolean = (getRegexs.filter(line.matches(_)).size > 0)

  def parse(superBlock: Block, tokenizer: Tokenizer): ClassBlock = {

    val isSealed: Boolean = tokenizer.nextToken.token != "open" // check open

    if (!isSealed)
      tokenizer.nextToken // skip class

    val className: String = tokenizer.nextToken.token
    tokenizer.nextToken // (
    var nextToken: String = tokenizer.nextToken.token

    var paramString = ""
    while (nextToken != ")") {
      paramString += nextToken
      nextToken = tokenizer.nextToken.token
    }

    val parameters = new Parameters().getParameters(paramString)

    var parentClass = ""

    if (tokenizer.nextToken.token == "extends") {
      parentClass = tokenizer.nextToken.token
    } else {
      parentClass = "java/lang/Object"
    }

    var implementedClasses = ""
    if (tokenizer.nextToken.token == "implements") {
      implementedClasses = tokenizer.nextToken.token
    }


    return new ClassBlock(superBlock, isSealed, className, parameters.toArray, parentClass, implementedClasses)
  }
}
