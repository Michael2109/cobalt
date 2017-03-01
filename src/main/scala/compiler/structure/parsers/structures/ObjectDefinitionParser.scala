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


import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block
import compiler.structure.blocks.structures.ObjectDefinitionBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

import scala.collection.mutable.ListBuffer

/**
  * Creation of a new instance of a class
  */
class ObjectDefinitionParser extends Parser[ObjectDefinitionBlock] {
  /**
    * A list of all regular expressions
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    "(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*[=][ ]*new[ ]*[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)[ ]*"
  )


  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectDefinitionBlock = {
    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val variableName: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    val className: String = tokenizer.nextToken.token
    val operator: String = tokenizer.nextToken.token
    val newKeyword: String = tokenizer.nextToken.token
    val initClassName: String = tokenizer.nextToken.token

    tokenizer.nextToken // skip "("

    var nextToken: String = tokenizer.nextToken.token
    val paramType: String = ""
    var paramName: String = ""
    val parameters: ListBuffer[Parameter] = new ListBuffer[Parameter]
    while (nextToken != ")") {
      {

        if (nextToken == ",") {
          nextToken = tokenizer.nextToken.token
        } else {
          paramName = nextToken.trim
          parameters.append(new Parameter(paramType, paramName))
          nextToken = tokenizer.nextToken.token
        }
      }
    }
    return new ObjectDefinitionBlock(superBlock, declaration, className, variableName, operator, newKeyword, initClassName, parameters)
  }
}
