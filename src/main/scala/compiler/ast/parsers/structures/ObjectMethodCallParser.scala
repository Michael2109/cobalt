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

package compiler.ast.parsers.structures


import compiler.ast.blocks.Block
import compiler.ast.blocks.structures.ObjectMethodCallBlock
import compiler.ast.parsers.Parser
import compiler.data.parameters.Parameter
import compiler.tokenizer.Tokenizer

import scala.collection.mutable.ListBuffer

/**
  * Parses calling of an objects method
  * E.g. .methodCall(10,20)
  */
class ObjectMethodCallParser extends Parser[ObjectMethodCallBlock] {
  /**
    * A list of all regular stack
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    "\\.[a-zA-Z][a-zA-Z0-9]*\\((([ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*[ ]?)*)*\\)"
  )


  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectMethodCallBlock = {

    tokenizer.nextToken
    val methodName: String = tokenizer.nextToken.token
    tokenizer.nextToken // ")"
    var nextToken: String = tokenizer.nextToken.token
    val paramType: String = ""
    var paramName: String = ""
    val parameters: ListBuffer[Parameter] = new ListBuffer[Parameter]
    while (nextToken != ")") {
      {
        if (nextToken == ",") {
          nextToken = tokenizer.nextToken.token
        } else {
          // todo find the paramType. Utilities add a method to get the type
          paramName = nextToken.trim
          val parameter = new Parameter(paramType, paramName)
          parameters.append(parameter)
          nextToken = tokenizer.nextToken.token
        }
      }
    }
    new ObjectMethodCallBlock(superBlock, methodName, parameters)
  }
}
