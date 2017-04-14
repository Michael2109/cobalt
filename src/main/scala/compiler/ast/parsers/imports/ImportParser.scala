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

package compiler.ast.parsers.imports

import compiler.ast.blocks.Block
import compiler.ast.blocks.imports.ImportBlock
import compiler.ast.parsers.Parser
import compiler.tokenizer.Tokenizer
import compiler.tokenizer.tokens.EmptyToken

class ImportParser extends Parser[ImportBlock] {

  /**
    * A list of all regular stack
    *
    * @return
    */
  override def getRegexs: List[String] = List("import [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*")

  def parse(superBlock: Block, tokenizer: Tokenizer): ImportBlock = {

    println(tokenizer.nextToken) // "import"

    var directory: String = ""

    // Get the string value of the next token.;
    var nextToken: String = ""
    while (!tokenizer.peek.tokenType.isInstanceOf[EmptyToken]) {
      println("TOKEN -> " + tokenizer.peek.token + "   ->   " + tokenizer.peek.tokenType)
      nextToken = tokenizer.nextToken.token
      println("NEXTTOKEN:" + nextToken)
      if (nextToken == ".") {
        directory += "/"
      }
      else {
        directory += nextToken
      }


    }
    println()
    new ImportBlock(directory, nextToken)
  }
}
