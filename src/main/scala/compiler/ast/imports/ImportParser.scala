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

package compiler.ast.imports

import compiler.ast.{Block, Parser}
import compiler.tokenizer.Tokenizer
import compiler.tokenizer.tokens.EmptyToken

class ImportParser extends Parser[ImportBlock] {

  /**
    * A list of all regular stack
    *
    * @return
    */
  override val regex: String = "import [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*"

  def parse(superBlock: Block, tokenizer: Tokenizer): ImportBlock = {

    tokenizer.nextToken // "import"

    var directory: String = ""

    // Get the string value of the next token.;
    var nextToken: String = ""
    while (!tokenizer.peek.tokenType.isInstanceOf[EmptyToken]) {

      nextToken = tokenizer.nextToken.token

      if (nextToken == ".") {
        directory += "/"
      }
      else {
        directory += nextToken
      }
    }

    // Remove the file name
    directory = directory.substring(0, directory.lastIndexOf("/"))

    new ImportBlock(directory, nextToken)
  }
}
