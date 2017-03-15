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

package compiler.structure.parsers.imports

import compiler.structure.blocks.Block
import compiler.structure.blocks.imports.ImportBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

class ImportParser extends Parser[ImportBlock] {

  /**
    * A list of all regular expressions
    *
    * @return
    */
  override def getRegexs: List[String] = List("import [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*")

  def parse(superBlock: Block, tokenizer: Tokenizer): ImportBlock = {

    tokenizer.nextToken // "import"

    var fileLoc: String = tokenizer.nextToken.token // Get the string value of the next token.;
    var nextToken: String = tokenizer.nextToken.token
    var fileName: String = nextToken

    while (nextToken != "") {
      {
        if (nextToken == ".") {
          fileLoc += "/"
        }
        else {
          fileLoc += nextToken
        }
        fileName = nextToken
        nextToken = tokenizer.nextToken.token
      }
    }
    val i: Int = fileLoc.lastIndexOf("/")
    fileLoc = if (i > -1) fileLoc.substring(0, i)
    else fileLoc
    new ImportBlock(fileLoc, fileName)
  }
}
