package compiler.parser.imports

import compiler.block.Block
import compiler.block.imports.ImportBlock
import compiler.parser.Parser
import java.io.File

import compiler.tokenizer.Tokenizer

class ImportParser extends Parser[ImportBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("import [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): ImportBlock = {
    tokenizer.nextToken // import
    var fileLoc: String = tokenizer.nextToken.getToken // Get the string value of the next token.;
    var nextToken: String = tokenizer.nextToken.getToken
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
        nextToken = tokenizer.nextToken.getToken
      }
    }
    val i: Int = fileLoc.lastIndexOf("/")
    fileLoc = if ((i > -1)) fileLoc.substring(0, i)
    else fileLoc
    return new ImportBlock(fileLoc, fileName)
  }
}