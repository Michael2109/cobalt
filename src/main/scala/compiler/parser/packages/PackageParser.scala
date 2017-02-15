package compiler.parser.packages

import compiler.block.Block
import compiler.block.packages.PackageBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class PackageParser extends Parser[PackageBlock] {

  def shouldParse(line: String): Boolean = line.matches("package [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*")

  def parse(superBlock: Block, tokenizer: Tokenizer): PackageBlock = {
    tokenizer.nextToken // skip "package"
    var directory: String = tokenizer.nextToken.token // Get the string value of the next token.;
    var nextToken: String = tokenizer.nextToken.token
    while (nextToken != "") {
      {
        if (nextToken == ".") {
          directory += "/"
        }
        else {
          directory += nextToken
        }
        nextToken = tokenizer.nextToken.token
      }
    }
    return new PackageBlock(directory)
  }
}
