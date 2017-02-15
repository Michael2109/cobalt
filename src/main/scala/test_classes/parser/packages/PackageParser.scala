package test_classes.parser.packages

import test_classes.block.Block
import test_classes.block.imports.ImportBlock
import test_classes.block.packages.PackageBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

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
