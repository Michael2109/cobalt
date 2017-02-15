package compiler.parser.comments

import compiler.block.Block
import compiler.block.comments.CommentBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.block.comments.CommentBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class CommentParser extends Parser[CommentBlock] {

  def shouldParse(line: String): Boolean = line.matches("//.*")

  def parse(superBlock: Block, tokenizer: Tokenizer): CommentBlock = new CommentBlock(superBlock)

}
