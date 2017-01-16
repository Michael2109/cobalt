package compiler.parser.comments;

import compiler.block.Block;
import compiler.block.comments.CommentBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class CommentParser extends Parser<CommentBlock> {

    @Override
    public boolean shouldParse(String line) {
        return line.matches("//.*");
    }

    @Override
    public CommentBlock parse(Block superBlock, Tokenizer tokenizer) {
        return new CommentBlock(superBlock);
    }
}