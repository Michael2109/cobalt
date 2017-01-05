package compiler;

import compiler.block.Block;

public class Utils <T>{

    public T getBlockType(Block block){
        return null;
    }


    // Prints block types with indentation.
    public void printBlocks(Block block, int indentation){
        String indentationString = "";
        for(int i = 0; i < indentation; i++){
            indentationString += "    ";
        }
        System.out.println(indentationString + block.getClass());
        for(Block sub : block.getSubBlocks()){
            printBlocks(sub, indentation + 1);
        }
    }

}
