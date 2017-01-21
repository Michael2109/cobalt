# JVM-Compiler #

An object oriented and functional language that runs on the JVM.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. Also to combine object oriented and functional aspects to make the language very powerful with few lines. 

### Example Code (So Far)
```
package asm

import compiler.block.ifs.IfBlock

class MyCode():

    // Constructor Calls
    print("Hello World!")
    int z = 2

    void method():
        int x = 10
        while (x < 20):
            print("Something")
            x += 1
        if (x < 10):
            print("Something else")
        ExampleClass test = new ExampleClass()
        test.add()
        
    void main():
        MyCode myCodeTest = new MyCode()
        myCodeTest.method()
```
## How it works

####Tokenization
The tokenizer takes a String and converts it into tokens depending on a set of rules.
```
var tokenDatas = new ArrayList[TokenData]
tokenDatas.add(new TokenData(Pattern.compile("^((-)?[0-9]+)"), TokenType.INTEGER_LITERAL))
tokenDatas.add(new TokenData(Pattern.compile("^((-)?[0-9]+[.][0-9])"), TokenType.DOUBLE_LITERAL))
tokenDatas.add(new TokenData(Pattern.compile("^([+][=])"), TokenType.ADD_OPERATOR))
tokenDatas.add(new TokenData(Pattern.compile("^([-][=])"), TokenType.SUBTRACT_OPERATOR))
tokenDatas.add(new TokenData(Pattern.compile("^([*][=])"), TokenType.MULTIPLY_OPERATOR))
tokenDatas.add(new TokenData(Pattern.compile("^([/][=])"), TokenType.DIVIDE_OPERATOR))
tokenDatas.add(new TokenData(Pattern.compile("^(\".*\")"), TokenType.STRING_LITERAL))
tokenDatas.add(new TokenData(Pattern.compile("^([;])"), TokenType.END_STATEMENT))
tokenDatas.add(new TokenData(Pattern.compile("^([:])"), TokenType.COLON))
tokenDatas.add(new TokenData(Pattern.compile("^([==])"), TokenType.EQUAL_TO))
tokenDatas.add(new TokenData(Pattern.compile("^([<])"), TokenType.SMALLER_THAN))
tokenDatas.add(new TokenData(Pattern.compile("^([<=])"), TokenType.SMALLER_THAN_EQUAL))
tokenDatas.add(new TokenData(Pattern.compile("^([>])"), TokenType.LARGER_THAN))
tokenDatas.add(new TokenData(Pattern.compile("^([>=])"), TokenType.LARGER_THAN_EQUAL))
tokenDatas.add(new TokenData(Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), TokenType.IDENTIFIER))
```
####Parsing
Each line is looped through. A check is made to find what the line represents. E.g "int x = 5" would be parsed as an integer definition. 
If the line is parsed then it is tokenized. 
After being split into the tokens the tokens are traversed through to get the relevant information.
```
class IntegerParser extends Parser[IntegerBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("int[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]+[=][ ]+[0-9]+")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): IntegerBlock = {
    tokenizer.nextToken // skip int
    val name: String = tokenizer.nextToken.getToken // get the variable name
    tokenizer.nextToken // skip the "="
    val value: String = tokenizer.nextToken.getToken // get the value assigned
    return new IntegerBlock(superBlock, name, value) // return an IntegerBlock with the arguments entered
  }
}
```
####Block Creation
Blocks are used to represent pieces of code in the memory.  
For every parser there is a block.  
E.g. An IntegerParser has an IntegerBlock. If the IntegerParser finds an integer definition it would create an IntegerBlock object and add it to a tree structure.  
A block uses the arguments input from the parser to generate the ASM code.  
A getOpeningCode(), getBodyCode() and getClosingCode() method is in each Block. This is called when the code is looped through to generate the ASM code.  
```

class IntegerBlock(var superBlock: Block, var name: String, var value: String) extends Block(superBlock, false, true) {
  private var `type`: String = "int"

  def init() {
  }

  def getName: String = {
    return name
  }

  def setName(name: String) {
    this.name = name
  }

  def getValue: String = {
    return value
  }

  def setValue(value: String) {
    this.value = value
  }

  def getType: String = {
    return `type`
  }

  def setType(`type`: String) {
    this.`type` = `type`
  }

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    return "mv.visitLdcInsn(" + value + ");\n" + "mv.visitVarInsn(ISTORE," + getId + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "int: " + name + " = " + value
  }
}
```

####ASM Code Generation
The tree structure is looped through calling the getOpeningCode(), getBodyCode() and getClosingCode() methods to recursively generate the file.  
```
 def generateASM(block: Block) {
    if (block.isInstanceOf[MethodBlock]) {
      val b: MethodBlock = block.asInstanceOf[MethodBlock]
      p(b.getOpeningCode)
      p(b.getBodyCode)
    }
    else {
      if (block.getOpeningCode != null && block.getOpeningCode != "") p(block.getOpeningCode)
      if (block.getBodyCode != null && block.getBodyCode != "") p(block.getBodyCode)
    }
    for (sub <- block.getSubBlocks) {
      generateASM(sub)
    }
    if (block.getClosingCode != null && block.getClosingCode != "") p(block.getClosingCode)
  }
```
##Bytecode
Bytecode is generated using ASM.  

##Opening the project
#####IntelliJ
File->New->Project from version control->GitHub  
Git Repository URL (The project repository URL)  
Parent Directory (Where you would like to store the project)  
Click OK  
Select Gradle on the right panel.   
Press the refresh all Gradle projects button.  

##Running the application. 
As program arguments you need the input file and the output file location. Remove the file extension like so.   
```
"C:\Users\Michael\Desktop\JVM Compiler\compiled\MyCode" "C:\Users\Michael\Desktop\JVM Compiler\src\main\java\asm\MyCode"
```
Run the "Runtime.java" file to execute the application. (Currently converting Runtime.java to Runtime.scala)



