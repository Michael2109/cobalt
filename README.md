# JVM-Compiler #

An object oriented and functional language that runs on the JVM.

The language syntax is similar to that of Java and Scala and uses indentation instead of curly braces for defining blocks of code.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. 

The compiler parses the file and splits it into a tree structure containing blocks. This structure is then converted into byte code.

### Example Code

```
package asm

import compiler.block.ifs.IfBlock

class MyCode(int x, int y):

    print("Hello World!")
    int z = 2

    void method1(int number):
        print ("Other test")

    print("Test")
    void otherMethod(int number):
        int x = 10
        while (x == 20):
            print("Something")
        if (x < 10):
            print("Something else")
        IfBlock test = new IfBlock()
        test.add()
```
