# JVM-Compiler #

An object oriented and functional language that runs on the JVM.

The language syntax is similar to that of Java and Scala and uses indentation instead of curly braces for defining blocks of code.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. 

The compiler parses the file and splits it into a tree structure containing blocks. This structure is then converted into byte code.

### Example Code

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
