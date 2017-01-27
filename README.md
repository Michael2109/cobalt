# JVM-Compiler #

An object oriented and functional language that runs on the JVM.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. Also to combine object oriented and functional aspects to make the language very powerful with few lines. 

## Example Code (So Far)
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
The input file needs to be a .mlg file.  
The output file generated should be placed in "build/classes/main/asm/fileName".
```
"C:\Users\Michael\Desktop\JVM Compiler\compiled\MyCode" "C:\Users\Michael\Desktop\JVM Compiler\src\main\java\asm\MyCode"
```
Run the "Runtime.java" file to execute the application. (Currently converting Runtime.java to Runtime.scala)

##Debugging
Open the generated class file to decompile the code and check the code was generated correctly. 

