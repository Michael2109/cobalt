# Cobalt

Cobalt is an object oriented and functional language that runs on the JVM.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. Also to combine object oriented and functional aspects to make the language very powerful with few lines. 

This project is new and if it is pointed in the right direction could be a language used in the future!   
If you are interested then please get in contact as you could join in from the start!   

##Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. 
As the project is in the alpha stages please get in contact to discuss any larger changes and/or features you think would be interesting to include. 

#Desired Features   
###Variable Types
```
byte b = 1           // 8 bit   
short s = 2          // 16 bit  
int i = 3            // 32 bit  
long l = 4           // 64 bit  
float f = 5.0f       // 32 bit  
double d = 6.0       // 64 bit  
char c = 'g'        
String ss = "Example"  
ClassName obj = new ClassName()  
```
###Modifiers   
Default modifier is private
```
protected            // Can be accessed by a child class
public               // Can be accessed outside the package
```
###Operators
```
a + b                // Addition   
a - b                // Subtraction
a * b                // Multiplication
a / b                // Division
a % b                // Modulus
a += b               // Increment by value. 
```
 
###While Loop
```
while (condition):
   doSomething
```
###For Loop (Including index)
```
for(element <- names index i):
   doSomething(i)
```
###For Each
```
list.foreach:
    doSomething
``` 
###Do While
```
do:
   doSomething
   while(condition)
```
###Continue
Skip the current iteration
```
continue
```
###Break
Break out of a loop
```
break
```

###Methods to convert strings to other types
Often strings represent other types such as integers or doubles. To convert these in Java you have to use "Integer.parseInt(str);" to convert from a string to an integer. Instead these methods should be parse of the language and the compiler should deal with it. 
```
"10".toInt  
"true".toBoolean  
"20.5".toDouble  
"05/15/2017".toDate  
```
###Switch
In Java a switch statement case can only contain one condition for each case. Instead multiple conditions could be allowed.
```
switch(value):
    case value.method == 1 and value.otherMethod == 5:
      doSomething()
    case value.method == 2:
      doSomethingElse
    default:
      print("Default Called") 
```
###Arrays
```
val example = {1,2,3,4,5}
print(example[2])           // Outputs 3
```

###Strings    
To make the code more readable and require less typing variables could be included inside the string and the compiler do all of the work.    
```
var x : int = 5
var y : int = 10
var s : String = "x = $x and y = $y"
```
If outputting a method return
```
var obj : Example = new Example
var s : String = "Hello ${obj.getWorld}"
```

###If Statements
If statement comparisons could be reduced down to much smaller sizes. This would make it less to type and much easier to read.   
*Before*   
```
if(x < 10 && x > 0):
   doSomething
```
*After*
```
if(0 < x < 10):
    doSomething
```

###Open type System       
When using Java I had to write programs that would return JSON to be decoded by PHP APIs. This would mean that external libraries would have to be used such as GSON etc. With GSON you would have to create a class structure that represents the JSON file you are reading in. This would take time and isn't very tidy if there are multiple nested lists.   
Instead an Open Type System could be used. This would allow for JSON (and other formats) to be treated as objects. The elements can be converted into variables and then be referenced which would be highly readable and would save generating unneeded classes. 

####Example xml (JSON to use the same concept)
*xml file -> Book.xml*   
```xml
<?xml version="1.0"?>
<catalog>
   <book id="bk101">
      <author>Gambardella, Matthew</author>
      <title>XML Developer's Guide</title>
      <genre>Computer</genre>
      <price>44.95</price>
      <publish_date>2000-10-01</publish_date>
      <description>An in-depth look at creating applications 
      with XML.</description>
   </book>
   <book id="bk102">
      <author>Ralls, Kim</author>
      <title>Midnight Rain</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-12-16</publish_date>
      <description>A former architect battles corporate zombies, 
      an evil sorceress, and her own childhood to become queen 
      of the world.</description>
      </book>
</catalog>
```
*Cobalt file*
```
var xml = new Book() // Define an xml object
print(xml.catalog.book[0].author) // output "Gambardella, Matthew"
```

##Desired Syntax  
##Code Example
```
package asm
import compiler.block.ifs.IfBlock

class MyCode(x : int, y : int):    

   // Constructor Calls    
   print("Hello World!")    
   var z : int = 2    
   x += 5    
   
   // Method definition    
   method(x : int)<-void:        
      while (x < 20):            
      x += 1        
   if (x < 10):            
      print("Something else")
   var test : ExampleClass = new ExampleClass()        
   test.methodCall()        
   
   // Main method (Will contain "static" keyword    
   main()<-void:        
      val z :int = 10        
      val zy:int = 15        
      var myCodeTest:MyCode = new MyCode(z, zy)        
      val x:int = 0        
      myCodeTest.method(x)
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
"compiled\MyCode" "src\main\java\asm\MyCode"
```
Run the "Runtime.java" file to execute the application. (Currently converting Runtime.java to Runtime.scala)

##Debugging
Open the generated class file to decompile the code and check the code was generated correctly. 

##Code Example - Compilable
```
package asm

import compiler.block.ifs.IfBlock

class MyCode(int x, int y):

    // Constructor Calls
    print("Hello World!")
    int z = 2
    x += 5

    // Method definition
    void method(int x):
        while (x < 20):
            x += 1
        if (x < 10):
            print("Something else")
           
        ExampleClass test = new ExampleClass()
        test.methodCall()
    
    // Main method (Will contain "static" keyword
    void main():
        int z = 10
        int zy = 15
        MyCode myCodeTest = new MyCode(z, zy)
        int x = 0
        myCodeTest.method(x)
```
