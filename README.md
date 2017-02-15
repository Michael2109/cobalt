# Cobalt

Cobalt is an object oriented and functional language that runs on the JVM.

The target is to create a language that is highly scalable, readable and thread safe. Also to combine object oriented and functional aspects to make the language very powerful with few lines. 

Currently the project is at the initial stages but is built on a very strong structure that should allow for the flexibility to move in any direction we please. I'm very open to new ideas for the project so if you feel anything should be added/considered then add it as an issue!   

If you are interested then please get in contact!   

#Main Features
* Thread Safe - Automatic Synchronization  
* Simplistic Asynchronous Programming  
* High readability   
* Condensed down code with the same functionality   
* Object oriented and functional  
* Focused on immutability
* Not nullable
* Classes sealed by default
* Language design pushes for a strong code structure  
* Instead of returning "void" return "this" for methods  

###Possible Features
* Primitives are wrapper classes  
* Operator overloading

#Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. 
As the project is in the alpha stages please get in contact to discuss any larger changes and/or features you think would be interesting to include. 

#Scala main similarities and differences?  
###Similarities
* Like Scala it would be frowned upon to use "var" and "null" values as this promotes well written code. Immutability would lower the overall amount of errors in a program as it develops to a larger scale.   

###Differences
* One issue I've found with Scala is that it there are a variety of ways that the same code can be written. In production this can lead to many different code styles being used that can lower productivity if not controlled correctly. Cobalt will be a language that is focused on having a very strong structure. This will be done by having the syntax written in such ways to allow for high readability and following of particular rules.  
* Classes would be automatically sealed unless the user explicitly changes this. This means that only when the user allows for methods to be inherited when needed.  
* An open type system would allow for other file formats such as XML and JSON to be treated as though they are objects. This would mean that elements within the files would be easily accessible.  
* Automatic synchronization to allow for the user to not worry about concurrency errors.  

#Features   

###Single line comment
```
// This is a single line comment
```
###Multiline comment
```
/* 
    This is a multi line comment   
*/
```
###Cobalt documentation comment
```
/**
 * Class Description
 * @param nameInit The persons name
 */
 class Person(nameInit:String):
    
    /** The age of the person **/
    var name:String = nameInit
    
    /**
     * Creates a greeting method for a perso
     *
     * @param otherPerson The person to greet
     * @return the greeting message
     */
     def greet(otherPerson:String) <- String:
         "Hello $otherPerson"
```
###Triple single quoted String (To be discussed)
Instead of having to separate a String out like so
```
"Multiple lines" +
"Are split up" +
"into sections"
```
This could be used instead
```
'''
Multiple lines
are treated 
as one
'''
```
###Special Characters
To ascape special characters use "\"   
```
"\"Something\""
```
| Escape Sequence | Character        |
| --------------- |:----------------:|
| \t              | Tabulation       |
| \b              | Backspace        |
| \n              | Newline          |
| \r              | Carriage Return  |
| \f              | Formfeed         |
| \\              | Backslash        |
| \'              | Single Quote     |
| \"              | Double Quote     |

###Double quoted String  
```
"This is a double quoted String"  
```
###String interpolation   
To make the code more readable and require less typing variables could be included inside the string and the compiler do all of the work.    
```
var x : int = 5
var y : int = 10
var s : String = "x = $x and y = $y"
```
If outputting a variable within a class
```
var y : Example = new Example
var s : String = "y = ${y.someVar}"
```
###Slashy Strings
Useful for regular expressions instead of having to escape single or double quotes   
```
var example:String = /Example String/
```
###Printing
```
print("Hello World")            // No Return
println("Hello World")          // Return
printf("Hello %s\n, "world")    // Format
```
###Variable Types
```
val b:byte = 1           // 8 bit   
val s:short = 2          // 16 bit  
val i:int = 3            // 32 bit  
val l:long = 4           // 64 bit  
val f:float = 5.0f       // 32 bit  
val d:double = 6.0       // 64 bit  
va; bo:boolean = true 
val c:char = 'g'        
val ss:String = "Example"  
val obj:ClassName = new ClassName()  
```
###Modifiers   
```
private              // Can only be accessed within the class
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
a ^^ b               // Power
a += b               // Increment by value. 
```
###Binary
Allow use of binary values as integers
```
var example:int = 0100100101001b   
```
###Hexadecimal  
Allow the use of hexadecimal values as integers
```
var example:int = 0xab
```
###Arrays - Mutable (To be defined)
```
Undefined
```
###List - Immutable
```
val example:List = List(1,2,3)
```
```
val example:List = List(1,2,3
                    4,5,6)
```
###Maps
Indentation is ignored  if the map extends past multiple lines.
```
val example:Map<String,Integer> = ("One"->1, "two"->2,
                                   "three"->3, "four"->4)
```
###If Statements
```
if (condition):
    doSomething
else:
    doSomethingElse
    
// inline
if (condition) doSomething
```
###While Loop
```
while (condition):
   doSomething
   
// inline
while (condition) doSomething
```
###For Loop (Including index)
```
for(element <- elements index i):
   doSomething(i)
   
// inline
for(element <- elements index i) doSomething(i)
```
###For Loop (Map example)
```
for(element <- map):
    print(element.key + " " + element.value)
```
###Nested For Loop
```
for(l1Element <- list1 : l2Element <- list2):
    doSomething
```
###Nested For Loop (Including index)
```
for(l1Element <- list1 index x: l2Element <- list2 index y):
    doSomething(x,y)
```
###For Each
```
list.forEach:
    doSomething
    
// inline
list.forEach doSomething
``` 
###Do While
```
do:
   // do something
   while(condition)
```
###Try catch
```
try:
    // do something
catch:
    case foo: FooException => handleFooException(foo)
    case bar: BarException => handleBarException(bar)
    case _: Throwable => println("Got some other kind of exception")
finally:
    // do something
```
###Continue (Discuss whether required)
Skip the current iteration
```
continue
```
###Break (Discuss whether required)
Break out of a loop
```
break
```
###Return
```
return
```
###Package
```
package dir/subDir
```
###Imports
```
import dir.subDir.{ClassName1, ClassName2}
import dir.ClassName
```
### Constructor
The constructor is anything within the class but outside of other methods. Otherwise constructors can be overloaded.   
Constructors can have default values.
```
class MyClass:
    
    a, b:int
    
    // Constructor
    MyClass() : this(42)
        print("Constructor")
    
    // Overloading a constructor
    MyClass(a:int = 5, b:int = 10):
        this.a = a  //a = 42
        this.b = b  // b = 10
```
###Method definition
Methods can have default values.
```
def methodName(x : int = 5) <- void:
    // doSomething
```
```
def methodName(x : int = 5):
    // doSomething
    //returns this
```
###Method calls
Methods with no parameters don't require parenthesis
```
obj.methodName       // No Parameters
obj.methodName(5)    // int parameter
```
###Partial Application
```
// undefined syntax
```
###Closures
```
val factor = 5
val multiplier = (i:int) = i * factor
```
```
power()<-int: 
  var i:int = 0
  return 
    next: function(): 
      var result:int = Math.pow(i, 2)
      i++
      return result
    
var gen = power();
print(gen.next()) // 0
print(gen.next()) // 1
```
###Anonymous Functions (Copied from Scala - New syntax to be defined with similar concepts)
```
var inc = (x:Int) => x+1   
var x = inc(7)-1   
```
```
var mul = (x: Int, y: Int) => x*y   
println(mul(3, 4))
```
```
var userDir = () => { System.getProperty("user.dir") }
println( userDir )
```

###Fat arrows
```
var names = ["James", "Andrew", "John"]
print(names.map((name) => name + ' Smith'))[0]) // James Smith
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
      // do something
    case value.method == 2:
      // do something
    default:
      print("Default Called") 
```
###Arrays
```
val example = {1,2,3,4,5}
print(example[2])           // Outputs 3
```
###Type Of  
Gets the type of an object. In an if statement it will automatically cast if true.  
```
if(obj typeOf ExampleClass):
    // Automatically cast to Example class if true
```

###Asynchronous Multithreading
Would start a new thread possible from a thread pool. This could either use default values or have config parameters for how many threads are in the pool etc.
```
// Default
async:
    // do something
    
//Configured - Pass a map
async(configMap):
    // do something
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
###UTF-8 Encoded   
The source files should be UTF-8 encoded.
###Time class
In Java
```
long start = System.currentTimeMillis();
// Do Something
System.out.println(System.currentTimeMillis() - start);
```
In Cobalt
```
var start:long = Time.now   // Call static method in Time class
// Do Something
println(Time.since*(start))
```
###Reflection
Allow finding of types at runtime.
###Structures
Gets placed on the stack to increase performance.  
Allows for creating objects as though they are built in types.
```
struct ExampleStruct:
    exampleString:String
    exampleInt:int
```

##Code Example (Subject to change)
```
package asm
import compiler.block.ifs.IfBlock

abstract class MyCode(xx:int, yy:int):

    xx += 2

    public:
        def method1(x:int) <- void:
            while (x < 10):
                println("Hello World!")
                x += 1

        def main() <- void:
            var x:int = 10
            var y:int = 15
            var myCode:MyCode = new MyCode(x, y)
            var w:float = 2.0f
            var d:double = 2.0
            myCode.method1(y)
            print("Hello World")
            var ifBlock:IfBlock = new IfBlock()
            
        def abstractMethod <- int
            
    private:
        def privateMethod(x:int) <- void:
            println("Private Method")
            
    def method2(y:String):
        println(y)
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
"language\MyCode" "src\main\java\asm\MyCode"
```
Run the "Runtime.java" file to execute the application. (Currently converting Runtime.java to Runtime.scala)

##Debugging
Open the generated class file to decompile the code and check the code was generated correctly. 

