# Cobalt #

Cobalt is an object oriented and functional language that runs on the JVM.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. Also to combine object oriented and functional aspects to make the language very powerful with few lines.    

This project is new and if it is pointed in the right direction could be a language used in the future!   
If you are interested then please get in contact as you could join in from the start!   

##Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. 
As the project is in the alpha stages please get in contact to discuss any larger changes and/or features you think would be interesting to include. 

##Desired Features   
###Open type System       
Would allow for objects to be created using XML and JSON formats.  

###Example xml
*xml file -> Book.xml*   
```xml
xml<?xml version="1.0"?>
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
*Cobalt file -> XML.co*
```
class XMLTest:
    
    void testXML():
        var xml = new Book() // Define an xml object
        print(xml.catalog.book[0].author) // output "Gambardella, Matthew"
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

##Code Example
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
