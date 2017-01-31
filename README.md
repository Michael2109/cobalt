# Cobalt #

Cobalt is an object oriented and functional language that runs on the JVM.

The target is to create a language that is highly scalable and readable to allow for fast development and easier debugging. Also to combine object oriented and functional aspects to make the language very powerful with few lines.    

This project is new and if it is pointed in the right direction could be a language used in the future! If you are interested then please get in contact as you could join in from the start!   

##Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. 
As the project is in the alpha stages please get in contact to discuss any larger changes and/or features you think would be interesting to include. 

##Language Specifications
To view the aims for the language features please view the wiki page.  
https://github.com/Michael2109/Cobalt/wiki

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
