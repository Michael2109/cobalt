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

#Language Specifications
To view all of the language specifications please view our wiki!  
https://github.com/Michael2109/cobalt/wiki


#Getting Starting
To view how to get up and running please view  
https://github.com/Michael2109/cobalt/wiki/Getting-Started
