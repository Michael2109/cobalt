# Cobalt

Cobalt is an object oriented and functional language that runs on the JVM.

Scala has shown that difficulties can arise in development when too many methods can be used to complete the same task. This can lead to multiple developers writing code in various styles which then leads to a difficult to manage codebase. The target is to create a powerful language that has similar features to Scala but to also create more clear structures to allow for teams to manage code more efficiency.

The syntax will be similar to languages such as Java, Scala, F# and python.  

# Main Features
* Object oriented and functional  
* Statically typed
* Immutable by default
* Classes and methods Sealed by default
* Non-nullable
* High readability    
* Simplistic Asynchronous Programming  
* Operator overloading

# Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. The project is in the alpha stages please get in contact to discuss any large changes and/or features you think would be interesting to include!

# Language Specifications
To view all of the language specifications please view our wiki!  
https://github.com/Michael2109/cobalt/wiki

# Getting Starting
To view how to get up and running please view  
https://github.com/Michael2109/cobalt/wiki/Getting-Started

# Example (Target)
All code is subject to change. 
```scala
// Example class with constructor
public class ExampleClass(arg1: Int, arg2: Int)

    // Expression - Type inference
    x = 5
    
    // Expression - Type specified
    x:Int = 5
    
    // Expression Block
    x:Int = 
        if(true)
            10
        else
            20
           
    // Function - Square value (Similar to F#)
    let square(x:Int) = x * x
    
    // Add - Specified return type
    let add(a:Int, b: Int):Int = x + b
    
    // Subtract - Add return type
    let subtract(a:Double b:Double): = 
        a - b
        
    // Add One - lambda
    let addOne = fun x -> x + 1
    
    // Add values to a formatted String
    let formatValues:String = fun a b c -> "$a $b $c"
    
    // Return list with x added to each element
    let getAdded(list, x) = list.map(fun i -> i + x)
    
// Singleton (Similar to Scala)
public object ExampleMain()

    // Entry Point
    let main(args: Array[String]) =
        obj = new ExampleClass()
        
        // Without parenthesis
        println (obj.square 10)
        
        // With parenthesis
        println (obj.square(10))
        
        // Create a list
        list = [1 .. 10]
        
        // Find the sum of all list elements squared
        lSquareSum = list.map(obj.square(_)).sum
        
        // Alternative 1
        lSquareSumAlt1 = list.map(obj.square).sum
            
        // Match 
        exampleMatch x:Int = 
            match x with
            1 -> "a"
            2 -> "b"
            _ -> "z"
        
        // If statements - inline
        if (square(12) > 100) println "Larger than 100"
        
        // If statements
        if (square(12) > 100)
            println "Larger than 100"
            
        // Elif and else
        if (false)
            println "Is true"
        elif (true)
            println "Is true"
        else 
            println "All others false"
```
