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
https://github.com/Michael2109/cobalt/wiki/Language-Specifications

# Getting Starting
To view how to get up and running please view our tutorial!
https://github.com/Michael2109/cobalt/wiki/Getting-Started

# Example (Target)
All code is subject to change. 
```scala
// Example class with constructor
public class ExampleClass(arg1: Int, arg2: Int)


    // Expression - Type inference
    x = 5
    
    
    // Expression - Type specified
    y:Int = 5
    
    
    // Mutable variable
    mutable z:Int = 10
    
    
    // Expression Block
    a:Int = 
        if(true)
            10
        else
            20
           
           
    // Function - Square value (Similar to F#)
    square(x:Int) = x * x
    
    
    // Add - Specified return type
    add(a:Int, b: Int):Int = x + b
    
    
    // Subtract - Add return type
    subtract(a:Double, b:Double) = 
        a - b
        
        
    // Add One - lambda
    addOne = fun x -> x + 1
    
    
    // Add values to a formatted String
    formatValues:String = fun a b c -> "$a $b $c"
    
    
    // Return list with x added to each element
    getAdded(list, x) = list.map(fun i -> i + x)
    
    // Function within function
    getEvenNumbers(list:List[Int])
        isEven(x: Int) =
            x % 2 == 0
        list.filter(isEven(_))
       
    /* Same/Similar to Scala */
    
    // Value with function literal
    doubleValue = (i: Int) => i * 2
    
    
    // Function with boolean return type
    f1 = (i: Int) => { i % 2 == 0 }
    
    
    // Example of other formats
    f2: (Int) => Boolean = i => 
        i % 2 == 0
        
    f3: Int => Boolean = i => 
        i % 2 == 0
        
    f4: Int => Boolean = i => i % 2 == 0
    
    f5: Int => Boolean = _ % 2 == 0
    
    
    // Anonymous function
    modMethod1(i: Int) = i % 2 == 0
    
    modMethod2(i: Int) = 
        i % 2 == 0
        
    modMethod3(i: Int): Boolean = i % 2 == 0
    
    modMethod4(i: Int): Boolean = 
        i % 2 == 0
    
    
// Singleton (Similar to Scala)
public object ExampleMain()

    // Entry Point
    main(args: Array[String])
    
    
        obj = new ExampleClass()
  
        
        // Call a instance method
        println (obj.square(10))
        
        
        // Create a list
        list = [1 .. 10]
        
        
        // Find the sum of all list elements squared
        lSquareSum = list.map(obj.square(_)).sum
        
        // Alternative
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
            println("Is true")
        elif (true)
            println("Is true")
        else 
            println("All others false")
```
