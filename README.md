# Cobalt

Cobalt is an purely functional language that runs on the JVM.

The syntax will be similar to languages such as Haskell and Scala.  

# Main Features
* Hybrid Language - Functional/Object Oriented
* Statically typed
* Immutable
* Can be used with other languages on the JVM
* Non-nullable
* High readability    

# Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. The project is in the alpha stages please get in contact to discuss any large changes and/or features you think would be interesting to include!

# Language Specifications
To view all of the language specifications please view our wiki!  
https://github.com/Michael2109/cobalt/wiki/Language-Specifications

# Getting Starting
To view how to get up and running please view our tutorial!
https://github.com/Michael2109/cobalt/wiki/Getting-Started

# Example Module (Target)
All code is subject to change. 
```

// Example module
module Example

import dir.subdir.Module

exampleFunction : Int -> Int -> Int
exampleFunction a b = 

    result:Int = a + b
    
    if(result > 10)
      // Do something
    else if(result < 5)
      // Do something
    else
      // Do something
          
square x = x * x

    
    
// Add values to a formatted String
formatValues : String -> String -> String -> String
formatValues a b c = "$a $b $c"
    
    // Return list with x added to each element
    getAdded(list, x) = list.map(i -> i + x)
    
    // Function within function
    getEvenNumbers(list:List[Int])
        isEven(x: Int) =
            x % 2 == 0
        list.filter(isEven(_))
  
    
    // Value with function literal
    doubleValue = (i: Int) -> i * 2
    
    
    // Function with boolean return type
    f1 = (i: Int) => { i % 2 == 0 }
    
    
    // Example of other formats
    f2: (Int) => Boolean = i -> 
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
    
    
        obj = new ExampleClass(50, 100)
  
        
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
            :1 -> "a"
            :2 -> "b"
            :_ -> "z"
        
        
        // If statements - inline
        if (square(12) > 100) println ("Larger than 100")
        
        // If statements
        if (square(12) > 100)
            println ("Larger than 100")
            
        // Elif and else
        if (false)
            println("Is true")
        elif (true)
            println("Is true")
        else 
            println("All others false")
```
