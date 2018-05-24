# Cobalt

[![Build Status](https://travis-ci.org/Michael2109/cobalt.svg?branch=0.1.x)](https://travis-ci.org/Michael2109/cobalt)

Cobalt is an object-oriented and functional language that runs on the JVM.

The syntax is similar to languages such as Haskell and Scala.  

# Main Features
* Hybrid Language - Functional/Object Oriented
* Statically typed
* Focusing on immutability
* Can be used with other languages on the JVM
* Non-nullable
* High readability    

# Project Contributions
To contribute to the Cobalt project, please send us a pull request from your fork of this repository!  
The project is in the alpha stages please get in contact to discuss any large changes and/or features you think would be interesting to include!

#### Setting up
1. Using CMD/terminal navigate to the `cobalt/compiler` directory 
2. Run `stack build` 
3. Run `stack exec compiler-exe` to execute the compiler. 
4. Run `stack test` to execute all tests. 

# Example Code
```
class ClassName 

    # Define a value (Immutable)
    let value: Int = 10
    
    # Define a variable (Mutable)
    let mutable variable: Int = 20
   
    # Define a method
    let method(): Int = 50 
    
    # Define a method with params (With block of code)
    let methodParams(x: Int, y: Int): Int = do
        if x > 20 then 
            100
        elif x > 10 then 
            200
        else 
            300
        
    # Function assigned to value
    let multiplyBy2 = fun (x: Int) -> x * 2
    
    # Higher order function
    let multiplyListBy2(list: List[Int]): List[Int] = list.map(multiplyBy2) 
    
    # Method with a function type as a parameter
    let functionParam(list: List[Int], x: fun Int -> Int): List[Int] = list.map(x)
```
