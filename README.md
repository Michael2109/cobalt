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
Make sure to have a quick read through the wiki pages to get an idea of where everything is heading. The project is in the alpha stages please get in contact to discuss any large changes and/or features you think would be interesting to include!

# Getting Started

#### Install IntelliJ IDE
https://www.jetbrains.com/idea/download/#section=windows

#### Install HaskForce IntelliJ plugin
http://haskforce.com/

#### Install Stack 
https://docs.haskellstack.org/en/stable/README/

### Known Problems and Fixes 
1. 
##### Stack error
`The program 'cpphs' is required but it could not be found.`
##### Fix
`stack install cpphs`

#### Setting up
1. `File -> New -> Project from version control -> Github`
2. Using CMD/terminal navigate to the `cobalt/compiler` directory 
3. Run `stack build` 
4. Run `stack exec compiler-exe` to run the the main function in `app/Main`. This currently compiles to Java code, compiles with javac and executes.
5. Run `stack test` to execute all tests. 

# Example 
##### Basic 2D animation displaying a square bouncing around the screen
```
import javax.swing.JFrame

# Frame
class Frame extends JFrame

val game: Game = new Game()
add(new Panel(game))
setTitle("Game")
setResizable(True)
setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
pack()
setSize(800, 600)
setVisible(True)
```
```
import javax.swing.JPanel
import java.awt.Graphics

# Panel
class Panel(game: Game) extends JPanel implements Runnable

public
  val thread: Thread = new Thread(this)
  var x: int = 50
  var y: int = 100
  var dx: int = 1
  var dy: int = 1

private
  var alive: boolean = True

thread.start()

update : void
update =
  x = x + dx
  y = y + dy

  if (x < 0 or x > 750)
    dx = dx * -1
  if (y < 0 or y > 550)
    dy = dy * -1

paint : Graphics -> void
paint g =
  super.paintComponent(g)

  g.drawRect(x, y, 50, 50)

@Override
run : void
run =
  try
    while(True)
      Thread.sleep(7)
      update()
      repaint()
  catch(e: Exception)
    println "Error: Thread failed"
```
```
# Main
object Main

main : [String] -> void
main args =
  val frame: Frame = new Frame()
```
```
# Game (Currently empty)
class Game
```
