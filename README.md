# Cobalt

Cobalt is an object-oriented and functional language that runs on the JVM.

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
# Game (Currently empty
class Game
```
