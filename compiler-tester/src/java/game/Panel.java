package game;import javax.swing.JPanel;
import java.awt.Graphics;public final class Panel extends JPanel implements Runnable{
Thread thread=new Thread(this);
int x=100;
int y=100;
public void ClassTest(){
thread.start();}
public void update(int x){
System.out.println("Updating");}
public void paint(Graphics g){
super.paintComponent(g);
g.drawRect(x, y, 50, 50);}
public void run(){
int x=10 + 1;}}