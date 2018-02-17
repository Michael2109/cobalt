package game;import javax.swing.JPanel;
import java.awt.Graphics;public final class Panel extends JPanel implements Runnable{
Thread thread=new Thread(this);
int x=100;
int y=100;
int dx=1;
int dy=1;
public Panel(){
thread.start();}
public void update(){
x=x + dx;
y=y + dy;
if(x > 800){
dx=-1;}
if(x < 0){
dx=1;}
if(y < 0){
dy=1;}
if(y > 600){
dy=-1;}
System.out.println("Updating");}
public void paint(Graphics g){
super.paintComponent(g);
g.drawRect(x, y, 50, 50);}
public void run(){
try{while(true){
Thread.sleep(7);
update();
repaint();}}
catch(Exception e){}}}