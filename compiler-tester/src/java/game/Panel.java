package game;import javax.swing.JPanel;
import java.awt.Graphics;public final class Panel extends JPanel implements Runnable{
public Thread thread=new Thread(this);public Thread thread(){return new Thread(this);} public int x=50;public int x(){return 50;} public int y=100;public int y(){return 100;} public int dx=1;public int dx(){return 1;} public int dy=1;public int dy(){return 1;}
private boolean alive=true;private boolean alive(){return true;}private final Game game;public Panel(Game game){this.game=game;thread.start();} public void update(){
x=x + dx;
y=y + dy;
if(x < 0 || x > 750){
dx=dx * -1;}
if(y < 0 || y > 550){
dy=dy * -1;}}
 public void paint(Graphics g){
super.paintComponent(g);
g.drawRect(x, y, 50, 50);}
@Override public void run(){
try{while(true){
Thread.sleep(7);
update();
repaint();}}
catch(final Exception e){System.out.println("Error: Thread failed");}}}