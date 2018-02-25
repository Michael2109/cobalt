package game;import javax.swing.JPanel;
import java.awt.Graphics;public final class Panel extends JPanel implements Runnable{
private Thread thread=new Thread(this);private boolean threadBool=false;public Thread thread(){ if(!threadBool){threadBool=true;thread=new Thread(this);}return thread;} private int x=50;private boolean xBool=false;public int x(){ if(!xBool){xBool=true;x=50;}return x;}public void x_(final int x){this.x=x;} private int y=100;private boolean yBool=false;public int y(){ if(!yBool){yBool=true;y=100;}return y;}public void y_(final int y){this.y=y;} private int dx=1;private boolean dxBool=false;public int dx(){ if(!dxBool){dxBool=true;dx=1;}return dx;}public void dx_(final int dx){this.dx=dx;} private int dy=1;private boolean dyBool=false;public int dy(){ if(!dyBool){dyBool=true;dy=1;}return dy;}public void dy_(final int dy){this.dy=dy;}
private boolean alive=true;private boolean aliveBool=false;private boolean alive(){ if(!aliveBool){aliveBool=true;alive=true;}return alive;}private void alive_(final boolean alive){this.alive=alive;}private final Game game;public Panel(Game game){this.game=game;thread.start();} public void update(){
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