package test_files;import javax.swing.JPanel;
import java.awt.Graphics;public final class ClassTest extends JPanel implements Runnable{
private int x=5;private boolean xBool=false;public int x(){ if(!xBool){xBool=true;x=5;}return x;} private Thread thread=new Thread(this);private boolean threadBool=false;public Thread thread(){ if(!threadBool){threadBool=true;thread=new Thread(this);}return thread;}public ClassTest(){} public void update(int x1){
if(x() < 50){
int nextX=x() + 1 * 2 - 3 / 2 + 5;}
 else {
int last=1000;}}
 public void paint(Graphics g){
int x=3;}
 public void run(){
int x=10 + 1;}}