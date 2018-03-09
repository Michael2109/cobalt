package test_files;import javax.swing.JPanel;
import java.awt.Graphics;public class ClassTest extends JPanel implements Runnable{
private int x;private boolean xBool=false;public int x(){ if(!xBool){xBool=true;x=5;}return x;} private Thread thread;private boolean threadBool=false;public Thread thread(){ if(!threadBool){threadBool=true;thread=new Thread(this);}return thread;}public ClassTest(){}}