package game;import javax.swing.JPanel;
import java.awt.Graphics;public class Panel extends JPanel implements Runnable{
private Game game;public Game game(){return game;}public void game_(Game game){ this.game=game;}public Panel(Game game){this.game=game;}}