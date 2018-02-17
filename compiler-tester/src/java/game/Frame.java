package game;import javax.swing.JFrame;public final class Frame extends JFrame {
public Frame(){
add(new Panel());
setTitle("Game");
setResizable(true);
pack();
setSize(800, 600);
setVisible(true);}}