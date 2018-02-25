package game;

import javax.swing.JPanel;
import java.awt.Graphics;

public final class Panel extends JPanel implements Runnable {
    public Thread thread = new Thread(this);
    boolean threadBool = false;

    public Thread thread() {
        if (!threadBool) {
            threadBool = true;
            return new Thread(this);
        } else {
            return thread;
        }
    }

    public int x = 50;
    boolean xBool = false;

    public int x() {
        if (!xBool) {
            xBool = true;
            return 50;
        } else {
            return x;
        }
    }

    public int y = 100;
    boolean yBool = false;

    public int y() {
        if (!yBool) {
            yBool = true;
            return 100;
        } else {
            return y;
        }
    }

    public int dx = 1;
    boolean dxBool = false;

    public int dx() {
        if (!dxBool) {
            dxBool = true;
            return 1;
        } else {
            return dx;
        }
    }

    public int dy = 1;
    boolean dyBool = false;

    public int dy() {
        if (!dyBool) {
            dyBool = true;
            return 1;
        } else {
            return dy;
        }
    }

    private boolean alive = true;
    boolean aliveBool = false;

    private boolean alive() {
        if (!aliveBool) {
            aliveBool = true;
            return true;
        } else {
            return alive;
        }
    }

    private final Game game;

    public Panel(Game game) {
        this.game = game;
        thread.start();
    }

    public void update() {
        x = x + dx;
        y = y + dy;
        if (x < 0 || x > 750) {
            dx = dx * -1;
        }
        if (y < 0 || y > 550) {
            dy = dy * -1;
        }
    }

    public void paint(Graphics g) {
        super.paintComponent(g);
        g.drawRect(x, y, 50, 50);
    }

    @Override
    public void run() {
        try {
            while (true) {
                Thread.sleep(7);
                update();
                repaint();
            }
        } catch (final Exception e) {
            System.out.println("Error: Thread failed");
        }
    }
}