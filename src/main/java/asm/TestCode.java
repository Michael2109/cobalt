package asm;

/**
 * Used to view ASM code
 */
public class TestCode {

    public static void main(String[] args) {
        new TestCode().add(new Integer(10));
    }

    public void add(int x) {
        x += 5;
    }

}
