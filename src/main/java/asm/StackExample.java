package asm;

import java.util.Stack;

public class StackExample {

    public static void main(String[] args) {

        Stack<Integer> stack = new Stack<>();

        int x = 5;
        stack.push(x);

        int y = 10;
        stack.push(y);

        int z = 15;
        stack.push(z);



        System.out.println(stack.peek());

    }

}
