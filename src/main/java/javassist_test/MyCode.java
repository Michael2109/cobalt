package javassist_test;

public class MyCode {
    public void method1() {

        int x = 2;
        double y = 0.0;
        char letter = 'a';
        System.out.println("test");
        System.out.println("other");
        while (x < 10) {
            System.out.println("Do something");
            System.out.println(x);
            x += 1;
        }
        System.out.println("Say something");
        boolean booleanTest = false;
        if (x > 5) {
            float test = 0.2f;
            double fdsfs = 20.0;
        }
        boolean something = true;
        if (something) {
            System.out.println("something is true");
        }
        System.out.println("Method 1 called");
    }

    public void method2() {

        float test = 0.1f;
        char letter2 = 'b';
        System.out.println("Test");
    }

    public void method3() {

        boolean name = true;
        System.out.println("Method 3 stuffs");
    }

    public void method4() {

        double x = 0.1;
    }

    public static void main(String args[]) {

        double y = 0.5;
        System.out.println("Working!!!");
        MyCode name = new MyCode();
        name.method1();
    }
}
