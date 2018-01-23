public class TestModule {
    public static void main(String[] args) {
        TestModule testmodule = new TestModule();
        int x = 3;
        int y = 10;
        int z = 15;
        int[] arrTest = {x, y, z};
        System.out.println(arrTest);
        testmodule.testFunction(x);
        String stringTest = "Helloworld";
    }

    public void testFunction(int x) {
        if (x < 50) {
            System.out.println(x);
            int nextX = x + 1 * 2 - 3 / 2 + 5;
            testFunction(nextX);
        } else {
            int last = 1000;
            System.out.println(last);
        }
    }
}
