import javassist.*;
import java.io.IOException;

public class JavassistTest {

    public static void main(String args[]) throws CannotCompileException, NotFoundException, IOException {

        ClassPool pool = ClassPool.getDefault();
        CtClass cc = pool.makeClass("Point");

        CtClass[] params = new CtClass[1];

        CtConstructor constructor = new CtConstructor(null, cc);
        cc.addConstructor(constructor);

        CtMethod method = CtNewMethod.make(
                "public static void main(String args[]){ System.out.println(\"Hello World\"); }",
                cc);



        cc.addMethod(CtNewMethod.make(
                "public static void main(String args[]){ System.out.println(\"Hello World\"); }",
                cc));

        cc.writeFile("build/classes/main");

        cc.detach();
    }

}
