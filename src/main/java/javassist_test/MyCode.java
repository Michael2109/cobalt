package javassist_test;
import javassist.*;
import java.io.IOException;

public class MyCode {

    public static void main(String args[]) throws CannotCompileException, NotFoundException, IOException {

        ClassPool pool = ClassPool.getDefault();
        CtClass cc = pool.makeClass("MyCode");

cc.addMethod(CtNewMethod.make("public static void method1(){ "+

"int x = 2;"+
"double y = 0.0;"+
"char letter = 'a';"+
"System.out.println(\"test\");"+
"System.out.println(\"other\");"+
"System.out.println(x);"+
"char letter2 = 'a';"+
"System.out.println(\"Say something\");"+
"if(true){"+
"float test = 0.2;"+
"double fdsfs = 20.0;"+
"}"+
"}", cc));
cc.addMethod(CtNewMethod.make("public static void method2(){ "+

"float test = 0.1;"+
"char letter2 = 'b';"+
"System.out.println(\"Test\");"+
"}", cc));
cc.addMethod(CtNewMethod.make("public static void method3(){ "+

"boolean name = true;"+
"System.out.println(\"Method 3 stuffs\");"+
"}", cc));
cc.addMethod(CtNewMethod.make("public static void method4(){ "+

"double x = 0.1;"+
"}", cc));

 cc.addMethod(CtNewMethod.make(
                        "public static void main(String args[]){ "+

"double y = 0.5;"+
"System.out.println(\"Working!!!\");"+
"method2();"+
"method1();"+
"method3();"+
"}", cc));
    cc.writeFile("build/classes/main");

        cc.detach();}}
