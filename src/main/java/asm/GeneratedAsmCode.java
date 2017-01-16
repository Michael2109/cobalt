package asm;

import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;

public class GeneratedAsmCode {

    public static byte[] dump() throws Exception {

        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES); 

    // Visit the class itself
        {
            cw.visit(V1_7,                              // Java 1.7
                    ACC_PUBLIC,                         // public class
                    "asm/GeneratedAsmCode",    // package and name
                    null,                               // signature (null means not generic)
                    "java/lang/Object",                 // superclass
                    new String[]{}); // interfaces
        }


// Build the constructor
        {
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "<init>",                           // method name
                    "(II)V",                              // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)

            mv.visitCode();                            // Start the code for this method
 Label lConstructor0 = new Label();
mv.visitLabel(lConstructor0);
            mv.visitVarInsn(ALOAD, 0);                 // Load "this" onto the stack

            mv.visitMethodInsn(INVOKESPECIAL,          // Invoke an instance method (non-virtual)
                    "java/lang/Object",                 // Class on which the method is defined
                    "<init>",                           // Name of the method
                    "()V",                              // Descriptor
                    false);                             // Is this class an interface?

Label lConstructor2 = new Label();
mv.visitLabel(lConstructor2);
mv.visitLocalVariable("this", "Lasm/GeneratedAsmCode;", null, lConstructor0, lConstructor2, 6);
mv.visitLocalVariable("x", "I", null, lConstructor0, lConstructor2, 9);
mv.visitLocalVariable("y", "I", null, lConstructor0, lConstructor2, 10);

       
     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello World!");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitLdcInsn(2);
mv.visitVarInsn(ISTORE,12);

     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Test");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitInsn(RETURN);                      // End the constructor method
}
   {
            /* Build 'add' method */
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "method1",                              // name
                    "()V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)



     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Other test");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
       mv.visitInsn(RETURN);                      // Return integer from top of stack
        }
   {
            /* Build 'add' method */
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "otherMethod",                              // name
                    "()V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)



mv.visitLdcInsn(10);
mv.visitVarInsn(ISTORE,17);

Label start18 = new Label();
mv.visitLabel(start18);
mv.visitVarInsn(ILOAD,17);
mv.visitLdcInsn(20);
Label l18 = new Label();
mv.visitJumpInsn(IF_ICMPGE, l18);

     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Something");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitJumpInsn(GOTO, start18);
mv.visitLabel(l18);

mv.visitVarInsn(ILOAD,17);mv.visitLdcInsn(10);
Label l20 = new Label();
mv.visitJumpInsn(IF_ICMPGE, l20);

     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Something else");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitLabel(l20);
mv.visitTypeInsn(NEW, "asm/TestCode");
mv.visitInsn(DUP);
mv.visitMethodInsn(INVOKESPECIAL, "asm/TestCode", "<init>", "()V", false);
mv.visitVarInsn(ASTORE,22);

mv.visitVarInsn(ALOAD, 22);
mv.visitMethodInsn(INVOKEVIRTUAL, "asm/test", "add", "()V", false);

       mv.visitInsn(RETURN);                      // Return integer from top of stack
        }

return cw.toByteArray();}
    public static void main(String [] args){
     DataOutputStream dout = null;
        try {
            dout = new DataOutputStream(new FileOutputStream("build/classes/main/asm/GeneratedAsmCode.class"));

        dout.write(dump());
        dout.flush();
        dout.close();
        } catch (FileNotFoundException e) {
        e.printStackTrace();
    } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
           } }
}
