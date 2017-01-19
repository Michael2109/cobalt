package asm;

import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;

public class MyCode {

    public static byte[] dump() throws Exception {

        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);

        // Visit the class itself
        {
            cw.visit(V1_7,                              // Java 1.7
                    ACC_PUBLIC,                         // public class
                    "asm/MyCode",    // package and name
                    null,                               // signature (null means not generic)
                    "java/lang/Object",                 // superclass
                    new String[]{}); // interfaces
        }


// Build the constructor
        {
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "<init>",                           // method name
                    "()V",                              // descriptor
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
            mv.visitLocalVariable("this", "Lasm/MyCode;", null, lConstructor0, lConstructor2, 0);


            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello World!");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
            mv.visitLdcInsn(2);
            mv.visitVarInsn(ISTORE, 11);

            mv.visitInsn(RETURN);                      // End the constructor method
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }
        {
            /* Build 'method1' method */
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "method1",                              // name
                    "(I)V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)
            mv.visitCode();

            Label lMethod0 = new Label();
            mv.visitLabel(lMethod0);
            Label lMethod1 = new Label();
            mv.visitLabel(lMethod1);
            mv.visitLocalVariable("this", "Lasm/method1;", null, lMethod0, lMethod1, 0);
            mv.visitLocalVariable("number", "I", null, lMethod0, lMethod1, 14);


            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Other test");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
            mv.visitInsn(RETURN);                      // Return integer from top of stack
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }

        {
            /* Build 'otherMethod' method */
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "otherMethod",                              // name
                    "(I)V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)
            mv.visitCode();

            Label lMethod0 = new Label();
            mv.visitLabel(lMethod0);
            Label lMethod1 = new Label();
            mv.visitLabel(lMethod1);
            mv.visitLocalVariable("this", "Lasm/otherMethod;", null, lMethod0, lMethod1, 0);
            mv.visitLocalVariable("x1", "I", null, lMethod0, lMethod1, 17);


            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("HELLO WORLD");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
            mv.visitInsn(RETURN);                      // Return integer from top of stack
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }

        {
// Main Method
            MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
            mv.visitCode();
            Label lMethod0 = new Label();
            mv.visitLabel(lMethod0);
            Label lMethod1 = new Label();
            mv.visitLabel(lMethod1);
            mv.visitLocalVariable("this", "Lasm/main;", null, lMethod0, lMethod1, 0);
            mv.visitLocalVariable("args", "[Ljava/lang/String;", null, lMethod0, lMethod1, 0);

            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello again");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
            mv.visitTypeInsn(NEW, "asm/MyCode");
            mv.visitInsn(DUP);
            mv.visitMethodInsn(INVOKESPECIAL, "asm/MyCode", "<init>", "()V", false);
            mv.visitVarInsn(ASTORE, 21);

            mv.visitLdcInsn(5);
            mv.visitVarInsn(ISTORE, 22);

            mv.visitLdcInsn(10);
            mv.visitVarInsn(ISTORE, 23);

            mv.visitVarInsn(ALOAD, 21);
            mv.visitIntInsn(ILOAD, 22);
            mv.visitMethodInsn(INVOKEVIRTUAL, "asm/MyCode", "method1", "(I)V", false);

            mv.visitVarInsn(ALOAD, 21);
            mv.visitIntInsn(ILOAD, 22);
            mv.visitMethodInsn(INVOKEVIRTUAL, "asm/MyCode", "otherMethod", "(I)V", false);

            mv.visitInsn(RETURN);                      // Return integer from top of stack
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }

        cw.visitEnd();
        return cw.toByteArray();
    }

    public static void main(String[] args) {
        DataOutputStream dout = null;
        try {
            dout = new DataOutputStream(new FileOutputStream("build/classes/main/asm/MyCode.class"));

            dout.write(dump());
            dout.flush();
            dout.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
