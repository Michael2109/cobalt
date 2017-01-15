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
 Label l0 = new Label();
mv.visitLabel(l0);
            mv.visitVarInsn(ALOAD, 0);                 // Load "this" onto the stack

            mv.visitMethodInsn(INVOKESPECIAL,          // Invoke an instance method (non-virtual)
                    "java/lang/Object",                 // Class on which the method is defined
                    "<init>",                           // Name of the method
                    "()V",                              // Descriptor
                    false);                             // Is this class an interface?

Label l2 = new Label();
mv.visitLabel(l2);
mv.visitLocalVariable("this", "Lasm/GeneratedAsmCode;", null, l0, l2, 0);
mv.visitLocalVariable("x", "I", null, l0, l2, 2);
mv.visitLocalVariable("y", "I", null, l0, l2, 3);

       
     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello World!");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitLdcInsn(2);
mv.visitVarInsn(ISTORE,5);

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
mv.visitVarInsn(ISTORE,9);

Label start10 = new Label();
mv.visitLabel(start10);
mv.visitVarInsn(ILOAD,9);
mv.visitLdcInsn(20);
Label l10 = new Label();
mv.visitJumpInsn(IF_ICMPGE, l10);

     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Something");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitJumpInsn(GOTO, start10);
mv.visitLabel(l10);

mv.visitVarInsn(ILOAD,9);mv.visitLdcInsn(10);
Label l12 = new Label();
mv.visitJumpInsn(IF_ICMPGE, l12);

     mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Something else");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitLabel(l12);
mv.visitTypeInsn(NEW, "asm/TestCode");
mv.visitInsn(DUP);
mv.visitMethodInsn(INVOKESPECIAL, "asm/TestCode", "<init>", "()V", false);
mv.visitVarInsn(ASTORE,14);

mv.visitVarInsn(ALOAD, 14);
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
