package asm;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.*;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;


public class StringTest{
    public static byte[] execute() throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_7, ACC_PUBLIC, "asm/StringTest", null, "java/lang/Object", new String[]{});


        {
            MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V" ,null, null);
            mv.visitCode();
// Constructor
            Label lConstructor0 = new Label();
            mv.visitLabel(lConstructor0);
            mv.visitVarInsn(ALOAD,0);
// Load "this" onto the stack

            mv.visitMethodInsn(INVOKESPECIAL,// Invoke an instance method (non-virtual)
                    "java/lang/Object", // Class on which the method is defined
                    "<init>",// Name of the method
                    "()V",// Descriptor
                    false);// Is this class an interface?

            Label lConstructor2 = new Label();
            mv.visitLabel(lConstructor2);


            mv.visitInsn(RETURN);                     mv.visitLocalVariable("this", "Lasm/StringTest;", null, lConstructor0, lConstructor2, 0);
            // End the constructor method
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }
        {
            /* Build 'test' method */
            MethodVisitor mv = cw.visitMethod(
                    0 ,                         // public method
                    "test",                              // name
                    "()V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)
            mv.visitCode();

            Label lMethod0 = new Label();
            mv.visitLabel(lMethod0);

            mv.visitLdcInsn(new Integer(5));
            mv.visitVarInsn(ISTORE,121);

            mv.visitVarInsn(ASTORE,122);

            mv.visitInsn(RETURN);
            Label lMethod1 = new Label();
            mv.visitLabel(lMethod1);
            mv.visitLocalVariable("this", "Lasm/test;", null, lMethod0, lMethod1, 0);
            // Return integer from top of stack
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }

        cw.visitEnd();
        return cw.toByteArray();

    }

    public static void main(String [] args){
        new File(new File("cobalt_build/asm/StringTest.class").getParent()).mkdirs();  DataOutputStream dout = null;
        try {
            dout = new DataOutputStream(new FileOutputStream("cobalt_build/asm/StringTest.class"));

            dout.write(execute());
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
