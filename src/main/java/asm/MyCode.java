package asm;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;

public class MyCode {
    public static byte[] execute() throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_7, ACC_PUBLIC, "asm/MyCode", null, "java/lang/Object", new String[]{});

        {
            MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "(II)V", null, null);
            mv.visitCode();
// Constructor
            Label lConstructor0 = new Label();
            mv.visitLabel(lConstructor0);
            mv.visitVarInsn(ALOAD, 0);
// Load "this" onto the stack

            mv.visitMethodInsn(INVOKESPECIAL,// Invoke an instance method (non-virtual)
                    "java/lang/Object", // Class on which the method is defined
                    "<init>",// Name of the method
                    "()V",// Descriptor
                    false);// Is this class an interface?

            Label lConstructor2 = new Label();
            mv.visitLabel(lConstructor2);


            mv.visitIincInsn(1, 2);
            mv.visitInsn(RETURN);
            mv.visitLocalVariable("this", "Lasm/MyCode;", null, lConstructor0, lConstructor2, 0);
            mv.visitLocalVariable("xx", "I", null, lConstructor0, lConstructor2, 1);
            mv.visitLocalVariable("yy", "I", null, lConstructor0, lConstructor2, 2);
            // End the constructor method
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

            Label start61 = new Label();
            mv.visitLabel(start61);
            mv.visitVarInsn(ILOAD, 1);
            mv.visitLdcInsn(10);
            Label l61 = new Label();
            mv.visitJumpInsn(IF_ICMPGE, l61);

            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello World!");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
            mv.visitIincInsn(1, 1);
            mv.visitJumpInsn(GOTO, start61);
            mv.visitLabel(l61);

            mv.visitInsn(RETURN);
            Label lMethod1 = new Label();
            mv.visitLabel(lMethod1);
            mv.visitLocalVariable("this", "Lasm/method1;", null, lMethod0, lMethod1, 0);
            // Return integer from top of stack
            mv.visitLocalVariable("x", "I", null, lMethod0, lMethod1, 1);
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }

        {
// Main Method
            MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
            mv.visitCode();
            Label lMethod0 = new Label();
            mv.visitLabel(lMethod0);

            mv.visitLdcInsn(new Integer(10));
            mv.visitVarInsn(ISTORE, 65);

            mv.visitLdcInsn(new Integer(15));
            mv.visitVarInsn(ISTORE, 66);

            mv.visitTypeInsn(NEW, "asm/MyCode");
            mv.visitInsn(DUP);
            mv.visitIntInsn(ILOAD, 65);
            mv.visitIntInsn(ILOAD, 66);
            mv.visitMethodInsn(INVOKESPECIAL, "asm/MyCode", "<init>", "(II)V", false);
            mv.visitVarInsn(ASTORE, 67);

            mv.visitLdcInsn(new Integer(1));
            mv.visitVarInsn(ISTORE, 68);

            mv.visitLdcInsn(new Float(2.0));
            mv.visitVarInsn(FSTORE, 69);

            mv.visitVarInsn(ALOAD, 67);
            mv.visitIntInsn(ILOAD, 68);
            mv.visitMethodInsn(INVOKEVIRTUAL, "asm/MyCode", "method1", "(I)V", false);

            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello World");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
            mv.visitTypeInsn(NEW, "compiler/block/ifs/IfBlock");
            mv.visitInsn(DUP);
            mv.visitMethodInsn(INVOKESPECIAL, "compiler/block/ifs/IfBlock", "<init>", "()V", false);
            mv.visitVarInsn(ASTORE, 72);

            mv.visitInsn(RETURN);
            Label lMethod1 = new Label();
            mv.visitLabel(lMethod1);
            mv.visitLocalVariable("this", "Lasm/main;", null, lMethod0, lMethod1, 0);
            mv.visitLocalVariable("args", "[Ljava/lang/String;", null, lMethod0, lMethod1, 0);                // Return integer from top of stack
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

            dout.write(execute());
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
