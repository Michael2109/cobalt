package asm;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

import java.io.*;

import static org.objectweb.asm.Opcodes.*;


public class ObjectTest{
public static byte[] execute() throws Exception {
ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
cw.visit(V1_7, ACC_PUBLIC, "asm/ObjectTest", null, "java/lang/Object", new String[]{});

{
// Main Method
MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
mv.visitCode();
Label lMethod0 = new Label();
mv.visitLabel(lMethod0);

    mv.visitLdcInsn("TEST");
    mv.visitVarInsn(ASTORE, 97);

mv.visitLdcInsn(new Integer(10));
    mv.visitVarInsn(ISTORE, 98);

mv.visitLdcInsn(new Long(0));
    mv.visitVarInsn(LSTORE, 99);

mv.visitLdcInsn(new Integer(15));
    mv.visitVarInsn(ISTORE, 100);

    mv.visitTypeInsn(NEW, "asm/MyCode");
mv.visitInsn(DUP);
    mv.visitIntInsn(ILOAD, 100);
    mv.visitMethodInsn(INVOKESPECIAL, "asm/MyCode", "<init>", "(I)V", false);
    mv.visitVarInsn(ASTORE, 101);

mv.visitLdcInsn(new Integer(1));
    mv.visitVarInsn(ISTORE, 102);

mv.visitLdcInsn(new Float(2.0));
    mv.visitVarInsn(FSTORE, 103);

    mv.visitVarInsn(ALOAD, 101);
    mv.visitIntInsn(ALOAD, 97);
    mv.visitMethodInsn(INVOKEVIRTUAL, "asm/MyCode", "method1", "(Ljava/lang/String;)V", false);

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

    public static void main(String [] args){
        new File(new File("build/classes/main/asm/ObjectTest.class").getParent()).mkdirs();
        DataOutputStream dout = null;
        try {
            dout = new DataOutputStream(new FileOutputStream("build/classes/main/asm/ObjectTest.class"));

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
