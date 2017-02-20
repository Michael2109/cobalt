package asm;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.*;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;


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

mv.visitLdcInsn(new Integer(10));
mv.visitVarInsn(ISTORE,99);

mv.visitLdcInsn(new Long(0));
mv.visitVarInsn(LSTORE,100);

mv.visitLdcInsn(new Integer(15));
mv.visitVarInsn(ISTORE,101);

mv.visitTypeInsn(NEW, "MyCode");
mv.visitInsn(DUP);
mv.visitIntInsn(ILOAD, 99);mv.visitIntInsn(ILOAD, 101);mv.visitMethodInsn(INVOKESPECIAL, "MyCode", "<init>", "(II)V", false);
mv.visitVarInsn(ASTORE,102);

mv.visitLdcInsn(new Integer(1));
mv.visitVarInsn(ISTORE,103);

mv.visitLdcInsn(new Float(2.0));
mv.visitVarInsn(FSTORE,104);

mv.visitVarInsn(ALOAD, 102);
mv.visitIntInsn(ILOAD, 103);mv.visitMethodInsn(INVOKEVIRTUAL, "/MyCode", "method1", "(I)V", false);

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
   new File(new File("cobalt_build/asm/ObjectTest.class").getParent()).mkdirs();  DataOutputStream dout = null;
        try {
            dout = new DataOutputStream(new FileOutputStream("cobalt_build/asm/ObjectTest.class"));

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
