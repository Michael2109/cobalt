import java.io.DataOutputStream;
import java.io.FileOutputStream;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;
public class Test { 

public static byte[] dump() throws Exception {
 //ClassWriter is a class visitor that generates the code for the class
        ClassWriter cw = new ClassWriter(0);
        FieldVisitor fv;
        MethodVisitor mv;
        //Start creating the class. 
        cw.visit(V1_6, ACC_PUBLIC + ACC_SUPER, "Test", null, "java/lang/Object", null);

 //Implementing the constructor
            mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
 //getVersion Method
            mv = cw.visitMethod(ACC_PUBLIC, "method1", "()I", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "Test", "version", "I");
            mv.visitInsn(IRETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
 //getVersion Method
            mv = cw.visitMethod(ACC_PUBLIC, "method2", "()I", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "Test", "version", "I");
            mv.visitInsn(IRETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
 //getVersion Method
            mv = cw.visitMethod(ACC_PUBLIC, "method3", "()I", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "Test", "version", "I");
            mv.visitInsn(IRETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
   cw.visitEnd();

 

        return cw.toByteArray();

    }

    public static void main(String [] args) throws Exception{
        DataOutputStream dout=new DataOutputStream(new FileOutputStream("asm/Test.class"));
        dout.write(dump());
        dout.flush();
        dout.close();
    }
}
