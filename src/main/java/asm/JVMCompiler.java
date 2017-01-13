package asm;

import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;

import java.io.DataOutputStream;
import java.io.FileOutputStream;

public class JVMCompiler {

    public static byte[] dump() throws Exception {

        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES);

        // Visit the class itself
        {
            cw.visit(V1_7,                              // Java 1.7
                    ACC_PUBLIC,                         // public class
                    "asm/Test",    // package and name
                    null,                               // signature (null means not generic)
                    "java/lang/Object",                 // superclass
                    new String[]{}); // interfaces
        }

        // Build the constructor
        {
            MethodVisitor con = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "<init>",                           // method name
                    "()V",                              // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)

            con.visitCode();                            // Start the code for this method
            con.visitVarInsn(ALOAD, 0);                 // Load "this" onto the stack

            con.visitMethodInsn(INVOKESPECIAL,          // Invoke an instance method (non-virtual)
                    "java/lang/Object",                 // Class on which the method is defined
                    "<init>",                           // Name of the method
                    "()V",                              // Descriptor
                    false);                             // Is this class an interface?

            con.visitInsn(RETURN);                      // End the constructor method
            con.visitMaxs(1, 1);                        // Specify max stack and local vars
        }

        {
            /* Build 'add' method */
            MethodVisitor mv = cw.visitMethod(
                    ACC_PUBLIC,                         // public method
                    "add",                              // name
                    "()V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)

            mv.visitCode();



            mv.visitLdcInsn(5);
            mv.visitVarInsn(ISTORE, 1);

            mv.visitLdcInsn(10);
            mv.visitVarInsn(ISTORE, 2);

            mv.visitLdcInsn(15);

            mv.visitVarInsn(ILOAD, 1);
            mv.visitVarInsn(ILOAD, 2);

        //    mv.visitVarInsn(IF_ICMPLE, );

            Label label = new Label();
            mv.visitLabel(label);












            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitLdcInsn("Hello World");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");

         //   mv.visitIntInsn(SIPUSH, 400);
       //     mv.visitVarInsn(ISTORE,4);

      //      mv.visitIntInsn(SIPUSH, 600);
       //     mv.visitVarInsn(ISTORE,5);

           // mv.visitLocalVariable("x", "I", null, l0, l1,1);
         //   mv.visitLocalVariable("y", "I", null, l1, l2,2);
          //  mv.visitLocalVariable("z", "I", null, l2, l3,3);
         //   mv.visitVarInsn(ISTORE, 3);
           // mv.visitLocalVariable("x", "I", null, l1, l2, 1);
            mv.visitInsn(RETURN);                      // Return integer from top of stack
          //  mv.visitMaxs(3, 4);                         // Specify max stack and local vars
        }

       // DynamicClassLoader loader = new DynamicClassLoader();
       // Class<?> clazz = loader.defineClass("asm.Test", cw.toByteArray());
    //    System.out.println(clazz.getName());
      //  GeneratedInterface calc = (GeneratedInterface)clazz.newInstance();

      //  calc.add();
       // System.out.println("2 + 2 = ");

        return cw.toByteArray();
    }

    public static void main(String [] args) throws Exception{
        DataOutputStream dout=new DataOutputStream(new FileOutputStream("build/classes/main/asm/Test.class"));
        dout.write(dump());
        dout.flush();
        dout.close();
    }

}