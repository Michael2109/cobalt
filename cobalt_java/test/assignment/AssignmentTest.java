/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Cobalt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.assignment;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.*;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.*;


public class AssignmentTest{
public static byte[] execute() throws Exception {
ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
cw.visit(V1_7, +ACC_PRIVATE, "test/assignment/AssignmentTest", null, "java/lang/Object", new String[]{});


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


mv.visitInsn(RETURN);                     mv.visitLocalVariable("this", "Ltest/assignment/AssignmentTest;", null, lConstructor0, lConstructor2, 0);
 // End the constructor method
mv.visitMaxs(0, 0);
mv.visitEnd();
}
{
// Main Method
MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
mv.visitCode();
Label lMethod0 = new Label();
mv.visitLabel(lMethod0);

mv.visitTypeInsn(NEW, "java/lang/Byte");
mv.visitInsn(DUP);
mv.visitLdcInsn(new Byte("10"));

mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Byte", "<init>", "(B)V", false);
mv.visitVarInsn(ASTORE,59);

mv.visitTypeInsn(NEW, "java/lang/Short");
mv.visitInsn(DUP);
mv.visitLdcInsn(new Short("5"));

mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Short", "<init>", "(S)V", false);
mv.visitVarInsn(ASTORE,61);

mv.visitTypeInsn(NEW, "java/lang/Integer");
mv.visitInsn(DUP);
mv.visitIntInsn(BIPUSH, 5);
mv.visitIntInsn(BIPUSH, 10);
mv.visitInsn(IADD);
mv.visitIntInsn(BIPUSH, 17);
mv.visitIntInsn(BIPUSH, 3);
mv.visitInsn(IMUL);
mv.visitInsn(ISUB);
mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V", false);
mv.visitVarInsn(ASTORE,63);

mv.visitTypeInsn(NEW, "java/lang/Long");
mv.visitInsn(DUP);
mv.visitLdcInsn(new Long(50000L));

mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Long", "<init>", "(J)V", false);
mv.visitVarInsn(ASTORE,73);

mv.visitTypeInsn(NEW, "java/lang/Float");
mv.visitInsn(DUP);
mv.visitLdcInsn(new Float(5.2F));

mv.visitLdcInsn(new Float(7.4F));

mv.visitInsn(FADD);
mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Float", "<init>", "(F)V", false);
mv.visitVarInsn(ASTORE,75);

mv.visitTypeInsn(NEW, "java/lang/Double");
mv.visitInsn(DUP);
mv.visitLdcInsn(new Double(5.0));

mv.visitLdcInsn(new Double(3.0));

mv.visitInsn(DMUL);
mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Double", "<init>", "(D)V", false);
mv.visitVarInsn(ASTORE,79);

mv.visitTypeInsn(NEW, "java/lang/Character");
mv.visitInsn(DUP);
mv.visitLdcInsn(new Character('a'));

mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Character", "<init>", "(C)V", false);
mv.visitVarInsn(ASTORE,83);

mv.visitLdcInsn("hiiiiiiiiiiiiiiiiiiii");

mv.visitVarInsn(ASTORE,85);

mv.visitTypeInsn(NEW, "java/lang/Integer");
mv.visitInsn(DUP);
mv.visitIntInsn(BIPUSH, 25);mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V", false);

mv.visitVarInsn(ASTORE,87);

mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,87);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,59);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Byte", "byteValue", "()B", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,61);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,63);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,73);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(J)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,75);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(F)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,79);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(D)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,83);
mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(C)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,85);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitVarInsn(ALOAD,87);

mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V");
mv.visitInsn(RETURN);     
Label lMethod1 = new Label();
mv.visitLabel(lMethod1);
mv.visitLocalVariable("this", "Ltest/assignment/main;", null, lMethod0, lMethod1, 0);
mv.visitLocalVariable("args", "[Ljava/lang/String;", null, lMethod0, lMethod1, 0);                // Return integer from top of stack
  mv.visitMaxs(0, 0);
mv.visitEnd();
}

   {
            /* Build 'sayHello' method */
            MethodVisitor mv = cw.visitMethod(
                    +ACC_PRIVATE  +ACC_FINAL,                         // public method
                    "sayHello",                              // name
                    "()V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)
mv.visitCode();

Label lMethod0 = new Label();
mv.visitLabel(lMethod0);

mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
mv.visitLdcInsn("sup");
mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
mv.visitInsn(RETURN);     
Label lMethod1 = new Label();
mv.visitLabel(lMethod1);
mv.visitLocalVariable("this", "Ltest/assignment/sayHello;", null, lMethod0, lMethod1, 0);
// Return integer from top of stack
mv.visitMaxs(0, 0);
mv.visitEnd();
}

cw.visitEnd();
return cw.toByteArray();
}
public static void main(String [] args){
try {
File file = new File("cobalt_generated/test/assignment/AssignmentTest.class");
new File(file.getParent()).mkdirs();file.createNewFile();
  DataOutputStream dout = null;
        try {
            dout = new DataOutputStream(new FileOutputStream("cobalt_generated/test/assignment/AssignmentTest.class"));

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
catch(IOException e){
e.printStackTrace();}
}
}

