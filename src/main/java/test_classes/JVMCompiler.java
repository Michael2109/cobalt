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

package test_classes;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

import java.io.DataOutputStream;
import java.io.FileOutputStream;

import static org.objectweb.asm.Opcodes.*;

/**
 * Test ASM code
 */
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
                    "(I)V",                            // descriptor
                    null,                               // signature (null means not generic)
                    null);                              // exceptions (array of strings)

            mv.visitCode();

            Label lMethod0 = new Label();
            mv.visitLabel(lMethod0);

            //   mv.visitLdcInsn();

            mv.visitInsn(RETURN);                      // Return integer from top of stack
            Label lmethod2 = new Label();
            mv.visitLabel(lmethod2);
            mv.visitLocalVariable("this", "Lasm/TestCode;", null, lMethod0, lmethod2, 0);
            mv.visitLocalVariable("x", "I", null, lMethod0, lmethod2, 1);
            mv.visitMaxs(0, 0);
            mv.visitEnd();
        }

        cw.visitEnd();

        // DynamicClassLoader loader = new DynamicClassLoader();
        // Class<?> clazz = loader.defineClass("asm.Test", cw.toByteArray());
        //    System.out.println(clazz.getName());
        //  GeneratedInterface calc = (GeneratedInterface)clazz.newInstance();

        //  calc.add();
        // System.out.println("2 + 2 = ");

        return cw.toByteArray();
    }

    public static void main(String[] args) throws Exception {
        DataOutputStream dout = new DataOutputStream(new FileOutputStream("build/classes/main/asm/Test.class"));
        dout.write(dump());
        dout.flush();
        dout.close();
    }

}