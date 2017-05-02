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

package cobalt.ast.structures

import java.io.File

import cobalt.ast.Block
import cobalt.ast.packages.PackageBlock
import cobalt.utilities.Utils

/**
  * Represents the whole file.
  * The base parent class for the AST
  */
class FileBlock(name: String, buildDir: File) extends Block(null, true, false) {

  override def getName: String = name

  override def getValue: String = ""

  override def getType: String = "file"

  override def getOpeningCode: String = {
    "package " + Utils.packageBlock(this).directory.replace("/", ".") + ";\n" +
      "import " + "java.io.DataOutputStream" + ";\n"+
      "import " + "java.io.FileNotFoundException" + ";\n"+
      "import " + "java.io.FileOutputStream" + ";\n"+
      "import " + "java.io.IOException" + ";\n"+
      "import " + "java.io.*" + ";\n"+
      "import " + "org.objectweb.asm.*" + ";\n"+
      "import static " + "org.objectweb.asm.Opcodes.*" + ";\n"
  }

  override def getClosingCode: String = {
    val packageBlock: PackageBlock = {

        var result = new PackageBlock("")
        for (sub <- subBlocks)
          sub match {
            case result1: PackageBlock => result = result1
            case _ =>
          }
        result
    }

    "public static void main(String [] args){\n" +
      "try {\n" +
      "File file = new File(\"" + buildDir.getPath.replace("\\", "/") +"\");\n" +
      "new File(file.getParent()).mkdirs();\n" +
      "file.createNewFile();\n" +
      "  DataOutputStream dout = null;\n" +
      "        try {\n" +
      "" + "            dout = new DataOutputStream(new FileOutputStream(file));\n" + "\n" + "        dout.write(execute());\n" + "        dout.flush();\n" + "        dout.close();\n" + "        } catch (FileNotFoundException e) {\n" + "        e.printStackTrace();\n" + "    } catch (IOException e) {\n" + "            e.printStackTrace();\n" + "        } catch (Exception e) {\n" + "            e.printStackTrace();\n" + "        " + "   } }\n" +
      "catch(IOException e){\n" +
      "e.printStackTrace();" +
      "}\n" +
      "}\n" +
      "}\n"
  }

  override def toString: String = "file: " + name
}