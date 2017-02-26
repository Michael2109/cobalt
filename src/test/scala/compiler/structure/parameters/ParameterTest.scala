/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Michael Haywood
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

package compiler.structure.parameters

// Represents what is input for ASM. E.g. int = 'I', String = 'Ljava/lang/String;'
class ParameterTest(var `type`: String, var name: String) {


  // Name of the variable
  def getName: String = name

  // ASM type. int = "I", String = "java.lang.String", etc
  def getAsmType: String = {
    if (getType == "int") "I"

    else if (getType == "String") "Ljava/lang/String;"

    else null
  }

  // Type of the variable
  def getType: String = `type`

  def setType(`type`: String) {
    this.`type` = `type`
  }

  override def toString: String = {
    return name + " : " + `type`
  }

}
