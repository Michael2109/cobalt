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

package compiler.symbol_table

import compiler.exceptions.UndefinedVarException
import compiler.structure.blocks.Block

import scala.collection.mutable.ListBuffer

object SymbolTable {
  private val SYMBOL_TABLE: SymbolTable = new SymbolTable

  def getInstance: SymbolTable = {
    SYMBOL_TABLE
  }
}

class SymbolTable() {
  var rows: ListBuffer[Row] = new ListBuffer[Row]

  def addRow(row: Row): ListBuffer[Row] = rows += row

  def exists(name: String, methodName: String, className: String): Boolean = {
    if (name == null) {
      return false
    }

    for (r <- rows) {
      if (r.getName == null || r.getMethodName == null || r.getClassName == null) {

      } else {
        if (r.getName == name && r.getMethodName == methodName && r.getClassName == className) {
          return true
        }
      }
    }
    // Check ifs variables declared in class scope exist
    for (r <- rows) {
      if (methodName == null && r.getMethodName == null && name != null && r.getName != null) {
        if (name == r.getName) {
          return true
        }
      }
    }
    false
  }

  def getType(block: Block): String = {
    block.getType
  }

  def printSymbols() {

    printf("%-1s %-5s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s \n", "|", "ID", "|", "Name", "|", "Type", "|", "Value", "|", "Method", "|", "Class")
    printf("%-1s %-5s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s \n", "+", "----", "+", "----", "+", "----", "+", "----", "+", "----", "+", "----")

    for (row <- rows) {
      printf("%-1s %-5s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s \n", " ", row.getId, " ", row.getName, " ", row.getType, " ", row.getValue, " ", row.getMethodName, " ", row.getClassName, " ")
    }

  }

  def getValue(method: Block, variableName: String): Row = {

    if (method == null) {
      for (row <- rows) {
        if (row.getMethodName == null && row.getName == variableName)
          return row
      }
    }

    for (row <- rows) {
      if (row.getMethodName != null && row.getMethodName == method.getName && row.getName != null && row.getName == variableName) {
        return row
      }
    }
    throw new UndefinedVarException("'" + variableName + "' doesn't exist")
  }
}