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

import compiler.ast.Block
import compiler.exceptions.UndefinedVarException
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object SymbolTable {
  private val SYMBOL_TABLE: SymbolTable = new SymbolTable

  def getInstance: SymbolTable = {
    SYMBOL_TABLE
  }
}

class SymbolTable() {

  val logger = LoggerFactory.getLogger(getClass)

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

  @throws(classOf[UndefinedVarException])
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
    printSymbols()
    throw new UndefinedVarException("'" + variableName + "' doesn't exist")

  }

  def printSymbols() {

    logger.debug(String.format("%-1s %-5s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s", "|", "ID", "|", "Name", "|", "Type", "|", "Value", "|", "Method", "|", "Class"))
    logger.debug(String.format("%-1s %-5s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s", "+", "----", "+", "----", "+", "----", "+", "----", "+", "----", "+", "----"))

    for (row <- rows) {
     logger.debug(String.format("%-1s %-5s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s", " ", ""+row.getId, " ", row.getName, " ", row.getType, " ", row.getValue, " ", row.getMethodName, " ", row.getClassName, " "))
    }

  }
}