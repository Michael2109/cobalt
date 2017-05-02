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

package cobalt.ast.ifs

import cobalt.ast.Block
import cobalt.ast.conditionals.ConditionalBlock
import cobalt.tokenizer.Tokenizer
import cobalt.utilities.{RPN, Utils}

/**
  * Represents an if statement
  *
  * @param superBlockInit The parent block
  */
class IfBlock(var superBlockInit: Block, tokenizer: Tokenizer) extends Block(superBlockInit, true, false) {

  val orderedStatementBlocks = getOrderedStatements(tokenizer)

  def getName: String = ""

  def getValue: String = ""

  def getType: String = "<IF_STATEMENT>"

  def getOpeningCode: String = {
    val values = orderedStatementBlocks.filter(!_.isInstanceOf[ConditionalBlock]).map(_.getOpeningCode).mkString("")
    values +
      "Label " + "l" + id + " = new Label();\n" +
      "mv.visitJumpInsn(" + orderedStatementBlocks.filter(_.isInstanceOf[ConditionalBlock]).head.getOpeningCode + "," +  "l" + id + ");\n"
  }

  def getClosingCode: String = {
    "mv.visitLabel(" + "l" + id + ");\n"
  }

  override def toString: String = getType + stack

  private def getOrderedStatements(tokenizer: Tokenizer): List[Block] = {

    var statementString = ""

    // Extract information from within the parenthesis
    while (tokenizer.peek.token != ")") {
      val nextToken = tokenizer.nextToken.token
      var found = false
      statementString += nextToken
      found = true
    }
    RPN.infixToRPN(Utils.getAllBlocks(this, statementString))

  }


}