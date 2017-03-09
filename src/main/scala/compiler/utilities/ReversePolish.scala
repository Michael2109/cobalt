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

package compiler.utilities

import compiler.structure.blocks.Block
import compiler.structure.blocks.operators._
import compiler.structure.blocks.variable.DefineVariableBlock

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Contains methods to convert to and from reverse polish notation
  */
object ReversePolish {

  val LEFT_ASSOC = 0
  val RIGHT_ASSOC = 1

  def main(args: Array[String]): Unit = {

    println(isOperator(new AddOpBlock(null)))
    println(isOperator(new DefineVariableBlock(null, false, "other", "something")))

    val blocks = Utils.getBlocks(null, "val x:int = 5 + 3 - 2 * 4/3").expressions.toList.drop(1)
    println(blocks)
    println(infixToRPN(blocks))
  }

  def OPERATORS(b: Block) = b match {

    case b: AddOpBlock => List(0, LEFT_ASSOC)

    case b: SubtractOpBlock => List(0, LEFT_ASSOC)

    case b: MultiplyOpBlock => List(5, LEFT_ASSOC)

    case b: DivideOpBlock => List(5, LEFT_ASSOC)

    case b: ModulusOpBlock => List(5, LEFT_ASSOC)

    case b: PowerOfOpBlock => List(10, RIGHT_ASSOC)

    case _ => List()

  }

  /**
    * Compare precendece of two operators.
    *
    * @param token1 The first operator .
    * @param token2 The second operator .
    * @return A negative number if token1 has a smaller precedence than token2,
    *         0 if the precendences of the two tokens are equal, a positive number
    *         otherwise.
    */
  def cmpPrecedence(token1: Block, token2: Block): Int = {
    if (!isOperator(token1) || !isOperator(token2)) {
      throw new IllegalArgumentException("Invalid tokens: " + token1
        + " " + token2);
    }
    OPERATORS(token1)(0) - OPERATORS(token2)(0);
  }

  def infixToRPN(inputTokens: List[Block]): List[Block] = {
    val out = new ListBuffer[Block]()
    val stack: mutable.Stack[Block] = new mutable.Stack[Block]()
    // For all the input tokens [S1] read the next token [S2]
    for (token <- inputTokens) {
      if (isOperator(token)) {
        // If token is an operator (x) [S3]
        var found = true
        while (!stack.isEmpty && isOperator(stack.head) && found) {

          // [S4]
          if ((isAssociative(token, LEFT_ASSOC) && cmpPrecedence(
            token,
            stack.head) <= 0) ||
            (isAssociative(token, RIGHT_ASSOC) && cmpPrecedence(
              token,
              stack.head) < 0)) {
            // [S5] [S6]
            out += stack.pop()
            //continue
          } else {
            found = false

          }
          //break
        }
        // Push the new operator on the stack [S7]
        stack.push(token)
      } else if (token.isInstanceOf[OpeningBracketOpBlock]) {
        // [S8]
        stack.push(token)
      } else if (token.isInstanceOf[ClosingBracketOpBlock]) {
        // [S9]
        while (!stack.isEmpty && !stack.head.isInstanceOf[OpeningBracketOpBlock]) // [S10]
          out += stack.pop()
        // [S11]
        stack.pop()
      }
      else {
        // [S12]
        out += token
      }
    }
    while (!stack.isEmpty) // [S13]
      out += stack.pop()
    //val output: Array[Block] = Array.ofDim[Block](out.size)
    out.toList
  }


  /**
    * find whether an operator
    *
    * @param block
    * @return
    */
  // todo Update so map works with class type not comparing like this...
  private def isOperator(block: Block) = {
    OPERATORS(block).nonEmpty
  }

  /**
    * Test the associativity of a certain operator token .
    *
    * @param token The token to be tested (needs to operator).
    * @param type  LEFT_ASSOC or RIGHT_ASSOC
    * @return True if the tokenType equals the input parameter type .
    */
  private def isAssociative(block: Block, assoc: Int): Boolean = {
    if (!isOperator(block)) {
      throw new IllegalArgumentException("Invalid token: ");
    }
    if (OPERATORS(block)(1) == assoc) {
      return true;
    }
    return false;
  }

}
