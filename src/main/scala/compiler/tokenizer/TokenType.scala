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

package compiler.tokenizer

object TokenType extends Enumeration {
  type TokenType = Value
  val EMPTY,
  TOKEN,
  IDENTIFIER,
  RETURN_TYPE,

  INTEGER_LITERAL,
  DOUBLE_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,
  CHARACTER_LITERAL,

  MODULUS_OPERATOR,
  ADD_OPERATOR,
  SUBTRACT_OPERATOR,
  MULTIPLY_OPERATOR,
  DIVIDE_OPERATOR,
  END_STATEMENT,
  COLON,
  SMALLER_THAN,
  SMALLER_THAN_EQUAL,
  LARGER_THAN,
  LARGER_THAN_EQUAL,
  EQUAL_TO = Value
}