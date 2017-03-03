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

package compiler.structure.blocks.comments

import compiler.structure.blocks.Block

/**
  * Represents a code comment
  * E.g. "// This is a comment"
  *
  * @param superBlockInit
  */
class CommentBlock(var superBlockInit: Block) extends Block(superBlockInit, false, false) {

  // Called after file is parsed
  override def init() {}

  /* Symbol table information */
  override def getName: String = ""

  override def getValue: String = ""

  override def getType(): String = "comment"

  /* Opening and closing byte code */
  override def getOpeningCode: String = ""

  override def getClosingCode: String = ""

  /* toString method */
  override def toString: String = "Comment"

}