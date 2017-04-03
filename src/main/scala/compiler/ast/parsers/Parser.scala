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

package compiler.ast.parsers

import compiler.ast.blocks.Block
import compiler.ast.parsers.cases.CaseParser
import compiler.ast.parsers.constants._
import compiler.ast.parsers.exceptions._
import compiler.ast.parsers.ifs.{ElifParser, ElseParser, IfParser, SwitchParser}
import compiler.ast.parsers.imports.ImportParser
import compiler.ast.parsers.loops.{DoParser, ForParser, WhileParser}
import compiler.ast.parsers.modifiers.ModifierParser
import compiler.ast.parsers.operators._
import compiler.ast.parsers.operators.assignment._
import compiler.ast.parsers.operators.assignment.bit._
import compiler.ast.parsers.operators.bit._
import compiler.ast.parsers.packages.PackageParser
import compiler.ast.parsers.prints.PrintlnParser
import compiler.ast.parsers.structures.kinds.{ClassParser, ObjectParser}
import compiler.ast.parsers.structures.methods.MethodParser
import compiler.ast.parsers.structures.{MethodCallParser, ObjectDefinitionParser, ObjectMethodCallParser}
import compiler.ast.parsers.super_keyword.SuperParser
import compiler.ast.parsers.syntax.ColonParser
import compiler.ast.parsers.variable.{DefineVariableParser, ThisKeywordParser, VariableParser}
import compiler.tokenizer.Tokenizer

abstract class Parser[T <: Block] {


  /**
    * A list of all regular expressions
    *
    * @return
    */
  def getRegexs: List[String]


  def shouldParse(line: String): Boolean = getRegexs.exists(_.r.findFirstIn(line).nonEmpty)

  /**
    * Take the superBlock and the tokenizer for the line and return a blocks of this parsers's type.
    */
  def parse(superBlock: Block, tokenizer: Tokenizer): Block
}

object Parsers {

  /* All available parsers */
  val parsers: List[Parser[_]] = List(

    new PackageParser,
    new ObjectParser,
    new ClassParser,
    new MethodParser,

    /* this */
    new ThisKeywordParser,

    /* case */
    new CaseParser,

    /* if, else */
    new IfParser,
    new ElifParser,
    new ElseParser,
    new DoParser,

    /* try, catch, finally */
    new TryParser,
    new CatchParser,
    new FinallyParser,

    /* super */
    new SuperParser,

    /* throw, throws */
    new ThrowParser,
    new ThrowsParser,

    /* printing */
    new PrintlnParser,

    /* loops */
    new ForParser,

    new ObjectDefinitionParser,
    new ObjectMethodCallParser,
    new SwitchParser,
    new ModifierParser,

    new MethodCallParser,
    new ImportParser,
    new WhileParser,



    /* bit assignments */
    new AndAssignOpParser,
    new ExclOrAssignOpParser,
    new InclOrAssignOpParser,
    new LeftShiftAssignOpParser,
    new RightShiftAssignOpParser,

    /* assignments */
    new AddAssignOpParser,
    new ModulusAssignOpParser,
    new MultiplyAssignOpParser,
    new SubtractAssignOpParser,

    /* bit */
    new ZeroRightFillShiftOpParser,
    new BitwiseAndOpParser,
    new BitwiseOrOpParser,
    new BitwiseXorOpParser,
    new LeftShiftOpParser,
    new RightShiftOpParser,

    /* operators */
    new OpeningBracketOpParser,
    new ClosingBracketOpParser,
    new PowerOfOpParser,
    new AndOpParser,
    new OrOpParser,
    new NotOpParser,
    new EqualsOpParser,
    new SmallerThanOpParser,
    new AddOpParser,
    new DivideOpParser,
    new MultiplyOpParser,
    new SubtractOpParser,
    new ModulusOpParser,

    new DefineVariableParser,
    new AssignmentOpParser,

    /* constants */
    new BooleanConstantParser,
    new CharConstantParser,
    new StringConstantParser,
    new FloatConstantParser,
    new DoubleConstantParser,
    new ShortConstantParser,
    new ByteConstantParser,
    new LongConstantParser,
    new IntConstantParser,

    new VariableParser,

    /* syntax */
    new ColonParser

  )

}