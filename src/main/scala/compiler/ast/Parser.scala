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

package compiler.ast

import compiler.ast.cases.CaseParser
import compiler.ast.conditionals.and.AndOpParser
import compiler.ast.conditionals.equals.EqualsOpParser
import compiler.ast.conditionals.larger_than.LargerThanOpParser
import compiler.ast.conditionals.larger_than_equal.LargerThanEqualOpParser
import compiler.ast.conditionals.not.NotOpParser
import compiler.ast.conditionals.or.OrOpParser
import compiler.ast.conditionals.smaller_than.SmallerThanOpParser
import compiler.ast.conditionals.smaller_than_equal.SmallerThanEqualOpParser
import compiler.ast.constants.boolean_constant.BooleanConstantParser
import compiler.ast.constants.byte_constant.ByteConstantParser
import compiler.ast.constants.char_constant.CharConstantParser
import compiler.ast.constants.double_constant.DoubleConstantParser
import compiler.ast.constants.float_constant.FloatConstantParser
import compiler.ast.constants.int_constant.IntConstantParser
import compiler.ast.constants.long_constant.LongConstantParser
import compiler.ast.constants.short_constant.ShortConstantParser
import compiler.ast.constants.string_constant.StringConstantParser
import compiler.ast.exceptions.exception_catch.CatchParser
import compiler.ast.exceptions.exception_finally.FinallyParser
import compiler.ast.exceptions.exception_throw.ThrowParser
import compiler.ast.exceptions.exception_throws.ThrowsParser
import compiler.ast.exceptions.exception_try.TryParser
import compiler.ast.ifs.{ElifParser, ElseParser, IfParser}
import compiler.ast.imports.ImportParser
import compiler.ast.loops.{DoParser, ForParser, WhileParser}
import compiler.ast.modifiers.ModifierParser
import compiler.ast.operators._
import compiler.ast.operators.assignment._
import compiler.ast.operators.assignment.bit._
import compiler.ast.operators.bit._
import compiler.ast.packages.PackageParser
import compiler.ast.prints.PrintlnParser
import compiler.ast.structures.kinds.{ClassParser, ObjectParser}
import compiler.ast.structures.methods.MethodParser
import compiler.ast.structures.parameter.ParameterParser
import compiler.ast.structures.{MethodCallParser, ObjectDefinitionParser, ObjectMethodCallParser}
import compiler.ast.super_keyword.SuperParser
import compiler.ast.variable.{DefineVariableParser, ThisKeywordParser, VariableParser}
import compiler.tokenizer.Tokenizer

abstract class Parser[T <: Block] {


  /**
    * A list of all regular stack
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

    new SmallerThanEqualOpParser,
    new SmallerThanOpParser,
    new LargerThanEqualOpParser,
    new LargerThanOpParser,
    new EqualsOpParser,

    new AddOpParser,
    new DivideOpParser,
    new MultiplyOpParser,
    new SubtractOpParser,
    new ModulusOpParser,

    new DefineVariableParser,
    new ParameterParser,
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

    new VariableParser


  )

}