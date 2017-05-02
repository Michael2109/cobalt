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

package cobalt.ast

import cobalt.ast.cases.CaseParser
import cobalt.ast.conditionals.and.AndOpParser
import cobalt.ast.conditionals.equals.EqualsOpParser
import cobalt.ast.conditionals.larger_than.LargerThanOpParser
import cobalt.ast.conditionals.larger_than_equal.LargerThanEqualOpParser
import cobalt.ast.conditionals.not.NotOpParser
import cobalt.ast.conditionals.or.OrOpParser
import cobalt.ast.conditionals.smaller_than.SmallerThanOpParser
import cobalt.ast.conditionals.smaller_than_equal.SmallerThanEqualOpParser
import cobalt.ast.constants.boolean_constant.BooleanConstantParser
import cobalt.ast.constants.byte_constant.ByteConstantParser
import cobalt.ast.constants.char_constant.CharConstantParser
import cobalt.ast.constants.double_constant.DoubleConstantParser
import cobalt.ast.constants.float_constant.FloatConstantParser
import cobalt.ast.constants.int_constant.IntConstantParser
import cobalt.ast.constants.long_constant.LongConstantParser
import cobalt.ast.constants.short_constant.ShortConstantParser
import cobalt.ast.constants.string_constant.StringConstantParser
import cobalt.ast.exceptions.exception_catch.CatchParser
import cobalt.ast.exceptions.exception_finally.FinallyParser
import cobalt.ast.exceptions.exception_throw.ThrowParser
import cobalt.ast.exceptions.exception_throws.ThrowsParser
import cobalt.ast.exceptions.exception_try.TryParser
import cobalt.ast.expression.ExpressionParser
import cobalt.ast.ifs.{ElifParser, ElseParser, IfParser}
import cobalt.ast.imports.ImportParser
import cobalt.ast.loops.{DoParser, ForParser, WhileParser}
import cobalt.ast.modifiers.ModifierParser
import cobalt.ast.operators._
import cobalt.ast.operators.assignment._
import cobalt.ast.operators.assignment.bit._
import cobalt.ast.operators.bit._
import cobalt.ast.packages.PackageParser
import cobalt.ast.prints.print.PrintParser
import cobalt.ast.prints.println.PrintlnParser
import cobalt.ast.structures.kinds.{ClassParser, ObjectParser}
import cobalt.ast.structures.methods.MethodParser
import cobalt.ast.structures.parameter.ParameterParser
import cobalt.ast.structures.{MethodCallParser, ObjectDefinitionParser, ObjectMethodCallParser}
import cobalt.ast.super_keyword.SuperParser
import cobalt.ast.variable.{DefineVariableParser, ThisKeywordParser, VariableParser}
import cobalt.tokenizer.Tokenizer

abstract class Parser[T <: Block] {


  /**
    * A list of all regular stack
    *
    * @return
    */

  val regex: String

  def shouldParse(line: String): Boolean = regex.r.findFirstIn(line).nonEmpty

  /**
    * Take the superBlock and the tokenizer for the line and return a blocks of this parsers's type.
    */
  def parse(superBlock: Block, tokenizer: Tokenizer): Block
}

object Parsers {

  /* All available parsers */
  val parsers: List[Parser[_]] = List(

    new PackageParser,

    new ClassParser,
    new MethodParser,
    new ObjectParser,
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
    new PrintParser,

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


    /* Expressions */
    new ExpressionParser,

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