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

import compiler.structure.parsers.Parser
import compiler.structure.parsers.assignment.AssignmentParser
import compiler.structure.parsers.constants.{DoubleConstantParser, FloatConstantParser, IntConstantParser, StringConstantParser}
import compiler.structure.parsers.ifs.IfParser
import compiler.structure.parsers.imports.ImportParser
import compiler.structure.parsers.loops.{ForParser, WhileParser}
import compiler.structure.parsers.modifiers.ModifierParser
import compiler.structure.parsers.operators.{AddParser, DivideParser, MultiplyParser, SubtractParser}
import compiler.structure.parsers.packages.PackageParser
import compiler.structure.parsers.structures.kinds.{ClassParser, ObjectParser}
import compiler.structure.parsers.structures.methods.MethodParser
import compiler.structure.parsers.structures.{MethodCallParser, ObjectMethodCallParser}
import compiler.structure.parsers.variable.{DefineVariableParser, VariableParser}

object Constants {

  /* All available parsers */
  val parsers: Array[Parser[_]] = Array(
    new ModifierParser,
    new MethodParser,
    new AddParser,
    new DivideParser,
    new MultiplyParser,
    new SubtractParser,
    new IfParser,
    //  new PrintParser,
    new ForParser,
    new MethodCallParser,
    new ImportParser,
    new WhileParser,
    //   new ObjectDefinitionParser,
    new ObjectMethodCallParser,
    new PackageParser,
    new ObjectParser,
    new ClassParser,

    new AssignmentParser,
    new DefineVariableParser,

    new VariableParser,
    new StringConstantParser,
    new FloatConstantParser,
    new DoubleConstantParser,
    new IntConstantParser
  )

}
