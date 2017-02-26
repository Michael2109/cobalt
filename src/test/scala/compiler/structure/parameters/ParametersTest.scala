/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Michael Haywood
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

package compiler.structure.parameters

import compiler.tokenizer.TokenizerTest

import scala.collection.mutable.ListBuffer

class ParametersTest {

  // Loop through tokens to get each parameter. Add each parameter to a list
  def getParameters(line: String): ListBuffer[ParameterTest] = {

    val result: ListBuffer[ParameterTest] = new ListBuffer[ParameterTest]

    val tokenizer = new TokenizerTest(line)

    var nextToken: String = tokenizer.nextToken.token
    var paramType: String = ""
    var paramName: String = ""

    var typeNext = false
    while (nextToken != "") {

      if (nextToken == ",") {
        nextToken = tokenizer.nextToken.token
      } else {
        if (nextToken == ":") {
          typeNext = true
        }
        else if (typeNext) {
          paramType = nextToken.trim
          typeNext = false
          result += new ParameterTest(paramType, paramName)
        }
        else {
          paramName = nextToken.trim
        }
        nextToken = tokenizer.nextToken.token
      }
    }

    return result
  }
}
