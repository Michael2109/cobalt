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

package cobalt_tests.primitives

import compiler.runtime.Main
import compiler.utilities.Utils
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}


@RunWith(classOf[JUnitRunner])
class BooleanTest() extends FunSuite with BeforeAndAfter {


  test("Boolean primitive test") {
    Main.main(Array("cobalt_source/test/primitives/BooleanTest.cobalt", "cobalt_java/test/primitives/BooleanTest.java", "cobalt_generated"))

    val output = Utils.executionOutput("cobalt_generated", "test.primitives.BooleanTest")
    println("<OUTPUT_START>")
    println(output)
    println("<OUTPUT_END>")

    // assert(output(0).equals("false"))
    //assert(output(1).equals("true"))
  }

}