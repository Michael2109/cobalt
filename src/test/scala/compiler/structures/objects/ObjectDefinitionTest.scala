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

package compiler.structures.objects

import java.io.File

import compiler.Base
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class ObjectDefinitionTest() extends FunSuite with BeforeAndAfter with Base {

  override val cobaltFile = new File("src/test/resources/source/structures/objects/ObjectDefinitionTest.cobalt")
  override val asmFile = new File("src/test/resources/asm/structures/objects/ObjectDefinitionTest.java")
  override val buildFile = new File("src/test/resources/generated/structures/objects/ObjectDefinitionTest.class")
  override val classPath = new File("src/test/resources/asm")

  test("Object definition test") {
    compile()
    val output = executeOutput()
    assert("10" == output(0))
  }

}