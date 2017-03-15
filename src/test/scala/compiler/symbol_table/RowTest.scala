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

package compiler.symbol_table

class RowTest {
  private var id: Int = 0
  private var `type`: String = null
  private var name: String = null
  private var value: String = null
  private var methodName: String = null
  private var className: String = null
  private var immutable: Boolean = false

  def getId: Int = id

  def setId(id: Int): RowTest = {
    this.id = id
    return this
  }

  def getType: String = `type`

  def setType(`type`: String): RowTest = {
    this.`type` = `type`
    this
  }

  def getName: String = name

  def setName(name: String): RowTest = {
    this.name = name
    this
  }

  def getValue: String = value

  def setValue(value: String): RowTest = {
    this.value = value
    this
  }

  def getMethodName: String = methodName

  def setMethodName(methodName: String): RowTest = {
    this.methodName = methodName
    this
  }

  def getClassName: String = className

  def setClassName(className: String): RowTest = {
    this.className = className
    this
  }

  def getImmutable: Boolean = immutable

  def setImmutable(immutable: Boolean): RowTest = {
    this.immutable = immutable
    this
  }

  override def toString: String = {
    return id + " : " + name + " : " + `type` + " : " + value + " " + methodName + " " + className
  }
}