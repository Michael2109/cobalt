package compiler.generators

import compiler.generators.structures.methods.MethodGen

object Generator extends Enumeration {
  type TokenType = Value
  val METHOD_GEN = new MethodGen


}
