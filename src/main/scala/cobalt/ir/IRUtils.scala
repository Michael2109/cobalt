package cobalt.ir

import cobalt.ast.AST.{Expression, IntConst}

object IRUtils {

  def getExpressionType(expression: Expression): String ={
    expression match {
      case _: IntConst => "I"
    }
  }

}
