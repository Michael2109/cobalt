package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IR._

/**
  * Methods for restructuring the IR
  */
object IRProcessor {

  def restructureIR(moduleIR: ModuleIR): Seq[ModelIR] ={
    setModelHeaders(moduleIR.moduleHeaderIR, moduleIR.models.asInstanceOf[Seq[ModelIR]])
    setAssignIds(moduleIR)
    moduleIR.models.asInstanceOf[Seq[ModelIR]]
  }

  def setModelHeaders(moduleHeaderIR: ModuleHeaderIR, modelIRs: Seq[ModelIR]): Unit = {
    modelIRs.map(x => x match {
      case classModel: ClassModelIR => classModel.moduleHeaderIR = moduleHeaderIR
      case objectModel: ObjectModelIR => objectModel.moduleHeaderIR = moduleHeaderIR
      case traitModel: TraitModelIR => traitModel.moduleHeaderIR = moduleHeaderIR
    })
  }

  def setAssignIds(moduleIR: ModuleIR): Unit ={
    var count = 0
    moduleIR.models.map(x => {
      def assignId(statementIR: StatementIR) {
        statementIR match {
          case assign: AssignIR => {
            assign.id = count
            count = count + 1
          }
          case blockStmt: BlockStmtIR => blockStmt.statements.map(assignId)
          case inline: InlineIR => assignId(inline)
          case method: MethodIR => assignId(method.body)
          case _ =>
        }
      }
      assignId(x)
    })
  }
}
