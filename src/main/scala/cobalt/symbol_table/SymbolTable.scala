package cobalt.symbol_table

import cobalt.ast.IRNew.{ObjectType, TypeIR}

import scala.collection.mutable.ListBuffer

abstract class SymbolTable {
  val entries: ListBuffer[SymbolTable] = ListBuffer[SymbolTable]()

  def exists(name: String): Boolean = {
    !entries.filter(_.exists(name)).isEmpty
  }

  def get(name: String): SymbolTable = {
      entries.filter(_.exists(name)).head
  }

  def getId(): Int
}

case class ClassEntry(name: String) extends SymbolTable {
  override def exists(name: String): Boolean = this.name.equals(name)

  override def getId(): Int = -1
}
case class MethodEntry(name: String, `type`: String) extends SymbolTable{
  override def exists(name: String): Boolean = this.name.equals(name)

  override def getId(): Int = -1
}
case class ValueEntry(name: String, id: Int, `type`: TypeIR) extends SymbolTable{
  override def exists(name: String): Boolean = this.name.equals(name)

  override def getId(): Int = id
}
