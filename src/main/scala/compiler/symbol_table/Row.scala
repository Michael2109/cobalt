package compiler.symbol_table

class Row {
  private var id: Int = 0
  private var `type`: String = null
  private var name: String = null
  private var value: String = null
  private var methodName: String = null
  private var className: String = null
  private var immutable: Boolean = false

  def getId: Int = id

  def setId(id: Int): Row = {
    this.id = id
    return this
  }

  def getType: String = `type`

  def setType(`type`: String): Row = {
    this.`type` = `type`
    this
  }

  def getName: String = name

  def setName(name: String): Row = {
    this.name = name
    this
  }

  def getValue: String = value

  def setValue(value: String): Row = {
    this.value = value
    this
  }

  def getMethodName: String = methodName

  def setMethodName(methodName: String): Row = {
    this.methodName = methodName
    this
  }

  def getClassName: String = className

  def setClassName(className: String): Row = {
    this.className = className
    this
  }

  def getImmutable: Boolean = immutable

  def setImmutable(immutable : Boolean) : Row = {
    this.immutable = immutable
    this
  }

  override def toString: String = {
    return id + " : " + name + " : " + `type` + " : " + value + " " + methodName + " " + className
  }
}