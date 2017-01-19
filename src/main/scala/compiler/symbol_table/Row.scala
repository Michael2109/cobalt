package compiler.symbol_table

class Row {
  private var id: Int = 0
  private var `type`: String = null
  private var name: String = null
  private var value: String = null
  private var methodName: String = null
  private var className: String = null

  def getId: Int = {
    return id
  }

  def setId(id: Int): Row = {
    this.id = id
    return this
  }

  def getType: String = {
    return `type`
  }

  def setType(`type`: String): Row = {
    this.`type` = `type`
    return this
  }

  def getName: String = {
    return name
  }

  def setName(name: String): Row = {
    this.name = name
    return this
  }

  def getValue: String = {
    return value
  }

  def setValue(value: String): Row = {
    this.value = value
    return this
  }

  def getMethodName: String = {
    return methodName
  }

  def setMethodName(methodName: String): Row = {
    this.methodName = methodName
    return this
  }

  def getClassName: String = {
    return className
  }

  def setClassName(className: String): Row = {
    this.className = className
    return this
  }

  override def toString: String = {
    return id + " : " + name + " : " + `type` + " : " + value + " " + methodName + " " + className
  }
}