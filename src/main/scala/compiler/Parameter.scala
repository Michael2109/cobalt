package compiler

// Represents what is input for ASM. E.g. int = 'I', String = 'Ljava/lang/String;'
class Parameter(val typeInit: String, var nameInit: String) {

  private val name: String = nameInit
  private var `type`: String = typeInit

  def getName: String = {
    return nameInit
  }

  // todo add util method to get the asm type from a variable/value
  def getAsmType: String = {
    if (getType == "int") {
      return "I"
    }
    if (getType == "String") {
      return "Ljava/lang/String;"
    }
    return null
  }

  override def toString: String = {
    return getType + " : " + name
  }

  def getType: String = {
    return `type`
  }

  def setType(`type`: String) {
    this.`type` = `type`
  }
}