package compiler

// Represents what is input for ASM. E.g. int = 'I', String = 'Ljava/lang/String;'
class Parameter(val `type`: String, var name: String) {



  // Name of the variable
  def getName: String = name

  // Type of the variable
  def getType: String = `type`

  def setType(`type`: String) {
    this.`type` = `type`
  }

  // ASM type. int = "I", String = "java.lang.String", etc
  def getAsmType: String = {
    if (getType == "int") return "I"
   
    if (getType == "String") return "Ljava/lang/String;"

    return null
  }

  override def toString: String = {
    return `type` + " : " + name
  }

}
