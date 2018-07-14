package cobalt.jar_loader

case class BytecodeMethod(name: String, access: Int, arguments: List[String], returnType: String, exceptions: List[String]) {

  def getSignature(): String ={
    "(" + arguments.mkString + ")" + returnType
  }

}
