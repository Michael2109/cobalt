package cobalt.jar_loader

case class BytecodeClass(name: String, access: Int, methods: List[BytecodeMethod]) {
  def getMethod(methodName: String): BytecodeMethod ={
    methods.find(_.name.equals(methodName)) match {
      case Some(method) => method
      case None => throw new Exception("Method not found: " + methodName)
    }
  }
}
