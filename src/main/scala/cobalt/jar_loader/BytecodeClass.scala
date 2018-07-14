package cobalt.jar_loader

case class BytecodeClass(name: String, access: Int, methods: List[BytecodeMethod]) {
  def getMethod(methodName: String, arguments: List[String]): BytecodeMethod ={
    methods.find(m => m.name.equals(methodName) && m.arguments.equals(arguments)) match {
      case Some(method) => method
      case None => throw new Exception("Method not found: " + methodName)
    }
  }
}
