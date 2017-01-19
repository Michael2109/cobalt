package compiler.exceptions

class ContainerException() extends RuntimeException {
  def this(message: String) {
    this()
    println(message)
  }

  def this(message: String, cause: Throwable) {
    this()
    println(message + " : " + cause.getStackTraceString)
  }

  def this(cause: Throwable) {
    this()
    println(cause.getStackTraceString)
  }
}