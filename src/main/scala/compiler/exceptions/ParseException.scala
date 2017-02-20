package compiler.exceptions

class ParseException() extends RuntimeException {
  def this(message: String) {
    this()
    println(message)
  }

  def this(message: String, cause: Throwable) {
    this()
    println(message)
  }

  def this(cause: Throwable) {
    this()
    cause.getStackTrace
  }
}