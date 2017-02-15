package test_classes.exceptions

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
    println(cause.getStackTraceString)
  }
}