package compiler.structure.parameter

import compiler.tokenizer.Tokenizer

import scala.collection.mutable.ListBuffer

object Test {
  def main(args: Array[String]): Unit = {
    val parameters = new Parameters().getParameters("x:int, y:int")

    parameters.foreach(println)

  }
}

class Parameters() {

  // Loop through tokens to get each parameter. Add each parameter to a list
  def getParameters(line: String): ListBuffer[Parameter] = {

    val result: ListBuffer[Parameter] = new ListBuffer[Parameter]

    val tokenizer = new Tokenizer(line)

    var nextToken: String = tokenizer.nextToken.getToken
    var paramType: String = ""
    var paramName: String = ""

    var typeNext = false
    while (nextToken != "") {

      if (nextToken == ",") {
        nextToken = tokenizer.nextToken.getToken
      } else {
        if (nextToken == ":") {
          typeNext = true
        }
        else if (typeNext) {
          paramType = nextToken.trim
          typeNext = false
          result += new Parameter(paramType, paramName)
        }
        else {
          paramName = nextToken.trim
        }
        nextToken = tokenizer.nextToken.getToken
      }
    }

    return result
  }
}
