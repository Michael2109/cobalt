package compiler.structure.parameters

import compiler.tokenizer.Tokenizer

import scala.collection.mutable.ListBuffer

class Parameters {

  // Loop through tokens to get each parameter. Add each parameter to a list
  def getParameters(line: String): ListBuffer[Parameter] = {

    val result: ListBuffer[Parameter] = new ListBuffer[Parameter]

    val tokenizer = new Tokenizer(line)

    var nextToken: String = tokenizer.nextToken.token
    var paramType: String = ""
    var paramName: String = ""

    var typeNext = false
    while (nextToken != "") {

      if (nextToken == ",") {
        nextToken = tokenizer.nextToken.token
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
        nextToken = tokenizer.nextToken.token
      }
    }

    return result
  }
}
