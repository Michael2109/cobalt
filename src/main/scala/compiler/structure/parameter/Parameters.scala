package compiler.structure.parameter

import java.util.{ArrayList, List}

import compiler.tokenizer.Tokenizer

class Parameters(line: String) {

  getParameters(line)

  // Loop through tokens to get each parameter. Add each parameter to a list
  private def getParameters(line: String): List[Parameter] = {

    val result: List[Parameter] = new ArrayList[Parameter]

    val tokenizer = new Tokenizer(line)

    var nextToken: String = tokenizer.nextToken.getToken
    var i: Int = 0
    var paramType: String = ""
    var paramName: String = ""

    while (nextToken != ")") {

      //	parameters += " " + nextToken + " ";
      if (nextToken == ",") {
        nextToken = tokenizer.nextToken.getToken
      } else {
        if (i % 2 == 0) {
          paramType = nextToken.trim
        }
        else {
          paramName = nextToken.trim
          result.add(new Parameter(paramType, paramName))
        }
        nextToken = tokenizer.nextToken.getToken
        i += 1
      }
    }

    return result
  }
}
