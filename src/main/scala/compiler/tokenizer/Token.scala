package compiler.tokenizer

import compiler.tokenizer.TokenType.TokenType

class Token(var token: String, var `type`: TokenType) {

  def getToken: String = token

  def getType: TokenType = `type`

  override def toString: String = token

}
