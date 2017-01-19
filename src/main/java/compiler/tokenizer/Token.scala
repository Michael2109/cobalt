package compiler.tokenizer

import compiler.tokenizer.TokenType.TokenType

class Token(var token: String, var `type`: TokenType) {

  def getToken: String = {
    return token
  }

  def getType: TokenType = {
    return `type`
  }

  override def toString: String = {
    return token
  }
}