package test_classes.tokenizer

import test_classes.tokenizer.TokenType.TokenType

class Token(val _token: String, val _type: TokenType) {

  def token: String = _token

  def tokenType: TokenType = _type

  override def toString: String = _token

}
