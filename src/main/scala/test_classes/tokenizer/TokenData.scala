package test_classes.tokenizer

import java.util.regex.Pattern

import test_classes.tokenizer.TokenType.TokenType

class TokenData(val pattern: Pattern, val `type`: TokenType) {

  def getPattern: Pattern =  pattern

  def getType: TokenType = `type`
  
}
