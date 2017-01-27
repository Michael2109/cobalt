package compiler.tokenizer

import java.util.regex.Pattern

import compiler.tokenizer.TokenType.TokenType

class TokenData(var pattern: Pattern, var `type`: TokenType) {

  def getPattern: Pattern =  pattern

  def getType: TokenType = `type`
  
}
