package compiler.tokenizer

import java.util.regex.Pattern

import compiler.tokenizer.TokenType.TokenType

class TokenData(val pattern: Pattern, val `type`: TokenType) {

  def getPattern: Pattern =  pattern

  def getType: TokenType = `type`
  
}
