package compiler.tokenizer

import java.util.regex.Pattern

import compiler.tokenizer.TokenType.TokenType

class TokenData(var pattern: Pattern, var `type`: TokenType) {
  def getPattern: Pattern = {
    return pattern
  }

  def getType: TokenType = {
    return `type`
  }
}