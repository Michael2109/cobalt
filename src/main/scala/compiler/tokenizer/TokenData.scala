package compiler.tokenizer

import java.util.regex.Pattern

import TokenType.TokenType

import scala.util.matching.Regex

class TokenData(val pattern: Regex, val `type`: TokenType) {

  def getPattern: Regex =  pattern

  def getType: TokenType = `type`
  
}
