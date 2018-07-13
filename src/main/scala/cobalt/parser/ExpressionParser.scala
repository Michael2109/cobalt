package cobalt.parser

import fastparse.noApi._
import WsApi._
import cobalt.ast.AST._
import cobalt.ast.AST
import cobalt.parser.ExpressionParser.newClassInstanceParser

object ExpressionParser {

  def accessModifier: P[Modifier] = P(LexicalParser.kw("protected")).map(_ => Protected()) | P(LexicalParser.kw("private")).map(_ => Private()) | P(LexicalParser.kw("local")).map(_ => PackageLocal())

  def annotationParser: P[Annotation] = P("@" ~ nameParser).map(Annotation)

  val expressionParser: P[Expression] = {
    def parensParser: P[Expression] = P( "(" ~ (expressionParser) ~ ")" )
    def termParser: P[Expression] = P(Chain(allExpressionsParser, multiply | divide ))
    def arith_exprParser: P[Expression] = P(Chain(termParser, add | subtract))
    def rExprParser: P[Expression] = P(Chain(arith_exprParser, LtE | Lt | GtE | Gt))

    def allExpressionsParser = methodCallParser | newClassInstanceParser | ternaryParser | numberParser | identifierParser | stringLiteral | parensParser
    P(Chain(rExprParser, and | or)).rep(sep = ".").map(expressions => {
      expressions.length match {
        case 0 => BlockExpr(Seq())
        case 1 => expressions.head
        case _ => NestedExpr(expressions)
      }
    })
  }

  def identifierParser: P[AST.Identifier] = LexicalParser.identifier.map(x => Identifier(Name(x)))

  def finalModifierParser: P[AST.Final.type] = P("final").map(x => Final)

  def methodCallParser: P[MethodCall] = P(nameParser ~ "(" ~ expressionParser.rep(sep = ",") ~ ")").map(x => MethodCall(x._1, BlockExpr(x._2)))

  def modifiers: P[Seq[Modifier]] = P(accessModifier | typeModifier).rep

  def nameParser: P[Name] = LexicalParser.identifier.map(x => Name(x))

  def newClassInstanceParser: P[NewClassInstance] = P(LexicalParser.kw("new") ~ typeRefParser ~ LexicalParser.kw("(") ~ expressionParser.rep(sep = ",") ~ LexicalParser.kw(")")).map(x => NewClassInstance(x._1, BlockExpr(x._2), None))

  def numberParser: P[Expression] = P(LexicalParser.floatnumber ~ P("F" | "f")).map(FloatConst) | P(LexicalParser.longinteger).map(LongConst) | P(LexicalParser.floatnumber).map(DoubleConst) | P(LexicalParser.integer).map(IntConst)

  def stringLiteral: P[StringLiteral] = LexicalParser.stringliteral.map(x => StringLiteral(x))

  def ternaryParser: P[Ternary] = P(LexicalParser.kw("if") ~ expressionParser ~ "then" ~ expressionParser ~ "else" ~ expressionParser).map(x => Ternary(x._1, x._2, x._3))

  def typeModifier: P[Modifier] = P(LexicalParser.kw("mutable")).map(_ => Final()) | P(LexicalParser.kw("abstract")).map(_ => Abstract()) | P(LexicalParser.kw("pure")).map(_ => Pure())

  def typeRefParser: P[Type] = refParser.map(Type)

  def refParser: P[Ref] = P(nameParser.rep(sep = ".", min=2)).map(x => RefQual(QualName(NameSpace(x.dropRight(1)), x.last))) | P(nameParser).map(RefLocal)

  private def Chain(p: P[Expression], op: P[AST.Operator]) = P(p ~ (op ~ p).rep).map {
    case (lhs, chunks) =>
      chunks.foldLeft(lhs) { case (lhs, (operator, rhs)) =>
        operator match {
          case op: ABinOp => new ABinary(op, lhs, rhs)
          case op: BBinOp => new BBinary(op, lhs, rhs)
          case op: RBinOp => new RBinary(op, lhs, rhs)
        }

      }
  }

  def op[T](s: P0, rhs: T) = s.!.map(_ => rhs)
  val Lt = op("<", AST.Less)
  val Gt = op(">", AST.Greater.asInstanceOf[Operator])
  val Eq = op("==", AST.Equal.asInstanceOf[Operator])
  val GtE = op(">=", AST.GreaterEqual.asInstanceOf[Operator])
  val LtE = op("<=", AST.LessEqual.asInstanceOf[Operator])
  val comp_op = P(LtE | GtE | Eq | Gt | Lt)
  val add = op("+", AST.Add.asInstanceOf[Operator])
  val subtract = op("-", AST.Subtract.asInstanceOf[Operator])
  val multiply = op("*", AST.Multiply.asInstanceOf[Operator])
  val divide = op("/", AST.Divide.asInstanceOf[Operator])
  val and = op("&&", AST.And.asInstanceOf[Operator])
  val or = op("||", AST.Or.asInstanceOf[Operator])
}
