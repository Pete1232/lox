package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError

import scala.reflect.ClassTag

trait Parser:
  def parse(
      tokens: List[TokenWithContext]
  ): List[Either[ParserError, Expression]]

object DefaultParser extends Parser:

  import ParserError.*
  def parse(
      tokens: List[TokenWithContext]
  ): List[Either[ParserError, Expression]] =
    parseLoop(tokens)

  @scala.annotation.tailrec
  private def parseLoop(
      tokensIn: List[TokenWithContext],
      result: List[Either[ParserError, Expression]] = Nil,
  ): List[Either[ParserError, Expression]] =
    if tokensIn.isEmpty then result
    else
      val (expressionResult, remainingTokens) = comparison(tokensIn)
      parseLoop(remainingTokens, result :+ expressionResult)

  private def binaryExpression(
      tokens: List[TokenWithContext],
      matchingTokens: List[Expression.BinaryOperator],
      production: List[TokenWithContext] => (
          Either[ParserError, Expression],
          List[TokenWithContext],
      ),
  ): (Either[ParserError, Expression], List[TokenWithContext]) =

    def leftAssociativeLoop(
        leftExpr: Expression,
        tokens: List[TokenWithContext],
    ): (Either[ParserError, Expression], List[TokenWithContext]) =
      tokens.headOption match
        case Some(tokenWithContext) =>
          matchingTokens.find(_ == tokenWithContext.token) match
            case None                => (Right(leftExpr), tokens)
            case Some(operatorToken) =>
              production(tokens.tail) match
                case (Left(error), remainingTokens)      =>
                  (Left(error), remainingTokens)
                case (Right(rightExpr), remainingTokens) =>
                  leftAssociativeLoop(
                    Expression.Binary(leftExpr, operatorToken, rightExpr),
                    remainingTokens,
                  )
        case _                      => (Right(leftExpr), tokens)

    production(tokens) match
      case (Right(leftExpr), remainingTokens) =>
        leftAssociativeLoop(leftExpr, remainingTokens)
      case (Left(err), remainingTokens)       => (Left(err), remainingTokens)

  private def comparison(
      tokens: List[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    binaryExpression(
      tokens,
      List(
        Token.SingleCharacter.Greater,
        Token.TwoCharacter.GreaterEqual,
        Token.SingleCharacter.Less,
        Token.TwoCharacter.LessEqual,
      ),
      term,
    )

  private def term(
      tokens: List[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    binaryExpression(
      tokens,
      List(Token.SingleCharacter.Minus, Token.SingleCharacter.Plus),
      factor,
    )

  private def factor(
      tokens: List[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    binaryExpression(
      tokens,
      List(Token.SingleCharacter.Star, Token.SingleCharacter.Slash),
      unary,
    )

  private def unary(
      tokens: List[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    tokens.headOption match
      case Some(tokenWithContext) =>
        tokenWithContext.token match
          case t @ (Token.SingleCharacter.Bang | Token.SingleCharacter.Minus) =>
            unary(tokens.tail) match
              case (Left(error), remainingTokens)       =>
                (Left(error), remainingTokens)
              case (Right(expression), remainingTokens) =>
                (
                  Right(
                    Expression.Unary(t, expression)
                  ),
                  remainingTokens,
                )
          case _                                                              =>
            (primary(tokenWithContext), tokens.tail)
      case None                   =>
        (Left(IncompleteExpression("unary")), tokens)

  private def primary(
      token: TokenWithContext
  ): Either[ParserError, Expression] =
    token.token match
      case Token.Keyword.False       => Right(Expression.Literal(false))
      case Token.Keyword.True        => Right(Expression.Literal(true))
      case Token.Keyword.Nil         => Right(Expression.Literal(null))
      case Token.LiteralNumber(_, n) => Right(Expression.Literal(n))
      case Token.LiteralString(_, s) => Right(Expression.Literal(s))
      case _                         =>
        Left(
          UnmatchedTokenError("primary", token.context.lineCount, token.token)
        )
end DefaultParser
