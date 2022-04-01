package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError

import scala.reflect.ClassTag

import cats.data.NonEmptyList

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
      val (expressionResult, remainingTokens) = expression(tokensIn)
      parseLoop(remainingTokens, result :+ expressionResult)

  private def binaryExpression(
      tokens: List[TokenWithContext],
      matchingTokens: List[Expression.BinaryOperator],
      production: List[TokenWithContext] => (
          Either[ParserError, Expression],
          List[TokenWithContext],
      ),
  ): (Either[ParserError, Expression], List[TokenWithContext]) =

    @scala.annotation.tailrec
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

  private def expression(
      tokens: List[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    equality(tokens)

  private def equality(
      tokens: List[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    binaryExpression(
      tokens,
      List(
        Token.TwoCharacter.EqualEqual,
        Token.TwoCharacter.BangEqual,
      ),
      comparison,
    )

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
            primary(NonEmptyList(tokenWithContext, tokens.tail))
      case None                   =>
        (Left(IncompleteExpression("unary")), tokens)

  private def primary(
      tokens: NonEmptyList[TokenWithContext]
  ): (Either[ParserError, Expression], List[TokenWithContext]) =
    tokens.head.token match
      case Token.Keyword.False =>
        (Right(Expression.Literal(false)), tokens.tail)
      case Token.Keyword.True  => (Right(Expression.Literal(true)), tokens.tail)
      case Token.Keyword.Nil   => (Right(Expression.Literal(null)), tokens.tail)
      case Token.LiteralNumber(_, n)       =>
        (Right(Expression.Literal(n)), tokens.tail)
      case Token.LiteralString(_, s)       =>
        (Right(Expression.Literal(s)), tokens.tail)
      case Token.SingleCharacter.LeftParen =>
        val (expressionResult, remainingTokens) = expression(tokens.tail)
        expressionResult match
          case Left(error) => (expressionResult, remainingTokens)
          case Right(expr) =>
            remainingTokens.headOption.map(_.token) match
              case Some(Token.SingleCharacter.RightParen) =>
                (Right(Expression.Group(expr)), remainingTokens.tail)
              case _                                      =>
                (
                  Left(
                    UnclosedGroupError(tokens.head.context.lineCount)
                  ),
                  remainingTokens.tail,
                )
      case _                               =>
        (
          Left(
            UnmatchedTokenError(
              "primary",
              tokens.head.context.lineCount,
              tokens.head.token,
            )
          ),
          tokens.tail,
        )
end DefaultParser
