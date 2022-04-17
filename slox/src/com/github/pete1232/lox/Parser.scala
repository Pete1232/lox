package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError
import com.github.pete1232.lox.io.Logging
import com.github.pete1232.lox.models.{
  ExpressionContext,
  Token,
  TokenWithContext,
}
import com.github.pete1232.lox.models.Expression

import cats.data.NonEmptyList
import cats.effect.IO
import org.typelevel.log4cats.Logger

trait Parser:
  def parse(
      tokens: List[TokenWithContext]
  ): IO[List[Either[ParserError, Expression]]]

object DefaultParser extends Parser with Logging:

  type ParserResponse =
    IO[(Either[ParserError, Expression], List[TokenWithContext])]

  import ParserError.*

  def parse(
      tokens: List[TokenWithContext]
  ): IO[List[Either[ParserError, Expression]]] =
    withLogger(parseLoop(tokens))

  private def parseLoop(
      tokensIn: List[TokenWithContext],
      result: IO[List[Either[ParserError, Expression]]] = IO.pure(Nil),
  )(using Logger[IO]): IO[List[Either[ParserError, Expression]]] =
    if tokensIn.isEmpty then result
    else
      expression(tokensIn).flatMap { parseResult =>
        val (expressionResult, remainingTokens) = parseResult
        val tokensToParse                       =
          expressionResult match
            case Right(_) => remainingTokens
            case Left(binaryOperatorError: BinaryExpressionNotOpened) =>
              remainingTokens
            case _ => synchronize(remainingTokens)
        parseLoop(tokensToParse, result.map(_ :+ expressionResult))
      }

  private def binaryExpression(
      tokens: List[TokenWithContext],
      matchingTokens: List[Expression.BinaryOperator],
      production: List[TokenWithContext] => ParserResponse,
  )(using Logger[IO]): ParserResponse =

    def leftAssociativeLoop(
        leftExpr: Expression,
        tokens: List[TokenWithContext],
    ): ParserResponse =
      tokens.headOption match
        case Some(tokenWithContext) =>
          matchingTokens.find(_ == tokenWithContext.token) match
            case None                => IO.pure((Right(leftExpr), tokens))
            case Some(operatorToken) =>
              production(tokens.tail).flatMap {
                _ match
                  case (Left(error), remainingTokens)      =>
                    IO.pure((Left(error), remainingTokens))
                  case (Right(rightExpr), remainingTokens) =>
                    leftAssociativeLoop(
                      Expression.Binary(leftExpr, operatorToken, rightExpr)(
                        using ExpressionContext(tokenWithContext.context)
                      ),
                      remainingTokens,
                    )
              }
        case _                      => IO.pure((Right(leftExpr), tokens))

    production(tokens).flatMap {
      _ match
        case (Right(leftExpr), remainingTokens) =>
          leftAssociativeLoop(leftExpr, remainingTokens)
        case (Left(err), remainingTokens)       =>
          IO.pure((Left(err), remainingTokens))
    }

  private def expression(
      tokens: List[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing an expression") *>
      comma(tokens)

  private def comma(
      tokens: List[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing a comma expression") *>
      binaryExpression(
        tokens,
        List(Token.SingleCharacter.Comma),
        conditional,
      )

  private def conditional(
      tokens: List[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing a conditional expression") *>
      equality(tokens).flatMap {
        _ match
          case (Left(err), remainingTokens)                =>
            IO.pure((Left(err), remainingTokens))
          case (Right(leftExpr), remainingTokensAfterLeft) =>
            remainingTokensAfterLeft.headOption match
              case Some(questionToken)
                  if questionToken.token == Token.SingleCharacter.Question =>
                expression(remainingTokensAfterLeft.tail).flatMap {
                  _ match
                    case (Left(err), remainingTokens)                    =>
                      IO.pure((Left(err), remainingTokens))
                    case (Right(middleExpr), remainingTokensAfterMiddle) =>
                      remainingTokensAfterMiddle.headOption match
                        case Some(colonToken)
                            if colonToken.token == Token.SingleCharacter.Colon =>
                          conditional(remainingTokensAfterMiddle.tail).map {
                            _ match
                              case (Left(err), remainingTokens) =>
                                (Left(err), remainingTokens)
                              case (
                                    Right(rightExpr),
                                    remainingTokensAfterRight,
                                  ) =>
                                (
                                  Right(
                                    Expression
                                      .Ternary(leftExpr, middleExpr, rightExpr)(
                                        using
                                        ExpressionContext(questionToken.context)
                                      )
                                  ),
                                  remainingTokensAfterRight,
                                )
                          }
                        case _ =>
                          IO.pure(
                            (
                              Left(
                                IncompleteConditionalError(
                                  questionToken.context.lineCount
                                )
                              ),
                              remainingTokensAfterMiddle,
                            )
                          )
                }
              case _ => IO.pure((Right(leftExpr), remainingTokensAfterLeft))
      }

  private def equality(
      tokens: List[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing an equality expression") *>
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
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing a comparison expression") *>
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
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing a term expression") *>
      binaryExpression(
        tokens,
        List(Token.SingleCharacter.Minus, Token.SingleCharacter.Plus),
        factor,
      )

  private def factor(
      tokens: List[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing a factor expression") *>
      binaryExpression(
        tokens,
        List(Token.SingleCharacter.Star, Token.SingleCharacter.Slash),
        unary,
      )

  private def unary(
      tokens: List[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    Logger[IO].trace("Parsing a unary expression") *> {
      tokens.headOption match
        case Some(tokenWithContext) =>
          tokenWithContext.token match
            case t @ (Token.SingleCharacter.Bang |
                Token.SingleCharacter.Minus) =>
              unary(tokens.tail).map {
                _ match
                  case (Left(error), remainingTokens)       =>
                    (Left(error), remainingTokens)
                  case (Right(expression), remainingTokens) =>
                    (
                      Right(
                        Expression.Unary(t, expression)(
                          using ExpressionContext(tokenWithContext.context)
                        )
                      ),
                      remainingTokens,
                    )
              }
            case _ =>
              primary(NonEmptyList(tokenWithContext, tokens.tail))
        case None                   =>
          IO.pure((Left(IncompleteExpression("unary")), tokens))
    }

  private def primary(
      tokens: NonEmptyList[TokenWithContext]
  )(using Logger[IO]): ParserResponse =
    val expressionContext = ExpressionContext(tokens.head.context)
    Logger[IO].trace("Parsing a primary expression") *> {
      tokens.head.token match
        case Token.Keyword.False                                          =>
          IO.pure(
            (
              Right(Expression.Literal(false)(using expressionContext)),
              tokens.tail,
            )
          )
        case Token.Keyword.True                                           =>
          IO.pure(
            (
              Right(Expression.Literal(true)(using expressionContext)),
              tokens.tail,
            )
          )
        case Token.Keyword.Nil                                            =>
          IO.pure(
            (
              Right(Expression.Literal(null)(using expressionContext)),
              tokens.tail,
            )
          )
        case Token.LiteralNumber(_, n)                                    =>
          IO.pure(
            (Right(Expression.Literal(n)(using expressionContext)), tokens.tail)
          )
        case Token.LiteralString(_, s)                                    =>
          IO.pure(
            (Right(Expression.Literal(s)(using expressionContext)), tokens.tail)
          )
        case Token.SingleCharacter.LeftParen                              =>
          Logger[IO].trace("Parsing a group expression") *>
            expression(tokens.tail).map { result =>
              val (expressionResult, remainingTokens) = result
              expressionResult match
                case Left(error) => (expressionResult, remainingTokens)
                case Right(expr) =>
                  remainingTokens.headOption.map(_.token) match
                    case Some(Token.SingleCharacter.RightParen) =>
                      (
                        Right(Expression.Group(expr)(using expressionContext)),
                        remainingTokens.tail,
                      )
                    case _                                      =>
                      (
                        Left(
                          UnclosedGroupError(tokens.head.context.lineCount)
                        ),
                        remainingTokens,
                      )
            }
        case Token.SingleCharacter.Slash | Token.SingleCharacter.Star     =>
          Logger[IO].debug("/ or * without left operand") *>
            binaryOperatorError(
              tokens.head.context.lineCount,
              tokens.tail,
              unary,
            )
        case Token.SingleCharacter.Plus                                   =>
          Logger[IO].debug("+ without left operand") *>
            binaryOperatorError(
              tokens.head.context.lineCount,
              tokens.tail,
              factor,
            )
        case Token.SingleCharacter.Greater | Token.SingleCharacter.Less |
            Token.TwoCharacter.GreaterEqual | Token.TwoCharacter.LessEqual =>
          Logger[IO].debug("> or >= or < or <= without left operand") *>
            binaryOperatorError(
              tokens.head.context.lineCount,
              tokens.tail,
              term,
            )
        case Token.TwoCharacter.EqualEqual | Token.TwoCharacter.BangEqual =>
          Logger[IO].debug("== or != without left operand") *>
            binaryOperatorError(
              tokens.head.context.lineCount,
              tokens.tail,
              comparison,
            )
        case _                                                            =>
          Logger[IO]
            .debug(
              s"${tokens.head.show} not matched - probably a token wasn't handled properly."
            )
            .as(
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
            )
      end match
    }
  end primary

  private def binaryOperatorError(
      errorLine: Int,
      tokens: List[TokenWithContext],
      production: List[TokenWithContext] => ParserResponse,
  )(using Logger[IO]): ParserResponse =
    Logger[IO].debug("Detected binary operator that wasn't opened correctly") *>
      production(tokens).map { result =>
        val remainingTokens = result._2
        (
          Left(
            BinaryExpressionNotOpened(errorLine)
          ),
          remainingTokens,
        )
      }

  private def synchronize(
      tokens: List[TokenWithContext]
  ): List[TokenWithContext] =
    import Token.Keyword.*
    tokens.headOption match
      case Some(tokenWithContext) =>
        if (tokenWithContext.token == Token.SingleCharacter.Semicolon) then
          tokens.tail
        else if List(Class, For, Fun, If, Print, Return, Var, While)
            .contains(tokenWithContext.token)
        then tokens
        else synchronize(tokens.tail)
      case None                   => List.empty

end DefaultParser
