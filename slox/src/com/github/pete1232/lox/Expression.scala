package com.github.pete1232.lox

import com.github.pete1232.lox.Token.OperatorToken

sealed trait Expression

final case class Binary(
    left: Expression,
    operator: OperatorToken,
    right: Expression,
)
