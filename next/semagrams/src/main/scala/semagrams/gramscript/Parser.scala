package semagrams.gramscript

import cats.parse.{Parser0, Parser => P, Numbers}
import semagrams.gramscript.Syntax._

object Parser {
    /* Edna voice: NO TABS */
    private[this] val anyspace: P[Unit] = P.charIn(" \r\n").void
    private[this] val anyspaces0: Parser0[Unit] = anyspace.rep0.void

    private[this] val spaces: P[Unit] = P.char(' ').rep.void
    private[this] val spaces0: Parser0[Unit] = P.char(' ').rep0.void

    private[this] val newline: P[Unit] =
        P.char('\n').surroundedBy(P.char(' ').rep0)
    private[this] val comma: P[Unit] =
        P.char(',').surroundedBy(anyspace.rep0)

    private[this] val openParen: P[Unit] = P.char('(').void
    private[this] val closeParen: P[Unit] = P.char(')').void

    private[this] val end: P[Unit] = P.string("end").void

    private def alphaIdentChar(c: Char): Boolean = {
        (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
        (c == '_') || (c >= '0' && c <= '9') || (c == '!')
    }

    private def alphaIdentBeginChar(c: Char): Boolean = {
        (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c == '_')
    }

    private def specialChar(c: Char): Boolean = {
        "+*-/^%:!&".contains(c)
    }

    private[this] val bool = P.string("true").as(Literal.B(true))
        .orElse(P.string("false").as(Literal.B(false)))

    private[this] val alphaSymStr = (
        P.charWhere(alphaIdentBeginChar) ~ P.charWhere(alphaIdentChar)
    ).map({ case (c,cs) => (c::cs).mkString })

    private[this] val specialSymStr = P.charWhere(specialChar).rep.map(_.mkString)

    private[this] val symStr = alphaSymStr.orElse(specialSymStr)

    private[this] val symbol = (P.char(':') *> symStr).map(s => Literal.Sym(s))

    private[this] val int  = Numbers.digits.map((s: String) => Literal.I(s.toInt))

    private[this] val float = for {
        intPart <- Numbers.digits
        _ <- P.char('.')
        decimalPart <- Numbers.digits0
    } yield Literal.R((intPart + "." + decimalPart).toDouble)

    val literal = bool.orElse(symbol).orElse(float.backtrack).orElse(int)

    val lit = literal.map(Expr.Lit(_))

    val identifier = symStr.filter(s => !reserved.contains(s)).map(Expr.Id(_))

    def interp(expr: P[Expr]): P[Expr] = {
        // Not going to worry about escaping yet
        val strSeg = P.charWhere(c => c != '$' && c != '"')
            .rep.map(_.toList.mkString)

        val interpSeg = for {
            _ <- P.string("${")
            e <- recurse
            _ <- P.char('}')
        } yield e

        val seg = str_body.map(Left(_)).
            orElse(interp_body.map(Right(_)))

        for {
            _ <- P.char('"')
            segs <- seg.rep0
            _ <- P.char('"')
        } yield Expr.Call(
            Expr.Id("concat"),
            segs.toSeq.map(
                {
                    case Left(s) => Expr.Lit(Literal.S(s))
                    case Right(e) => e
                }))
    }

    def block(expr: P[Expr]): P[Expr] = {
        for {
            name <- P.stringIn(block_keywords).backtrack
            _ <- spaces
            head <- recurse
            _ <- newline
            body <- (recurse <* newline).rep0
            _ <- end
        } yield Expr.Block(name, Some(head), body.toSeq)
    }

    val expr: P[Expr] = P.recursive[Expr] { recurse =>
        val subexpr = recurse.between(P.char('('), P.char(')'))
            .orElse(lit)
            .orElse(interp(recurse))
            .orElse(id)
            .backtrack

        val funcArgs = for {
            _ <- openParen
            _ <- anyspaces0
            args <- recurse.repSep0(comma)
            _ <- anyspaces0
            _ <- P.char(',').?
            _ <- anyspaces0
            _ <- closeParen
        } yield args

        val call = for {
            head <- subexpr
            argses <- funcArgs.rep0
        } yield argses.foldLeft(head)({ case (e, args) => Expr.Call(e, args) })

        val opExpr = for {
            e <- call
            ops <- (call ~ call).rep0
        } yield e

        block(recurse).orElse(opExpr)
    }
}
