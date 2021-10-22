package semagrams.gramscript

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Prop
import org.scalacheck.Gen

import semagrams.gramscript.Parser._
import semagrams.gramscript.Syntax._
import semagrams.gramscript.ParseError

object ParserSpec extends Properties("Parser") {
    // Not going to deal with prefix operators yet
    val intGen = Gen.posNum[Int].map(i => Expr.Lit(Literal.I(i)))
    val symStr = Gen.alphaStr.filter(_.length > 0)
    val symbolGen = symStr.map(s => Expr.Lit(Literal.Sym(s)))
    val doubleGen = Gen.posNum[Double].map(x => Expr.Lit(Literal.R(x)))
    val boolGen = Gen.oneOf(true,false).map(b => Expr.Lit(Literal.B(b)))

    val litGen = Gen.oneOf(intGen, symbolGen, doubleGen, boolGen)

    def checkExpr(e: Expr) = {
        val input = e.toInput
        val parsed = expr.parseAll(input)
        parsed match {
            case Right(ep) => (e == ep) :| "success"
            case Left(err) => {
                val pe = ParseError("<nofile>", input)
                false :| f"error: ${pe.prettyprint(err)}"
            }
        }
    }
    property("literals") = forAll(litGen) { checkExpr }

    val idGen = symStr.map(s => Expr.Id(s))

    def blockGen(depth: Int): Gen[Expr] = {
        val subgenerator = if (depth > 0) {
            Gen.oneOf(idGen, litGen, blockGen(depth - 1), callGen(depth - 1))
        } else {
            Gen.oneOf(idGen, litGen)
        }
        for {
            name <- Gen.oneOf(block_keywords)
            head <- Gen.oneOf(litGen, idGen)
            body <- Gen.containerOf[Seq,Expr](subgenerator)
        } yield Expr.Block(name, Some(head), body)
    }

    property("blocks") = forAll(blockGen(1)) { checkExpr }

    def callGen(depth: Int): Gen[Expr] = {
        val subgenerator = if (depth > 0) {
            Gen.oneOf(idGen, litGen, blockGen(depth - 1), callGen(depth - 1))
        } else {
            Gen.oneOf(idGen, litGen)
        }
        for {
            name <- idGen
            args <- Gen.containerOf[Seq,Expr](subgenerator)
        } yield Expr.Call(name, args)
    }

    property("calling") = forAll(callGen(1)) { checkExpr }

    def opGen(depth: Int): Gen[Expr] = {
        val subgenerator = if (depth > 0) {
            Gen.oneOf(idGen, litGen, callGen(depth - 1), blockGen(depth - 1), opGen(depth - 1))
        } else {
            Gen.oneOf(idGen, litGen)
        }
        for {
            op <- Gen.oneOf(ops.keys)
            lhs <- subgenerator
            rhs <- subgenerator
        } yield Expr.BinOp(op, lhs, rhs)
    }

    property("binops") = forAll(opGen(2)) { checkExpr }
}