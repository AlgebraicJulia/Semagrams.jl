package semagrams.gramscript

import Syntax._

object Associativities {
    enum Associativity:
        case LAssoc(p: Int)
        case RAssoc(p: Int)
        case NoAssoc(p: Int)

        def prec() = this match {
            case LAssoc(p) => p
            case RAssoc(p) => p
            case NoAssoc(p) => p
        }

    import Associativity._

    implicit val cmpAssociativities: PartialOrdering[Associativity] = new PartialOrdering[Associativity] {

        def lteq(a: Associativity, b: Associativity) = {
            tryCompare(a,b) == Some(-1)
        }

        def tryCompare(a: Associativity, b: Associativity) = {
            if (a.prec() < b.prec()) {
                Some(-1)
            } else if (a.prec() == b.prec()) {
                (a,b) match {
                    case (LAssoc(_), LAssoc(_)) => Some(1)
                    case (RAssoc(_), RAssoc(_)) => Some(-1)
                    case _ => None
                }
            } else {
                Some(1)
            }
        }
    }


    val precedences = Map("=" -> NoAssoc(10),
                          "::" -> NoAssoc(20),
                          "->" -> RAssoc(30),
                          "&&" -> LAssoc(40), "||" -> LAssoc(40),
                          "+" -> LAssoc(50), "-" -> LAssoc(50),
                          "*" -> LAssoc(60), "/" -> LAssoc(60),
                          "^" -> NoAssoc(70))

    case class FlatExpr(head: Expr, tail: Option[OpExpr])

    case class OpExpr(op: String, rest: FlatExpr)

    def largestIdx[A](xs: List[A])(implicit ord: PartialOrdering[A]): Option[Int] = {
        var largest = xs.head
        var idx = 0
        for ((x,i) <- xs.tail.zipWithIndex) {
            ord.tryCompare(x, largest) match {
                case Some(v) => {
                    if (v > 0) {
                        largest = x
                        idx = i+1
                    }
                }
                case None => return None
            }
        }
        return Some(idx)
    }

    def mergeAt(idx: Int, first: Expr, rest: List[(String, Expr)]): (Expr, List[(String, Expr)]) = {
        if (idx == 0) {
            (merge(rest.head._1, first, rest.head._2), rest.tail)
        } else {
            val res = mergeAt(idx - 1, rest.head._2, rest.tail)
            (first, (rest.head._1, res._1)::res._2)
        }
    }

    def merge(op: String, e1: Expr, e2: Expr) = {
        if (special_ops.contains(op)) {
            Expr.Special(op, Seq(e1, e2))
        } else {
            Expr.Call(mkId(op), Seq(e1, e2))
        }
    }

    def associate(first: Expr, rest: List[(String, Expr)]): Option[Expr] = {
        if (rest.isEmpty) {
            Some(first)
        } else {
            largestIdx(rest.map({ case (op,_) => precedences(op) })).flatMap(
                nextMerge => {
                    val next = mergeAt(nextMerge, first, rest)
                    associate(next._1, next._2)
                }
            )
        }
    }
}
