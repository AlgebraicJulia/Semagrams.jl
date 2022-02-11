package semagrams.gramscript

import scala.collection.mutable
import cats.instances.try_

object Syntax {
    /* We may at some point upgrade this to support dot paths, like a.b, and also machine-generated names */
    case class Identifier(label: String)

    enum Expr:
        case Block(name: String, head: Option[Expr], body: Seq[Expr])
        case Call(head: Expr, args: Seq[Expr])
        case Special(name: String, args: Seq[Expr])
        case Lit(value: Literal)
        case Id(name: Identifier)

        def toInput(b: mutable.StringBuilder, indent: Int): Unit = this match {
            case Block(name, head, body) => {
                b ++= name
                b ++= " "
                head.map(_.toInput(b, indent))
                b ++= "\n"
                for (e <- body) {
                    b ++= " " * (indent+2)
                    e.toInput(b, indent+2)
                    b ++= "\n"
                }
                b ++= " " * indent
                b ++= "end"
            }
            case Call(head, args) => {
                head.toInput(b,indent)
                b += '('
                for (e <- args.dropRight(1)) {
                    e.toInput(b,indent)
                    b ++= ","
                }
                if (args.length > 0) {
                    args.last.toInput(b, indent)
                }
                b += ')'
            }
            case Special(head, args) => {
                b ++= head
                b += '('
                for (e <- args.dropRight(1)) {
                    e.toInput(b,indent)
                    b ++= ","
                }
                if (args.length > 0) {
                    args.last.toInput(b, indent)
                }
                b += ')'
            }
            case Lit(value) => value.toInput(b)
            case Id(name) => b ++= name.label
        }

        def toInput: String = {
            val b = mutable.StringBuilder()
            this.toInput(b, 0)
            b.toString
        }
    
    def mkId(label: String) = Expr.Id(Identifier(label))

    enum Literal:
        case I(v: Int)
        case R(v: Double)
        case S(v: String)
        case Sym(v: String)
        case B(v: Boolean)

        def toInput(b: mutable.StringBuilder): Unit = this match {
            case I(v) => b ++= v.toString
            case R(v) => b ++= f"$v%.20f"
            case Sym(v) => b ++= ":" + v
            case B(v) => b ++= v.toString
            case S(v) => "\"" + v + "\""
        }

    val block_keywords = Seq(
        "function",
        "view", "query", "mutate",
        "schema",
        "type",
        "let"
    );

    val reserved = block_keywords ++ Seq("end")

    val special_ops = Seq("=", "::", "->")

}