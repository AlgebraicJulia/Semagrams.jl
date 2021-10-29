package semagrams.gramscript

import Syntax._
import Parser._
import scala.collection.immutable.HashMap
import cats.data.ReaderT
import cats.implicits._
import cats.syntax.applicative._
import cats.Monad

object EvalPure {
    type Result[A] = Either[String,A]

    enum Val:
        case Concrete(v: Literal)
        case Closure(v: Seq[Val] => Result[Val])
    
    import Val._

    type Env = HashMap[Identifier, Val]

    type F[A] = ReaderT[Result, Env, A]

    def const[A](x: Result[A]): F[A] = ReaderT[Result, Env, A](_ => x)
    def get(): F[Env] = ReaderT[Result, Env, Env](Right(_))

    def lookup(i: Identifier): F[Val] = for {
        env <- get()
        v <- const(env.get(i).toRight(s"Could not find identifier $i"))
    } yield v

    def splitLast[A](xs: Seq[A]): Option[(Seq[A],A)] = {
        if (xs.isEmpty) {
            None
        } else {
            Some((xs.dropRight(1), xs.last))
        }
    }

    def compile(e: Expr): F[Val] = {
        e match {
            case Expr.Lit(l) => Concrete(l).pure
            case Expr.Id(i) => lookup(i)
            case Expr.Call(f, args) => for {
                fv <- compile(f)
                closure <- const(fv match {
                    case Concrete(_) => Left("tried to call a non-function")
                    case Closure(c) => Right(c)
                })
                argsv <- args.traverse(compile)
                result <- const(closure(argsv))
            } yield result
            case Expr.Block("let", None, lines) => for {
                split <- const(splitLast(lines).toRight("must be at least one line in let statement"))
                bindings <- const(split._1.traverse(b => b match {
                    case Expr.Special("=", Seq(Expr.Id(i), rhs)) => Right((i, compile(rhs)))
                    case _ => Left("All lines except for last in let statment must be binding")
                }))
                env <- get()
                newenv <- const(bindings.foldM(env)({ case (env, (i, rhs)) => rhs.run(env).map(v => env + ((i, v))) }))
                v <- const(compile(split._2).run(newenv))
            } yield v
            case Expr.Special("->", Seq(Expr.Id(i), e)) => for {
                env <- get()
                compiled = compile(e)
            } yield (Closure(v => compiled.run(env + ((i,v.head)))))
            case _ => const(Left("unrecognized block or special"))
        }
    }

    val primitives = HashMap[Identifier, Val](
        (Identifier("+"), Closure(args => args match {
            case Seq(Concrete(Literal.I(x)), Concrete(Literal.I(y))) => Right(Concrete(Literal.I(x + y)))
            case _ => Left("unsupported types for addition")
         }))
    )

    def run(s: String): Result[Val] = for {
        e <- expr.parseAll(s).leftMap(_.toString)
        v <- compile(e).run(primitives)
    } yield v
}