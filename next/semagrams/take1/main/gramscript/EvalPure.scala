package semagrams.gramscript

import Syntax._
import Parser._
import scala.collection.immutable.HashMap
import cats.data.ReaderT
import cats.implicits._
import cats.syntax.applicative._
import cats.Monad

import scala.scalajs.js
import scala.scalajs.js.DynamicImplicits._

object EvalPure {
    type Result[A] = Either[String,A]

    enum Val:
        case Concrete(v: Literal)
        case Closure(v: Seq[Val] => Result[Val])
    
    import Val._

    enum Ty[T]:
        case Int extends Ty[Int]
        case Real extends Ty[Double]
        case Symbol extends Ty[String]
        case String extends Ty[String]
        case Bool extends Ty[Boolean]
        case Function extends Ty[Seq[Val] => Result[Val]]

    def coerceToVal[A](ty: Ty[A], v: A): Val = {
        ty match {
            case Ty.Int => Concrete(Literal.I(v))
            case Ty.Real => Concrete(Literal.R(v))
            case Ty.Symbol => Concrete(Literal.Sym(v))
            case Ty.String => Concrete(Literal.S(v))
            case Ty.Bool => Concrete(Literal.B(v))
            case Ty.Function => Closure(v)
        }
    }

    def coerceFromVal[A](ty: Ty[A], v: Val): Result[A] = {
        ty match {
            case Ty.Int => v match {
                case Concrete(Literal.I(x)) => Some(x)
                case _ => None
            }
            case Ty.Real => v match {
                case Concrete(Literal.R(x)) => Some(x)
                case _ => None
            }
            case Ty.Symbol => v match {
                case Concrete(Literal.Sym(x)) => Some(x)
                case _ => None
            }
            case Ty.String => v match {
                case Concrete(Literal.S(x)) => Some(x)
                case _ => None
            }
            case Ty.Bool => v match {
                case Concrete(Literal.B(x)) => Some(x)
                case _ => None
            }
            case Ty.Function => v match {
                case Closure(x) => Some(x)
                case _ => None
            }
        } match {
            case Some(x) => Right(x.asInstanceOf[A])
            case None => Left(s"$v not of type $ty")
        }
    }

    trait FFIDecl {
        val name: String
        def mkFn()(args: Seq[Val]): Result[Val]
    }

    case class FFIDecl0[X](name: String, ret: Ty[X], fn: () => X) extends FFIDecl {
        def mkFn()(args: Seq[Val]): Result[Val] = {
            if (args.length == 0) {
                Right(coerceToVal(this.ret, this.fn()))
            } else {
                Left(s"${this.name} called with wrong number of arguments")
            }
        }
    }

    case class FFIDecl1[X,A](name: String, ret: Ty[X], arg0: Ty[A], fn: A => X) extends FFIDecl {
        def mkFn()(args: Seq[Val]): Result[Val] = {
            if (args.length == 1) {
                for {
                    arg0v <- coerceFromVal(arg0, args(0))
                } yield coerceToVal(this.ret, this.fn(arg0v))
            } else {
                Left(s"${this.name} called with wrong number of arguments")
            }
        }
    }

    case class FFIDecl2[X,A,B](name: String, ret: Ty[X], arg0: Ty[A], arg1: Ty[B], fn: (A,B) => X) extends FFIDecl {
        def mkFn()(args: Seq[Val]): Result[Val] = {
            if (args.length == 2) {
                for {
                    arg0v <- coerceFromVal(arg0, args(0))
                    arg1v <- coerceFromVal(arg1, args(1))
                } yield coerceToVal(this.ret, this.fn(arg0v, arg1v))
            } else {
                Left(s"${this.name} called with wrong number of arguments")
            }
        }
    }

    type Env = HashMap[Identifier, Val]

    def combineFFIs(ffis: Seq[FFIDecl])(args: Seq[Val]): Result[Val] = {
        val fns = ffis.map(_.mkFn())
        var lasterr = ""
        for (fn <- fns) {
            fn(args) match {
                case Right(x) => return Right(x)
                case Left(err) => {
                    lasterr = err
                }
            }
        }
        return Left(lasterr)
    }

    def generateFFI(ffis: Seq[FFIDecl]): Env = {
        HashMap[Identifier, Val]() ++ ffis.groupBy(_.name).map({ case (i,ffis) => (Identifier(i), Closure(combineFFIs(ffis))) })
    }

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

    val primitives = generateFFI(Seq(
        FFIDecl2("+", Ty.Int, Ty.Int, Ty.Int, _+_),
        FFIDecl2("+", Ty.Real, Ty.Int, Ty.Real, _+_),
        FFIDecl2("+", Ty.Real, Ty.Real, Ty.Int, _+_),
        FFIDecl2("+", Ty.Real, Ty.Real, Ty.Real, _+_),
        FFIDecl2("+", Ty.String, Ty.String, Ty.String, _+_),
        FFIDecl1("concat", Ty.String, Ty.String, s => s)
    ))

    def run(s: String): Result[Val] = for {
        e <- expr.parseAll(s).leftMap(_.toString)
        v <- compile(e).run(primitives)
    } yield v
}