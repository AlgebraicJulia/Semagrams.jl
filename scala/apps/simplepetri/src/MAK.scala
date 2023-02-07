package semagrams.simplepetri

import semagrams.api._
import semagrams.acsets.{_, given}
import Petris._
import com.raquo.laminar.api.L._

import org.scalajs.dom
import scala.scalajs.js
import js.annotation._

case class Variable(name: String)

case class MassActionEquation(
  lhs: Variable,
  rhs: Seq[(Int, Variable, Seq[(Variable, Int)])]
) {
  def toLatex() = {
    val rhsexpr = rhs.zipWithIndex.foldLeft("")(
      {
        case (expr, ((coeff, rate, factors), idx)) => {
          val factorsexpr = factors
            .map({
              case (pop, 1) => s"${pop.name}"
              case (pop, 0) => ""
              case (pop, i) =>  s"${pop.name}^{${i}}"
            }).mkString("")
          val prefix = coeff match {
            case 1 => if (idx == 0) "" else " + "
            case -1 => " - "
            case i if i > 1 => s" + ${i}"
            case i if i < -1 => s" - ${i.abs}"
          }
          s"${expr} ${prefix} ${rate.name} \\; ${factorsexpr}"
        }
      }
    )
    s"\\dot{${lhs.name}} &= ${if (rhsexpr == "") "0" else rhsexpr}"
  }
}

def escapeUnderscores(s: String) = s.replaceAll("_","\\\\_")

def variable(petri: ACSet, s: Part, default: String): Variable = {
  petri.trySubpart(Content, s) match {
    case Some("") | None => Variable(s"${default}_{${s.path(0)._2.id + 1}}")
    case Some(name) if name.length > 1 => Variable(s"\\mathrm{${escapeUnderscores(name)}}")
    case Some(name) => Variable(escapeUnderscores(name))
  }
}

def massActionEquations(petri: ACSet): String = {
  val ts = petri.partsOnly(ROOT, T).sortBy(_.path(0)._2.id)
  val ss = petri.partsOnly(ROOT, S).sortBy(_.path(0)._2.id)
  val inputMatrix = ts.map(
    t => (t -> ss.map(
      s => (s -> petri.incident(s, IS).filter(petri.trySubpart(IT,_) == Some(t)).length)
    ).toMap)
  ).toMap
  val outputMatrix = ts.map(
    t => (t -> ss.map(
      s => (s -> petri.incident(s, OS).filter(petri.trySubpart(OT,_) == Some(t)).length)
    ).toMap)
  ).toMap
  val eqs = ss.map(
    s => MassActionEquation(
      variable(petri, s, "C"),
      ts.map(t => {
        val (i,o) = (inputMatrix(t)(s), outputMatrix(t)(s))
        (
          o - i,
          variable(petri, t, "r"),
          ss.map(s1 => {
            (variable(petri, s1, "C"), inputMatrix(t)(s1))
          }).filter(_._2 != 0)
        )
      }).filter(_._1 != 0)
    )
  )
  s"\\begin{align*} ${eqs.map(_.toLatex()).mkString(" \\\\ \n")} \\end{align*}"
}

@js.native
@JSImport("katex", JSImport.Default)
object katex extends js.Object {
  def render(eq: String, elt: dom.Element, options: js.Dictionary[Any]): Unit = js.native
}

def massActionTypeset($petri: Signal[ACSet]): Element = {
  def render(petri: ACSet) = {
    val elt = div()
    katex.render(
      massActionEquations(petri),
      elt.ref,
      js.Dictionary("displayMode" -> true)
    )
    elt
  }
  div(
    child <-- $petri.map(render)
  )
}
