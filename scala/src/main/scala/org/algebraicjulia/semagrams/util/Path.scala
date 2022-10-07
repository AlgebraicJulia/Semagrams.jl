package org.algebraicjulia.semagrams.util

object Path {
  enum Element {
    case MoveTo(p: Complex)
    case LineTo(p: Complex)
    case Cubic(cs: Complex, ce: Complex, p: Complex)
    case ClosePath

    def toSvg = {
      this match {
        case MoveTo(p)        => s"M ${p.toSvg}"
        case LineTo(p)        => s"L ${p.toSvg}"
        case Cubic(cs, ce, p) => s"C ${cs.toSvg} ${ce.toSvg} ${p.toSvg}"
        case ClosePath        => "Z"
      }
    }
  }
}

extension (elts: Seq[Path.Element]) {
  def toSvg: String = elts.map(_.toSvg).mkString(" ")
}
