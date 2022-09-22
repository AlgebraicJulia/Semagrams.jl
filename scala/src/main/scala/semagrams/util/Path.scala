package semagrams.util

object Path {
  enum Element {
    case MoveTo(p: Complex)
    case LineTo(p: Complex)
    case Cubic(cs: Complex, ce: Complex, p: Complex)
    case ClosePath
  }
}
