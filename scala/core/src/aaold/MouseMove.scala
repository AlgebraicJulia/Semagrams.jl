// package semagrams.state

// import semagrams.util.Complex

// import com.raquo.laminar.api.L._
// import org.scalajs.dom
// import org.scalajs.dom.SVGSVGElement


// def mouseMoveListener(eventWriter: Observer[Event]) = inContext(
//   svg => onMouseMove.map(
//     evt => Event.MouseMove(svgCoords(svg.ref.asInstanceOf[SVGSVGElement], evt)))
//       --> eventWriter
// )
