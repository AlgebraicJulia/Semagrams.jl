package semagrams.util

import semagrams.util.Complex


import semagrams.Part

/** Create the declaration of a Tikz node. */
def tikzNode(
    shape: String,
    nodename: String,
    ctr: Complex,
    label: String = "",
    draw: Boolean = true
): String =
  s"\\node[${shape}${
      if draw then ",draw" else ""
    },align=center] (${nodename}) at (${ctr.x},${-ctr.y}) {$label};\n"

/** Convert strings to Tikz labels by converting "\n" to "\\" */
def tikzLabel(label: String, size: String = "") = label
  .split("\n")
  .map(line => if size == "" then line else "\\" + size + " " + line)
  .mkString("\\\\")

/** Create the declarations for a sequence of nodes along a path from `start` to
  * `stop`. Include a `shape` string to make the nodes visible.
  */
def tikzPorts(
    pts: Seq[Part],
    start: String,
    stop: String,
    shape: String = ""
): String =
  s"\\path ($start) to\n"
    + pts.zipWithIndex
      .map((p, idx) => "\t" + tikzPort(p, idx, pts.length, shape))
      .mkString("")
    + s"($stop);\n"

/** Create the declaration for a single node along a path. */
def tikzPort(p: Part, idx: Int, n_ports: Int, shape: String = "") =
  val pos = (1.0 + 2 * idx) / (2 * n_ports)

  if shape == ""
  then s"node[pos=$pos](${p.tikzName}){}\n"
  else s"node[pos=$pos,draw,$shape](${p.tikzName}){}\n"

/** Create the declarations for a sequence of input ports. Include a `shape`
  * string to make the ports visible.
  */
def tikzInPorts(pts: Seq[Part], shape: String = ""): String =
  if pts.isEmpty
  then ""
  else
    tikzPorts(
      pts,
      s"${pts.head.tikzName}.north west",
      s"${pts.head.tikzName}.south west",
      shape
    )

/** Create the declarations for a sequence of input ports. Include a `shape`
  * string to make the ports visible.
  */
def tikzOutPorts(pts: Seq[Part], shape: String = ""): String =
  if pts.isEmpty
  then ""
  else
    tikzPorts(
      pts,
      s"${pts.head.tikzName}.north east",
      s"${pts.head.tikzName}.south east",
      shape
    )

  /** Wrap tikz declarations in the surrounding LaTeX */
def tikzWrapper(inside: String) = s"""
\\documentclass[convert]{standalone}
\\usepackage{tikz}
\\begin{document}
\\begin{tikzpicture}
\\draw (0,0) rectangle (10,-10);

$inside

\\end{tikzpicture}
\\end{document}
"""
