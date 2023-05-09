package semagrams.util

import semagrams.util.Complex
import semagrams.acsets.Part
import semagrams.acsets.ROOT


def tikzNode(shape:String,nodename:String,ctr:Complex,label:String="",draw:Boolean=true): String =
  s"\\node[${shape}${if draw then ",draw" else ""},align=center] (${nodename}) at (${ctr.x},${-ctr.y}) {$label};\n"


def tikzLabel(label:String,size:String="") = label.split("\n")
  .map(line => if size=="" then line else "\\" + size + " " + line)
  .mkString("\\\\")


def tikzInPorts(pts:Seq[Part],shape:String = ""): String = if pts.isEmpty | pts.head == ROOT
  then ""
  else tikzPorts(
    pts,
    s"${pts.head.init.tikzName}.north west",
    s"${pts.head.init.tikzName}.south west",
    shape
  )

def tikzOutPorts(pts:Seq[Part],shape:String = ""): String = if pts.isEmpty | pts.head == ROOT
  then ""
  else tikzPorts(
    pts,
    s"${pts.head.init.tikzName}.north east",
    s"${pts.head.init.tikzName}.south east",
    shape
  )

def tikzPorts(pts:Seq[Part],start:String,stop:String,shape:String = ""): String = 
  s"\\path ($start) to\n"
    + pts.zipWithIndex.map((p,idx) =>
      "\t" + tikzPort(p,idx,pts.length,shape)  
    ).mkString("")
    + s"($stop);\n"


def tikzPort(p:Part,idx:Int,n_ports:Int,shape:String = "") =
  val pos = (1.0 + 2 * idx) / (2 * n_ports)

  if shape == ""
  then s"node[pos=$pos](${p.tikzName}){}\n"
  else s"node[pos=$pos,draw,$shape](${p.tikzName}){}\n"

  // s"\\path (${p.init.tikzName}.north west) -- $nodeString (${p.init.tikzName}.south west);\n"
  
