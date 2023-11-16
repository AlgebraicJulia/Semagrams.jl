package semagrams.dwds

import semagrams._
import semagrams.acsets._
import semagrams.util._
import semagrams.rendering._
// import semagrams.{PropMap, PartProp}

def wireProps[D:PartData](
  e:Part,
  src: PartProp,
  tgt: PartProp,
  wireDir: Part => Complex = _ => Complex(0, 0),
)(_e: Part, data: D, m: EntityMap[D]): PropMap = 


  val p = data.getProps()

  val propOpt = for
    s <- p.get(src)
    t <- p.get(tgt)
    sc <- m.findCenter(s)
    tc <- m.findCenter(t)
    (sspr,sdata) <- m.get(s)
    (tspr,tdata) <- m.get(t)
    sd = wireDir(s)
    td = wireDir(t)
  yield (
      PropMap()
        .set(Start, sc)
        .set(End, tc)
        .set(StartDir, sd)
        .set(EndDir, td)
        .set(TikzStart, s.tikzName)
        .set(TikzEnd, t.tikzName)  
    )
    
  propOpt.getOrElse(PropMap())
  
  