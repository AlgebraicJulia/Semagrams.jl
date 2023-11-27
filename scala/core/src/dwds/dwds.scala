package semagrams.dwds

import semagrams._
import semagrams.util._
import semagrams.rendering._

def wireProps[D:PartData](
  tag:SpanTag,
  wireDir: Part => Complex = _ => Complex(0, 0),
)(_e: Part, data: D, m: EntitySeq[D]): PropMap = 
  val SpanTag(ctxt,Span(src,tgt),apex,(s,tOpt)) = tag


  val propOpt = for
    t <- tOpt
    sc <- m.findCenter(s)
    tc <- tOpt.flatMap(m.findCenter)
    (sspr,sdata) <- m.getPart(s,ctxt)
    (tspr,tdata) <- m.getPart(t,ctxt)
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
  
  