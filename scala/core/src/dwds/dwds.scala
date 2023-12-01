package semagrams.dwds

import semagrams._
import semagrams.util._
import semagrams.rendering._
import semagrams.partprops._

def wireProps[D: PartData](
    tag: SpanTag,
    wireDir: Part => Complex = _ => Complex(0, 0)
)(_e: Part, data: D, m: EntitySeq[D]): PropMap =
  val SpanTag(ctxt, _, apex, (s, tOpt)) = tag
  val propOpt = for
    t <- tOpt
    sc <- m.findCenter(s)
    tc <- tOpt.flatMap(m.findCenter)
    (sspr, sdata) <- m.getTag(s)
    (tspr, tdata) <- m.getTag(t)
    sd = wireDir(s.keyPart)
    td = wireDir(t.keyPart)
  yield (
    PropMap()
      .set(Start, sc)
      .set(End, tc)
      .set(StartDir, sd)
      .set(EndDir, td)
      .set(TikzStart, s.keyPart.tikzName)
      .set(TikzEnd, t.keyPart.tikzName)
  )

  propOpt.getOrElse(PropMap())
