import { add2, mulN2 } from "@thi.ng/vectors";
import { EditorContext } from "./EditorState";
import * as svg from "@thi.ng/hiccup-svg";

export function wirenode({ state }: EditorContext, wire_idx: number) {
    const e = state.lw.wires.wires.get(wire_idx)!;
    const sloc = state.lw.getLoc(e.src)!;
    const tloc = state.lw.getLoc(e.tgt)!;
    const cloc = mulN2([], add2([], sloc, tloc), 1 / 2);
    return svg.polyline([sloc, cloc, tloc], { stroke: "black", "marker-mid": "url(#arrow)" });
}
