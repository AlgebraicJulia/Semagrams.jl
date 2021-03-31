import { add2, mulN2 } from "@thi.ng/vectors";
import { EditorState } from "./EditorState";
import m from "mithril";

interface WireAttrs {
    state: EditorState
    wire_idx: number
}

export const WireNode: m.Component<WireAttrs> = {
    view({ attrs: { state, wire_idx } }) {
        const e = state.lw.wires.wires.get(wire_idx)!;
        const sloc = state.lw.getLoc(e.src)!;
        const tloc = state.lw.getLoc(e.tgt)!;
        const cloc = mulN2([], add2([], sloc, tloc), 1 / 2);
        return m("polyline", {
            points: [sloc, cloc, tloc].map(p => [...p].join(",")).join(" "),
            stroke: "black", "marker-mid": "url(#arrow)"
        });
    }
}
