import { EditorState } from "./EditorState";
import * as CS from "./ColorScheme";
import m from "mithril";
import { equiv } from "@thi.ng/equiv";
import { wire_entity } from "./Semagram";
import { curvePoints, midpoint, svgPath } from "./ArrowUtils";

interface WireAttrs {
    state: EditorState
    wire_idx: number
}

const WIRE_HANDLE_RADIUS = 7;

export const WireHandle: m.Component<WireAttrs> = {
    view({ attrs: { state, wire_idx } }) {
        const offset = state.ls.getOffset(wire_idx);
        const e = state.ls.sg.wires.get(wire_idx)!;
        const sloc = state.ls.getLoc(e.src)!;
        const tloc = state.ls.getLoc(e.tgt)!;
        const mp = midpoint(sloc, tloc, offset);
        return m("circle", {
            r: WIRE_HANDLE_RADIUS,
            cx: mp[0],
            cy: mp[1],
            "fill-opacity": "0",
            "stroke-opacity": "0",
            "data-w": wire_idx,
            onmouseenter: state.cursor.handlemouseenterwire,
            onmouseout: state.cursor.handlemouseoutwire,
            onmousedown: state.cursor.handlemousedownwire,
            onmouseup: state.cursor.handlemouseupwire,
        })
    }
}

/**
 * Offset is used to compute the curve which allows us to distinguish between
 * multiple wires going between the same pair of attachments.
 */
export const WireNode: m.Component<WireAttrs> = {
    view({ attrs: { state, wire_idx } }) {
        const offset = state.ls.getOffset(wire_idx);
        const e = state.ls.sg.wires.get(wire_idx)!;
        const sloc = state.ls.getLoc(e.src)!;
        const tloc = state.ls.getLoc(e.tgt)!;
        const marker = equiv(state.dialogue.selected, wire_entity(wire_idx))
            ? `url(#arrow-selected)` :
            (equiv(state.cursor.hoveredEntity, wire_entity(wire_idx))
                ? `url(#arrow-hovered)` : `url(#arrow)`);
        return m("path", {
            d: svgPath(curvePoints(sloc, tloc, offset)),
            stroke: CS.accent,
            "marker-mid": marker,
            fill: "none",
            ...state.ls.sg.style_fns[e.ty](e.weights)
        });
    }
}
