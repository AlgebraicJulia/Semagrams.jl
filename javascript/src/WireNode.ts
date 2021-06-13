import { add2, dist2, mulN2, rotate, sub2, Vec, Vec2Like, ZERO2 } from "@thi.ng/vectors";
import { EditorState } from "./EditorState";
import * as CS from "./ColorScheme";
import m from "mithril";

const WIREOFFSETCONSTANT = 60;
const WIRE_HANDLE_RADIUS = 7;

interface WireAttrs {
    state: EditorState
    wire_idx: number
}

/**
 * This stringifies our representation of SVG paths.
 * This should probably be factored out of this file, into `SVGUtils.ts` file
 * or something.
 */
function svgPath(segs: Array<[string, Array<Vec>]>) {
    return segs.map(seg => {
        const pts = seg[1].map(p => [...p].join(",")).join(" ");
        return `${seg[0]} ${pts}`
    }).join(" ");
}

/**
 * This computes the SVG spec for the points of the spline, so that
 * when we have multiple wires going between two attachments, they can curve away from each other.
 */
function curvePoints(p1: Vec2Like, p2: Vec2Like, offset: number): Array<[string, Array<Vec>]> {
    offset = WIREOFFSETCONSTANT * offset;
    const c = mulN2([], add2([], p1, p2), 1 / 2);
    const c1 = mulN2([], add2([], p1, c), 1 / 2);
    const c2 = mulN2([], add2([], c, p2), 1 / 2);
    const dp = sub2([], p2, p1);
    const dphat = mulN2([], dp, 1 / dist2(dp, ZERO2));
    const dqhat = rotate([], dphat, Math.PI / 2);
    const c1up = add2([], c1, mulN2([], dqhat, offset));
    const c2up = add2([], c2, mulN2([], dqhat, offset));
    const cup = add2([], c, mulN2([], dqhat, offset));
    return [
        ["M", [p1]],
        ["C", [c1up, c1up, cup]],
        ["C", [c2up, c2up, p2]]
    ];
}

function midpoint(p1: Vec2Like, p2: Vec2Like, offset: number): Vec {
    offset = WIREOFFSETCONSTANT * offset;
    const c = mulN2([], add2([], p1, p2), 1 / 2);
    const dp = sub2([], p2, p1);
    const dphat = mulN2([], dp, 1 / dist2(dp, ZERO2));
    const dqhat = rotate([], dphat, Math.PI / 2);
    const cup = add2([], c, mulN2([], dqhat, offset));
    return cup;
}

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
            onmouseout: state.cursor.handlemouseoutwire
        })
    }
}

/**
 * The most important parameter here is "offset"; otherwise this is similar to BoxNode or PortNode.
 * Offset is calculated for all of the wires at once, which is why it is passed in
 * rather than computed here.
 * Offset is used to compute the curve which allows us to distinguish between
 * multiple wires going between the same pair of attachments.
 *
 * TODO: Maybe the offsets should be a field of the LocatedSemagram, and recomputed
 * everytime wires get added/deleted, rather than computed in the render loop of
 * Editor?
 */
export const WireNode: m.Component<WireAttrs> = {
    view({ attrs: { state, wire_idx } }) {
        const offset = state.ls.getOffset(wire_idx);
        const e = state.ls.sg.wires.get(wire_idx)!;
        const sloc = state.ls.getLoc(e.src)!;
        const tloc = state.ls.getLoc(e.tgt)!;
        const marker = state.cursor.hoveredWire == wire_idx ? `url(#arrow-hovered)` : `url(#arrow)`;
        return m("path", {
            d: svgPath(curvePoints(sloc, tloc, offset)),
            stroke: CS.accent,
            "marker-mid": marker,
            fill: "none"
        });
    }
}
