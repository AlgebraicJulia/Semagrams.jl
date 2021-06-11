import { map } from "@thi.ng/transducers";
import { EditorState, ModalState } from "./EditorState";
import { WireNode } from "./WireNode";
import { BoxNode } from "./BoxNode";
import { add2, hash } from "@thi.ng/vectors";
import m from "mithril";
import { HashMap } from "@thi.ng/associative";
import { hashAttachment } from "./Semagram";
import { centerIndex } from "./Util";


/**
 * Markers for the middle of wires.
 * TODO: Markers are kind of restrictive... Maybe just put the shape in the middle of the wire manually?
 */
function makeMarker(id: string, color: string) {
    return m("marker",
        {
            id: id,
            viewBox: "0 0 10 10",
            refX: "5",
            refY: "5",
            markerWidth: "6",
            markerHeight: "6",
            orient: "auto",
        },
        m("path", {
            d: "M 0,0 L 10,5 L 0, 10 Z",
            style: "stroke-width:0.5px",
            fill: color,
            stroke: "black",
        })
    )
}

const resistorMarker =
    m("marker",
        {
            id: "resistor",
            viewBox: "0 0 82.5 30",
            refX: "45",
            refY: "15",
            markerWidth: "30",
            markerHeight: "10",
            orient: "auto",
        },
        m.trust("<g transform=\"translate(0 15)\"><rect width=\"90\" height=\"30\" x=\"0\" y=\"-15\" style=\"fill:black;stroke:none\"/><polyline points=\"0.0,0.0 15.0,0.0 22.5,-15.0 30.0,15.0 37.5,-15.0 45.0,15.0 52.5,-15.0 60.0,15.0 67.5,0.0 82.5,0.0\" style=\"stroke-width:2px;stroke:white;fill:none\"></polyline></g>")
    );

const capacitorLineStyle = "stroke-width:3px;stroke:white;fill:none"

const capacitorMarker =
    m("marker",
        {
            id: "capacitor",
            viewBox: "0 0 60 60",
            refX: "30",
            refY: "30",
            markerWidth: "20",
            markerHeight: "20",
            orient: "auto"
        },
        m("g", { "transform": "translate(30 30)" },
            m("rect", { width: "60", height: "60", x: "-30", y: "-30", style: "stroke:none;fill:black" }),
            m("polyline", { points: "-30,0 -10,0", style: capacitorLineStyle }),
            m("polyline", { points: "30,0 10,0", style: capacitorLineStyle }),
            m("polyline", { points: "-10,30 -10,-30", style: capacitorLineStyle }),
            m("polyline", { points: "10,30 10,-30", style: capacitorLineStyle })
        )
    )

const grid = m("pattern", {
    id: "grid",
    width: "60",
    height: "60",
    patternUnits: "userSpaceOnUse",
},
    m("path", {
        d: "M 60,0 L 0,0 L 0,60",
        fill: "none",
        stroke: "gray",
        "stroke-width": "1"
    }))

const svgdefs = m("defs", grid, resistorMarker, capacitorMarker, makeMarker("arrow-sel", "lightgrey"), makeMarker("arrow", "white"));

/**
 * TODO: These should be runtime-configurable.
 */
const MODAL_TL = [20, 20];
const MODAL_WIDTH = 150;
const MODAL_HEIGHT_PER_LINE = 20;
const MODAL_XPADDING = 10;
const MODAL_YPADDING = 10;

/**
 * Component for the Modal. Shows up as a rectangle with the choices in it.
 */
const ChoiceModal: m.Component<EditorAttrs> = {
    view({ attrs: { state } }) {
        const modal = state.dialogue.modal;
        if (modal.ty != ModalState.Normal) {
            const bg = m("rect", {
                x: MODAL_TL[0],
                y: MODAL_TL[1],
                width: MODAL_WIDTH,
                height: MODAL_HEIGHT_PER_LINE * modal.choices.length + MODAL_YPADDING,
                stroke: "black",
                fill: "white"
            });
            return m("g", {},
                bg,
                ...modal.choices.map((choice, i) => {
                    let pos = add2([],
                        MODAL_TL,
                        [MODAL_XPADDING, (i + 1) * MODAL_HEIGHT_PER_LINE])
                    return m("text", { x: pos[0], y: pos[1] }, `${i + 1}: ${choice}`)
                })
            );
        } else {
            return m("g");
        }
    }
}

interface EditorAttrs {
    state: EditorState
}

const globalStyle = `
.katex { font-size: 1.5em; }
`;

/**
 * This is the root component for all the UI.
 * It has children of
 * - All boxes/wires/ports
 * - the modal
 *
 * TODO: This should be refactored so that there is an "EditorPane",
 * which displays the Semagram, and a "EditorUI", which has UI elements on top of
 * the EditorPane.
 */
export const Editor: m.Component<EditorAttrs> = {
    oncreate({ dom, attrs: { state } }) {
        state.cursor.svgelt = dom as SVGSVGElement;
    },

    view({ attrs: { state } }) {
        const boxnodes = [...map(box_idx => m(BoxNode, { state, box_idx }), state.boxes())];
        const wires_by_src_tgt =
            new HashMap<[number, number], [number, boolean][]>(null, { hash });
        for (const wire_idx of state.wires()) {
            const w = state.ls.sg.getWire(wire_idx)!;
            const s = hashAttachment(w.src);
            const t = hashAttachment(w.tgt);
            var key: [number, number];
            var val: [number, boolean];
            if (s <= t) {
                key = [s, t];
                val = [wire_idx, true];
            } else {
                key = [t, s];
                val = [wire_idx, false];
            }
            var wlist;
            if (wlist = wires_by_src_tgt.get(key)) {
                wlist.push(val);
            } else {
                wires_by_src_tgt.set(key, [val]);
            }
        }
        const wirenodes = map((wlist) =>
            wlist.map((w, i) => {
                const offset = (w[1] ? 1 : -1) * centerIndex(i, wlist.length);
                return m(WireNode, { state, wire_idx: w[0], offset })
            }),
            wires_by_src_tgt.values());
        // const wirenodes = map(wire_idx => m(WireNode, { state, wire_idx }),
        // state.lw.wireviz.wires.keys());
        return m("svg", {
            width: "95%",
            height: "500px",
            onmousemove: state.cursor.handlemousemove,
            onkeydown: state.handlekeydown,
            tabindex: "0",
            style: { "border-style": "solid", "stroke-width": "2px" },
        },
            m("style", m.trust(globalStyle)),
            svgdefs,
            m("rect", { width: "100%", height: "100%", fill: "url(#grid)" }),
            m(ChoiceModal, { state }),
            ...wirenodes,
            ...boxnodes,
        );
    }
}
