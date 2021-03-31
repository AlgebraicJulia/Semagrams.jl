import { map } from "@thi.ng/transducers";
import { EditorState, ModalState } from "./EditorState";
import { WireNode } from "./WireNode";
import { BoxNode } from "./BoxNode";
import { add2 } from "@thi.ng/vectors";
import m from "mithril";

function makeMarker(id: string, color: string) {
    return m("marker",
        {
            id: id,
            viewBox: "0 0 10 10",
            refX: "5",
            refY: "5",
            markerWidth: "12",
            markerHeight: "12",
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

const grid = m("pattern", {
    id: "grid",
    width: "100",
    height: "100",
    patternUnits: "userSpaceOnUse",
},
    m("path", {
        d: "M 100,0 L 0,0 L 0,100",
        fill: "none",
        stroke: "gray",
        "stroke-width": "1"
    }))

const svgdefs = m("defs", grid, makeMarker("arrow-sel", "lightgrey"), makeMarker("arrow", "white"));

const MODAL_TL = [20, 20];
const MODAL_WIDTH = 150;
const MODAL_HEIGHT_PER_LINE = 20;
const MODAL_XPADDING = 10;
const MODAL_YPADDING = 10;

const ChoiceModal: m.Component<EditorAttrs> = {
    view({ attrs: { state } }) {
        if (state.modal.ty != ModalState.Normal) {
            const bg = m("rect", {
                x: MODAL_TL[0],
                y: MODAL_TL[1],
                width: MODAL_WIDTH,
                height: MODAL_HEIGHT_PER_LINE * state.modal.choices.length + MODAL_YPADDING,
                stroke: "black",
                fill: "white"
            });
            return m("g", {},
                bg,
                ...state.modal.choices.map((choice, i) => {
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

export const Editor: m.Component<EditorAttrs> = {
    oncreate({ dom, attrs: { state } }) {
        state.svgelt = dom as SVGSVGElement;
    },

    view({ attrs: { state } }) {
        const boxnodes = [...map(box_idx => m(BoxNode, { state, box_idx }), state.lw.wires.boxes.keys())];
        const wirenodes = map(wire_idx => m(WireNode, { state, wire_idx }),
            state.lw.wires.wires.keys());
        return m("svg", {
            width: "95%",
            height: "500px",
            onmousemove: state.handlemousemove,
            onkeydown: state.handlekeydown,
            tabindex: "0",
            style: { "border-style": "solid", "stroke-width": "2px" },
        },
            svgdefs,
            m("rect", { width: "100%", height: "100%", fill: "url(#grid)" }),
            m(ChoiceModal, { state }),
            ...wirenodes,
            ...boxnodes,
        );
    }
}
