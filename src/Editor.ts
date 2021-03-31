import * as svg from "@thi.ng/hiccup-svg";
import { map } from "@thi.ng/transducers";
import { ILifecycle } from "@thi.ng/hdom";
import { EditorContext, ModalState } from "./EditorState";
import { wirenode } from "./WireNode";
import { boxnode } from "./BoxNode";
import { add2 } from "@thi.ng/vectors";

function makeMarker(id: string, color: string) {
    return ["marker",
        {
            id: id,
            viewBox: "0 0 10 10",
            refX: "5",
            refY: "5",
            markerWidth: "12",
            markerHeight: "12",
            orient: "auto",
        },
        svg.path([["M", [0, 0]], ["L", [10, 5]], ["L", [0, 10]], ["Z"]], {
            style: "stroke-width:0.5px",
            fill: color,
            stroke: "black",
        })
    ]
}

const grid = ["pattern", {
    id: "grid",
    width: "100",
    height: "100",
    patternUnits: "userSpaceOnUse",
},
    svg.path([["M", [100, 0]], ["L", [0, 0]], ["L", [0, 100]]], {
        fill: "none",
        stroke: "gray",
        "stroke-width": "1"
    })]

const svgdefs = svg.defs(grid, makeMarker("arrow-sel", "lightgrey"), makeMarker("arrow", "white"));

const MODAL_TL = [20, 20];
const MODAL_WIDTH = 150;
const MODAL_HEIGHT_PER_LINE = 20;
const MODAL_XPADDING = 10;
const MODAL_YPADDING = 10;

function choiceModal({ state }: EditorContext) {
    if (state.modal.ty != ModalState.Normal) {
        const bg = svg.rect(
            MODAL_TL,
            MODAL_WIDTH,
            MODAL_HEIGHT_PER_LINE * state.modal.choices.length + MODAL_YPADDING,
            { stroke: "black", fill: "white" }
        )
        return svg.group({},
            bg,
            ...state.modal.choices.map((choice, i) =>
                svg.text(add2([],
                    MODAL_TL,
                    [MODAL_XPADDING, (i + 1) * MODAL_HEIGHT_PER_LINE]),
                    `${i + 1}: ${choice}`))
        );
    }
}

export class Editor implements ILifecycle {
    init(el: Element, { state }: EditorContext) {
        state.svgelt = el as SVGSVGElement;
    }

    render({ state }: EditorContext) {
        const boxnodes = [...map(box_idx => boxnode({ state }, box_idx), state.lw.wires.boxes.keys())];
        return svg.svg({
            width: "95%",
            height: "500px",
            onmousemove: state.handlemousemove,
            onkeydown: state.handlekeydown,
            tabindex: "0",
            style: { "border-style": "solid", "stroke-width": "2px" },
        },
            svgdefs,
            ["rect", { width: "100%", height: "100%", fill: "url(#grid)" }],
            choiceModal,
            ...map(wire_idx => [wirenode, wire_idx], state.lw.wires.wires.keys()),
            ...boxnodes,
        );
    }
}
