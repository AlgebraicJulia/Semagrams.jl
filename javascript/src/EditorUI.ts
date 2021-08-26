import { EditorState, ModalState } from './EditorState';
import { DEFAULT_KEYBINDINGS, COMMAND_DOCS } from "./Commands";
import { SVG_HEIGHT } from "./Constants";
import { BoxHandle } from './BoxNode';
import { PortHandle } from './PortNode';
import { map, mapIndexed } from '@thi.ng/transducers';
import m from 'mithril';
import { add2 } from '@thi.ng/vectors';
import { WireHandle } from './WireNode';

/**
 * TODO: These should be runtime-configurable.
 */
const MODAL_TL = [20, 20];
const MODAL_WIDTH = 150;
const MODAL_HEIGHT_PER_LINE = 20;
const MODAL_XPADDING = 10;
const MODAL_YPADDING = 10;

const DocWindow: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        if (state.dialogue.helpwindow) {
            const height = MODAL_HEIGHT_PER_LINE * DEFAULT_KEYBINDINGS.size + MODAL_YPADDING;
            const tl = [MODAL_TL[0], SVG_HEIGHT - height - MODAL_TL[1]];
            const bg = m("rect", {
                x: tl[0],
                y: tl[1],
                width: MODAL_WIDTH,
                height: MODAL_HEIGHT_PER_LINE * DEFAULT_KEYBINDINGS.size + MODAL_YPADDING,
                stroke: "black",
                fill: "white"
            })
            return m("g", {},
                bg,
                ...mapIndexed((i, [key, cmd]) => {
                    const desc = COMMAND_DOCS.get(cmd)!;
                    if (desc != undefined) {
                        let pos = add2([],
                            tl,
                            [MODAL_XPADDING, (i + 1) * MODAL_HEIGHT_PER_LINE]);
                        return m("text", { x: pos[0], y: pos[1] }, `${key}: ${desc.short}`)
                    }
                    return m("text")
                }, DEFAULT_KEYBINDINGS.entries(),
                ))
        } else {
            return m("text", { x: MODAL_TL[0], y: SVG_HEIGHT - MODAL_TL[1] }, "? for help");
        }
    }
}


/**
 * Component for the Modal. Shows up as a rectangle with the choices in it.
 */
const ChoiceModal: m.Component<{ state: EditorState }> = {
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

export const PanHandle: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        return m(
            "rect",
            {
                height: "100%",
                width: "100%",
                x: 0,
                y: 0,
                "fill-opacity": "0",
                "stroke-opacity": "0",
                onmousedown: state.cursor.handlemousedownpan,
                onmouseup: state.cursor.handlemouseuppan,
            })
    }
}

export const EditorHandles: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        const boxhandles = map(box_idx => m(BoxHandle, { state, box_idx, isExport: false }),
            state.boxes());
        const porthandles = map(
            ({ box_idx, port_idx }) => m(PortHandle, { state, box_idx, port_idx, isExport: false }),
            state.ports());
        const wirehandles = map(wire_idx => m(WireHandle, { state, wire_idx, isExport: false }),
            state.wires());
        return m("g",
            ...wirehandles,
            ...boxhandles,
            ...porthandles
        );
    }
}

export const EditorUI: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        return m("g",
            m(ChoiceModal, { state }),
            m(DocWindow, { state }));
    }
}
