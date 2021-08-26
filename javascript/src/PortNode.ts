import m from "mithril";
import { colorAttachment, EditorState } from "./EditorState";
import { port_entity } from "./Semagram";
import * as CS from "./ColorScheme";
import { equiv } from "@thi.ng/equiv";

const PORTRADIUS = 7;

interface PortAttrs {
    state: EditorState
    box_idx: number
    port_idx: number
    isExport: boolean
}

export const PortHandle: m.Component<PortAttrs> = {
    view({ attrs: { state, box_idx, port_idx } }) {
        const a = port_entity(box_idx, port_idx);
        const loc = state.ls.getLoc(a)!;
        const attrs = {
            "fill-opacity": "0",
            "stroke-opacity": "0",
            "data-a": JSON.stringify(a),
            onmouseenter: state.cursor.handlemouseenterattachment,
            onmouseout: state.cursor.handlemouseoutattachment,
            onmousedown: state.cursor.handlemousedownport,
            onmouseup: state.cursor.handlemouseupport,
        };
        return m("circle", {
            r: PORTRADIUS,
            cx: loc[0],
            cy: loc[1],
            ...attrs
        });
    }
}

/**
 * See the comment for BoxNode.
 */
export const PortNode: m.Component<PortAttrs> = {
    view({ attrs: { state, box_idx, port_idx, isExport } }) {
        const a = port_entity(box_idx, port_idx);
        const loc = state.ls.getLoc(a)!;
        const port = state.ls.sg.getPort(box_idx, port_idx)!;
        const attrs = {
            fill: colorAttachment(state, a),
            stroke: CS.accent,
            ...state.ls.sg.style_fns[port.ty](port.weights)
        };
        const portnode = m("circle", {
            r: PORTRADIUS,
            cx: loc[0],
            cy: loc[1],
            ...attrs
        });
        const highlight = !isExport ? m("circle", {
            fill: equiv(state.dialogue.selected, a) ? "yellow" : "none",
            stroke: "none",
            r: PORTRADIUS * 1.5,
            cx: loc[0],
            cy: loc[1],
        }) : m("g");
        return m("g", highlight, portnode);
    }
}
