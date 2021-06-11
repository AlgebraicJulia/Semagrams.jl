import m from "mithril";
import { colorAttachment, EditorState } from "./EditorState";
import { port_attach } from "./Semagram";
import * as CS from "./ColorScheme";

const PORTRADIUS = 7;

interface PortAttrs {
    state: EditorState,
    box_idx: number,
    port_idx: number
}

/**
 * See the comment for BoxNode.
 */
export const PortNode: m.Component<PortAttrs> = {
    view({ attrs: { state, box_idx, port_idx } }) {
        const a = port_attach(box_idx, port_idx);
        const loc = state.ls.getLoc(a)!;
        const port = state.ls.sg.boxes.get(box_idx)!.ports.get(port_idx)!;
        const attrs = {
            fill: colorAttachment(state, a),
            stroke: port.color ?? CS.accent,
            "data-a": JSON.stringify(a),
            onmouseenter: state.cursor.handlemouseenterattachment,
            onmouseout: state.cursor.handlemouseoutattachment,
        };
        return m("circle", {
            r: PORTRADIUS,
            cx: loc[0],
            cy: loc[1],
            ...attrs
        });
    }

}
