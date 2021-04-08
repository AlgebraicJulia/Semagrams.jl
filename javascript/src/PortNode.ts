import m from "mithril";
import { colorAttachment, EditorState } from "./EditorState";
import { port_attach } from "./WireViz";

const PORTRADIUS = 7;

interface PortAttrs {
    state: EditorState,
    box_idx: number,
    port_idx: number
}

export const PortNode: m.Component<PortAttrs> = {
    view({ attrs: { state, box_idx, port_idx } }) {
        const a = port_attach(box_idx, port_idx);
        const loc = state.lw.getLoc(a)!;
        const port = state.lw.wireviz.boxes.get(box_idx)!.ports.get(port_idx)!;
        const attrs = {
            fill: colorAttachment(state, a),
            stroke: port.color ?? "black",
            "data-a": JSON.stringify(a),
            onmouseenter: state.handlemouseenterattachment,
            onmouseout: state.handlemouseoutattachment,
        };
        return m("circle", {
            r: PORTRADIUS,
            cx: loc[0],
            cy: loc[1],
            ...attrs
        });
    }

}
